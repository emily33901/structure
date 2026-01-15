use std::{cell::RefCell, mem::MaybeUninit, rc::Rc};

use anyhow::Result;
use egui::{Color32, RichText, ahash::HashMap};

use crate::{
    AddressResponse, State,
    pe::{Section, SectionCategory},
    process::OpenProcess,
    rtti::Rtti,
};

pub(crate) fn interpret_as<T: Sized>(bytes: &[u8]) -> &T {
    if std::mem::size_of::<T>() < bytes.len() {
        panic!("not enough bytes to transmute as T");
    }

    unsafe { &*(bytes.as_ptr() as *const T) }
}

pub(crate) fn ascii_byte(byte: &u8) -> char {
    let c = *byte as char;

    if c.is_ascii() && !c.is_ascii_control() {
        c
    } else {
        '.'
    }
}

pub(crate) fn section_for_address(sections: &[Section], address: usize) -> Option<&Section> {
    let Ok(index) = sections.binary_search_by(|s| {
        if address > s.address && address < (s.address + s.len) {
            std::cmp::Ordering::Equal
        } else {
            s.address.cmp(&address)
        }
    }) else {
        return None;
    };

    Some(&sections[index])
}

pub(crate) fn rtti_if_address_is_vtable<'a>(
    state: &'a mut State<'_>,
    address: usize,
) -> Option<&'a Rtti> {
    let section = section_for_address(state.sections, address)?;

    if !matches!(section.category, SectionCategory::Data) {
        return None;
    }

    let vfunc_address = state.memory.read(address);

    // See if the first pointer points to code
    let func_section = section_for_address(state.sections, vfunc_address)?;

    if !matches!(func_section.category, SectionCategory::Code) {
        return None;
    }

    // Then see if the RTTI descriptor points back to the vtable
    state.rtti.get(state.memory.read(address - 8), state.memory)
}

pub(crate) fn disect_address(state: &RefCell<State>, address: usize, ui: &mut egui::Ui) {
    let Some(section) = section_for_address(state.borrow().sections, address) else {
        return;
    };

    ui.add(egui::Label::new(format!("-> <{}>", section.category.as_str(),)).selectable(false));

    let address_text = if let Some(module_path) = section.module_path.as_ref() {
        format!(
            "{}.{:016X}",
            std::path::Path::new(module_path)
                .file_name()
                .unwrap()
                .to_str()
                .unwrap(),
            address
        )
    } else {
        format!("{:016X}", address)
    };

    let r = ui.add(
        egui::Label::new(highlightable_address_text(state, address, address_text))
            .sense(egui::Sense::click()),
    );

    if r.hovered() {
        state.borrow_mut().this_frame_mut().highlighted_address = Some(address);
    }

    if r.clicked() {
        let address = state
            .borrow_mut()
            .registry
            .find_or_register_address(address.into());

        state
            .borrow_mut()
            .response(AddressResponse::AddressStruct(Some(address), None));
    }

    if let Some(rtti) = rtti_if_address_is_vtable(&mut state.borrow_mut(), address) {
        ui.label(rtti.names.join(" : "));
    } else {
        let address = state.borrow_mut().memory.read(address);
        if let Some(rtti) = rtti_if_address_is_vtable(&mut state.borrow_mut(), address) {
            ui.label(format!("-> {}", rtti.names.join(" : ")));
        }
    }
}

pub(crate) fn disect_bytes(state: &RefCell<State>, bytes: &[u8], ui: &mut egui::Ui) {
    let value = *interpret_as::<usize>(bytes);

    ui.with_layout(egui::Layout::left_to_right(egui::Align::Center), |ui| {
        ui.add(egui::Label::new(RichText::new(format!("{}", value))));
        ui.add(egui::Label::new(RichText::new(format!("0x{:X}", value))));

        if value == 0 {
            return;
        }

        disect_address(state, value, ui);
    });
}

pub(crate) fn highlightable_address_text(
    state: &RefCell<State>,
    address: usize,
    text: impl Into<String>,
) -> egui::RichText {
    let mut text = RichText::new(text);

    if let Some(highlighted_address) = state.borrow().last_frame.highlighted_address
        && highlighted_address == address
    {
        println!("address highlighted");
        text = text.background_color(Color32::DARK_RED);
    }

    text
}

const MEMORY_PAGE_LEN: usize = 4096;

pub(crate) enum Memory<'a> {
    Null,
    Process {
        process: &'a OpenProcess,
        pages: HashMap<usize, Vec<u8>>,
    },
}

impl<'a> Memory<'a> {
    pub(crate) fn new_process(process: &'a OpenProcess) -> Self {
        Self::Process {
            process,
            pages: Default::default(),
        }
    }

    pub(crate) fn new_null() -> Self {
        Self::Null
    }

    pub(crate) fn round_to_page(address: usize) -> usize {
        (address / MEMORY_PAGE_LEN) * MEMORY_PAGE_LEN
    }

    pub(crate) fn get(&mut self, address: usize, buffer: &mut [u8]) {
        match self {
            Memory::Null => {
                buffer.fill(0);
            }
            Memory::Process { process, pages } => {
                let page_start = Self::round_to_page(address);

                let dest_len = buffer.len();

                let page_count = (address.saturating_sub(page_start)) / MEMORY_PAGE_LEN + 1;

                let mut start = 0;
                for i in 0..page_count {
                    let page_address = page_start + i * MEMORY_PAGE_LEN;

                    let mut page = pages
                        .entry(page_address)
                        .or_insert_with(|| {
                            let mut buffer = vec![0; MEMORY_PAGE_LEN];
                            let _ = process.read_process_memory(page_address, &mut buffer);
                            buffer
                        })
                        .as_slice();

                    if i == 0 {
                        // Start however far we are supposed to in that page
                        page = &page[address - page_start..];
                    }

                    let end = page.len().min(dest_len);

                    buffer[start..end].copy_from_slice(&page[..end]);

                    start += end - start;
                }
            }
        }
    }

    pub(crate) fn read<T: Sized>(&mut self, address: usize) -> T {
        let mut value: MaybeUninit<T> = MaybeUninit::uninit();

        let slice = unsafe {
            std::slice::from_raw_parts_mut(
                value.as_mut_ptr() as *mut _ as *mut u8,
                std::mem::size_of::<T>(),
            )
        };

        self.get(address, slice);

        unsafe { value.assume_init() }
    }

    pub(crate) fn reader(&'a mut self, range: std::ops::Range<usize>) -> MemoryReader<'a> {
        MemoryReader {
            memory: self,
            range,
            offset: 0,
        }
    }

    pub(crate) fn pattern_scan(
        &'a mut self,
        range: std::ops::Range<usize>,
        pattern: &str,
        offset: usize,
    ) -> Result<Option<usize>> {
        let start = range.start;

        let result = patternscan::scan_first_match(self.reader(range), pattern)?
            .map(|addr| start + addr + offset);

        Ok(result)
    }

    pub(crate) fn process(&self) -> Option<&OpenProcess> {
        match self {
            Memory::Null => None,
            Memory::Process { process, .. } => Some(process),
        }
    }
}

pub struct MemoryReader<'a> {
    memory: &'a mut Memory<'a>,
    range: std::ops::Range<usize>,
    offset: usize,
}

impl std::io::Read for MemoryReader<'_> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let pos = self.range.start + self.offset;
        if pos > self.range.end {
            return Ok(0);
        }
        let read_len = buf.len().min(self.range.end - pos);
        self.memory.get(pos, buf);
        Ok(read_len)
    }
}

/// Rhai-compatible wrapper for Memory that can be used in scripts.
/// Uses interior mutability since Memory requires &mut self for reads
/// and Rhai passes by value/clone.
#[derive(Clone)]
pub struct RhaiMemory {
    inner: Rc<RefCell<*mut Memory<'static>>>,
}

impl RhaiMemory {
    pub fn new(memory: &mut Memory) -> Self {
        Self {
            inner: Rc::new(RefCell::new(memory as *mut _ as *mut Memory<'static>)),
        }
    }

    pub fn read_u8(&mut self, address: i64) -> i64 {
        unsafe { (*(*self.inner.borrow_mut())).read::<u8>(address as usize) as i64 }
    }

    pub fn read_u16(&mut self, address: i64) -> i64 {
        unsafe { (*(*self.inner.borrow_mut())).read::<u16>(address as usize) as i64 }
    }

    pub fn read_u32(&mut self, address: i64) -> i64 {
        unsafe { (*(*self.inner.borrow_mut())).read::<u32>(address as usize) as i64 }
    }

    pub fn read_u64(&mut self, address: i64) -> i64 {
        unsafe { (*(*self.inner.borrow_mut())).read::<u64>(address as usize) as i64 }
    }

    pub fn read_string(&mut self, address: i64, len: i64) -> String {
        let len = len as usize;
        let mut buffer = vec![0_u8; len];
        unsafe { (*(*self.inner.borrow_mut())).get(address as usize, &mut buffer) }
        String::from_utf8_lossy(&buffer).to_string()
    }
}
