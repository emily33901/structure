use std::mem::{offset_of, MaybeUninit};

use anyhow::Result;
use egui::{ahash::HashMap, RichText};
use windows::Win32::System::Threading::PEB_LDR_DATA;

use crate::{
    process::{OpenProcess, Section, SectionCategory},
    registry::Registry,
    rtti::Rtti,
    AddressResponse, State,
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
    Some(
        state
            .rtti
            .get(state.memory.read(address - 8), state.memory)?,
    )

    // TODO(emily): This is wrong, this is the type_info vtable ptr
    // if rtti.vtable != address {
    //     return false;
    // }
}

pub(crate) fn disect_bytes(
    state: &mut State,
    bytes: &[u8],
    ui: &mut egui::Ui,
) -> Option<AddressResponse> {
    let mut response = None;

    let value = *interpret_as::<usize>(bytes);

    ui.horizontal(|ui| {
        ui.add(egui::Label::new(RichText::new(&format!("{}", value))));
        ui.add(egui::Label::new(RichText::new(&format!("0x{:X}", value))));

        if value == 0 {
            return;
        }

        let Some(section) = section_for_address(state.sections, value) else {
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
                value
            )
        } else {
            format!("{:016X}", value)
        };

        let r = ui.add(egui::Label::new(RichText::new(&address_text)).sense(egui::Sense::click()));

        if r.clicked() {
            response = Some(AddressResponse::AddressStruct(
                Some(state.registry.find_or_register_address(value.into())),
                None,
            ));
        }

        if let Some(rtti) = rtti_if_address_is_vtable(state, value) {
            ui.label(rtti.names.join(" : "));
        }
    });

    response
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
            process: process,
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
}
