use std::mem::{offset_of, MaybeUninit};

use anyhow::Result;
use egui::{ahash::HashMap, RichText};
use windows::Win32::System::Threading::PEB_LDR_DATA;

use crate::{
    process::{OpenProcess, Section, SectionCategory},
    registry::Registry,
    AddressResponse,
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

pub(crate) fn is_vtable(address: usize, memory: &mut Memory<'_>, sections: &[Section]) -> bool {
    let Some(section) = section_for_address(sections, address) else {
        return false;
    };

    if !matches!(section.category, SectionCategory::Data) {
        return false;
    }

    let vfunc_address = memory.read(address);

    // See if the first pointer points to code
    let Some(func_section) = section_for_address(sections, vfunc_address) else {
        return false;
    };

    if !matches!(func_section.category, SectionCategory::Code) {
        return false;
    }

    // Then see if the RTTI descriptor points back to the vtable
    let Some(rtti) = rtti_type_descriptor(memory.read(address - 8), memory) else {
        return false;
    };

    // TODO(emily): This is wrong, this is the type_info vtable ptr
    // if rtti.vtable != address {
    //     return false;
    // }

    true
}

pub(crate) fn disect_bytes(
    sections: Option<&[Section]>,
    registry: &mut Registry,
    memory: &mut Memory<'_>,
    bytes: &[u8],
    ui: &mut egui::Ui,
) -> Option<AddressResponse> {
    let mut response = None;

    let value = *interpret_as::<usize>(bytes);

    ui.horizontal(|ui| {
        ui.add(egui::Label::new(RichText::new(&format!("{}", value))));
        ui.add(egui::Label::new(RichText::new(&format!("0x{:X}", value))));

        if value != 0 {
            let Some(sections) = sections else { return };
            let Some(section) = section_for_address(sections, value) else {
                return;
            };

            ui.add(
                egui::Label::new(format!("-> <{}>", section.category.as_str(),)).selectable(false),
            );

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

            let r =
                ui.add(egui::Label::new(RichText::new(&address_text)).sense(egui::Sense::click()));

            if r.clicked() {
                response = Some(AddressResponse::AddressStruct(
                    Some(registry.find_or_register_address(value.into())),
                    None,
                ));
            }

            if is_vtable(value, memory, sections) {
                ui.label(format!("vtable"));
            }
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

    fn read<T: Sized>(&mut self, address: usize) -> T {
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

#[repr(C)]
struct RTTICompleteObjectLocator {
    signature: u32,
    offset: u32,
    constructor_displacement_offset: u32,
    type_descriptor_offset: u32,
    class_descriptor_offset: u32,
    self_offset: u32,
}

#[repr(C)]
struct RTTITypeDescriptor {
    vtable: usize,
    spare: usize,
    name_bytes: i8,
}

#[repr(C)]
struct RTTIClassHierarchyDescriptor {
    signature: u32,
    attributes: u32,
    base_class_count: u32,
    array_offset: u32,
}

#[repr(C)]
struct RTTIObjectLocation {
    mdisp: u32,
    pdisp: u32,
    vdisp: u32,
}

#[repr(C)]
struct RTTIBaseClassDescriptor {
    type_descriptor_offset: u32,
    contained_bases: u32,
    object_location: RTTIObjectLocation,
    attributes: u32,
    class_descriptor_offset: u32,
}

pub(crate) struct Rtti {
    vtable_address: usize,
    names: Vec<String>,
}

pub(crate) fn rtti_type_descriptor(
    address: usize,
    memory: &mut Memory<'_>,
) -> Option<RTTITypeDescriptor> {
    let complete_object_locator: RTTICompleteObjectLocator = memory.read(address);

    if complete_object_locator.self_offset == 0
        || complete_object_locator.class_descriptor_offset == 0
    {
        return None;
    }

    let base_address = address - complete_object_locator.self_offset as usize;

    let type_descriptor_address =
        base_address + complete_object_locator.type_descriptor_offset as usize;

    let type_descriptor: RTTITypeDescriptor = memory.read(type_descriptor_address);

    Some(type_descriptor)
}

pub(crate) fn rtti(address: usize, memory: &mut Memory<'_>) -> Option<Rtti> {
    let complete_object_locator: RTTICompleteObjectLocator = memory.read(address);

    if complete_object_locator.self_offset == 0
        || complete_object_locator.class_descriptor_offset == 0
    {
        return None;
    }

    let base_address = address - complete_object_locator.self_offset as usize;

    let type_descriptor_address =
        base_address + complete_object_locator.type_descriptor_offset as usize;

    let type_descriptor: RTTITypeDescriptor = memory.read(type_descriptor_address);

    let class_hierarchy_descriptor_address =
        base_address + complete_object_locator.class_descriptor_offset as usize;

    let class_hierarchy_descriptor: RTTIClassHierarchyDescriptor =
        memory.read(class_hierarchy_descriptor_address);

    let base_class_array_address = base_address + class_hierarchy_descriptor.array_offset as usize;

    let mut base_class_offsets = vec![0_u32; class_hierarchy_descriptor.base_class_count as usize];

    let mut base_class_offsets_bytes = unsafe {
        std::slice::from_raw_parts_mut(
            base_class_offsets.as_mut_ptr() as *mut u8,
            class_hierarchy_descriptor.base_class_count as usize * std::mem::size_of::<u32>(),
        )
    };

    memory.get(base_class_array_address, &mut base_class_offsets_bytes);

    let mut rtti = Rtti {
        vtable_address: type_descriptor.vtable,
        names: vec![],
    };

    for base_class_offset in base_class_offsets {
        let base_class_descriptor: RTTIBaseClassDescriptor =
            memory.read(base_address + base_class_offset as usize);

        // Read some memory that contains the name in it

        let mut buffer = [0_u8; 1000];

        let type_descriptor_address =
            base_address + base_class_descriptor.type_descriptor_offset as usize;
        memory.get(
            type_descriptor_address + offset_of!(RTTITypeDescriptor, name_bytes),
            &mut buffer,
        );

        rtti.names.push(
            unsafe { std::ffi::CStr::from_ptr(buffer.as_ptr() as *const _) }
                .to_string_lossy()
                .to_string(),
        );
    }

    Some(rtti)
}
