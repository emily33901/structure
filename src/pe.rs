use std::{ffi::c_void, mem::offset_of};

use anyhow::Result;
use windows::Win32::System::{
    Diagnostics::Debug::{
        IMAGE_NT_HEADERS64, IMAGE_SCN_CNT_CODE, IMAGE_SCN_CNT_INITIALIZED_DATA,
        IMAGE_SCN_CNT_UNINITIALIZED_DATA, IMAGE_SECTION_HEADER,
    },
    Kernel::LIST_ENTRY,
    Memory::{
        VirtualQueryEx, MEMORY_BASIC_INFORMATION, MEM_COMMIT, MEM_IMAGE, MEM_MAPPED, MEM_PRIVATE,
        PAGE_EXECUTE, PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE, PAGE_READONLY, PAGE_READWRITE,
        PAGE_WRITECOPY,
    },
    SystemServices::IMAGE_DOS_HEADER,
    Threading::PEB_LDR_DATA,
    WindowsProgramming::LDR_DATA_TABLE_ENTRY,
};

use crate::memory::Memory;

pub(crate) struct Module {
    address: usize,
    size: usize,
    full_path: String,
    code_range: std::ops::Range<usize>,
}

#[derive(PartialEq)]
pub(crate) enum SectionType {
    Image,
    Mapped,
    Private,
}

pub(crate) enum SectionCategory {
    Unknown,
    Heap,
    Private,
    Code,
    Data,
}

impl SectionCategory {
    pub(crate) fn as_str(&self) -> &'static str {
        match self {
            SectionCategory::Unknown => "Unknown",
            SectionCategory::Heap => "Heap",
            SectionCategory::Private => "Private",
            SectionCategory::Code => "Code",
            SectionCategory::Data => "Data",
        }
    }
}

pub(crate) struct Section {
    pub(crate) address: usize,
    pub(crate) len: usize,

    pub(crate) category: SectionCategory,
    pub(crate) name: Option<String>,
    // TODO(emily): I feel like this could just be an Rc<Module>, would need to figure out
    // where to keep those around though.
    pub(crate) module_path: Option<String>,
    typ: SectionType,

    read: bool,
    write: bool,
    execute: bool,
    copy_on_write: bool,
    guard: bool,
}

pub(crate) fn modules(memory: &mut Memory<'_>) -> Result<Vec<Module>> {
    let Some(process) = memory.process() else {
        return Ok(vec![]);
    };

    let (remote_peb_address, remote_peb) = process.remote_peb_ldr()?;

    let head = remote_peb.InMemoryOrderModuleList;
    let head_address: usize = head.Blink as usize;
    let mut current_address = head.Flink as usize;

    let mut modules = vec![];

    while current_address != head_address {
        let current: LIST_ENTRY = memory.read(current_address);
        let entry: LDR_DATA_TABLE_ENTRY =
            memory.read(current_address - offset_of!(LDR_DATA_TABLE_ENTRY, InMemoryOrderLinks));

        // TODO(emily): Hacky hack
        if entry.InMemoryOrderLinks.Flink as usize == current_address {
            break;
        }

        let address = entry.DllBase as usize;

        let (_, nt_header) = module_headers(memory, address);
        let code_range = code_range(address, nt_header);

        let module = Module {
            address,
            size: unsafe { *(&entry.Reserved3[1] as *const _ as *const u32) } as usize,
            full_path: {
                let len = entry.FullDllName.Length as usize / 2;
                let mut buffer = vec![0_u16; len];
                unsafe {
                    let mut buffer_bytes = std::slice::from_raw_parts_mut(
                        buffer.as_mut_ptr() as *mut _,
                        entry.FullDllName.Length as usize,
                    );
                    memory.get(entry.FullDllName.Buffer.0 as usize, &mut buffer_bytes);
                    String::from_utf16_lossy(std::slice::from_raw_parts(buffer.as_ptr(), len))
                }
            },
            code_range,
        };

        modules.push(module);

        current_address = entry.InMemoryOrderLinks.Flink as usize;
    }

    Ok(modules)
}

// TODO(emily): Go move this this isnt a pe section?
pub(crate) fn sections(memory: &mut Memory<'_>) -> Result<Vec<Section>> {
    let Some(process) = memory.process() else {
        return Ok(vec![]);
    };

    let mut memory_information = MEMORY_BASIC_INFORMATION::default();
    memory_information.RegionSize = 0x1000;

    let mut address = 0_usize;

    let mut sections = vec![];

    while unsafe {
        VirtualQueryEx(
            process.handle(),
            Some(address as *const usize as *const c_void),
            &mut memory_information,
            std::mem::size_of::<MEMORY_BASIC_INFORMATION>(),
        ) != 0
            && address + memory_information.RegionSize > address
    } {
        if memory_information.State == MEM_COMMIT {
            let section_type = if memory_information.Type == MEM_IMAGE {
                SectionType::Image
            } else if memory_information.Type == MEM_MAPPED {
                SectionType::Mapped
            } else if memory_information.Type == MEM_PRIVATE {
                SectionType::Private
            } else {
                unreachable!()
            };

            let section = Section {
                address,
                len: memory_information.RegionSize,
                category: if section_type == SectionType::Private {
                    SectionCategory::Heap
                } else {
                    SectionCategory::Unknown
                },
                typ: section_type,
                read: memory_information.Protect.contains(PAGE_READONLY),
                write: memory_information.Protect.contains(PAGE_READWRITE),
                execute: memory_information.Protect.contains(PAGE_EXECUTE)
                    || memory_information.Protect.contains(PAGE_EXECUTE_READ)
                    || memory_information.Protect.contains(PAGE_EXECUTE_READWRITE),
                copy_on_write: memory_information.Protect.contains(PAGE_WRITECOPY),
                guard: memory_information.Protect.contains(PAGE_EXECUTE),
                name: None,
                module_path: None,
            };

            sections.push(section);
        }

        address += memory_information.RegionSize;
    }

    Ok(sections)
}

pub(crate) fn module_sections(
    memory: &mut Memory<'_>,
    modules: &[Module],
    sections: &mut [Section],
) -> Result<()> {
    for module in modules {
        let (dos_header, nt_header) = module_headers(memory, module.address);

        let mut section_headers =
            vec![IMAGE_SECTION_HEADER::default(); nt_header.FileHeader.NumberOfSections as usize];

        memory.get(
            module.address
                + dos_header.e_lfanew as usize
                + std::mem::size_of::<IMAGE_NT_HEADERS64>(),
            unsafe {
                std::slice::from_raw_parts_mut(
                    section_headers.as_mut_ptr() as *mut _ as *mut _,
                    section_headers.len() * std::mem::size_of::<IMAGE_SECTION_HEADER>(),
                )
            },
        );

        let read_section_headers = section_headers.len();

        for header in &section_headers[..read_section_headers] {
            let module_section_address = module.address + header.VirtualAddress as usize;
            let section_virtual_size = unsafe { header.Misc.VirtualSize } as usize;

            for section in sections
                .iter_mut()
                .skip_while(|section| section.address < module.address)
            {
                if !(module_section_address >= section.address
                    && module_section_address < (section.address + section.len)
                    && header.VirtualAddress as usize + section_virtual_size <= module.size)
                {
                    continue;
                }

                if header.Characteristics.contains(IMAGE_SCN_CNT_CODE) {
                    section.category = SectionCategory::Code;
                } else if header
                    .Characteristics
                    .contains(IMAGE_SCN_CNT_INITIALIZED_DATA)
                    || header
                        .Characteristics
                        .contains(IMAGE_SCN_CNT_UNINITIALIZED_DATA)
                {
                    section.category = SectionCategory::Data
                }

                section.name = Some(String::from_utf8_lossy(&header.Name).to_string());
                section.module_path = Some(module.full_path.clone());
            }
        }
    }

    Ok(())
}

fn module_headers(
    memory: &mut Memory<'_>,
    base_address: usize,
) -> (IMAGE_DOS_HEADER, IMAGE_NT_HEADERS64) {
    let dos_header: IMAGE_DOS_HEADER = memory.read(base_address);
    let nt_header: IMAGE_NT_HEADERS64 = memory.read(base_address + dos_header.e_lfanew as usize);
    (dos_header, nt_header)
}

fn code_range(address: usize, nt_header: IMAGE_NT_HEADERS64) -> std::ops::Range<usize> {
    (address + nt_header.OptionalHeader.BaseOfCode as usize)
        ..(address
            + nt_header.OptionalHeader.BaseOfCode as usize
            + nt_header.OptionalHeader.SizeOfCode as usize)
}
