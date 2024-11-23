use std::{
    ffi::c_void,
    mem::{offset_of, MaybeUninit},
};

use windows::{
    Wdk::System::Threading::{NtQueryInformationProcess, ProcessBasicInformation},
    Win32::{
        Foundation::{CloseHandle, HANDLE, STATUS_SUCCESS},
        System::{
            Diagnostics::{
                Debug::{
                    ReadProcessMemory, IMAGE_NT_HEADERS64, IMAGE_SCN_CNT_CODE,
                    IMAGE_SCN_CNT_INITIALIZED_DATA, IMAGE_SCN_CNT_UNINITIALIZED_DATA,
                    IMAGE_SECTION_HEADER,
                },
                ToolHelp::{Process32FirstW, Process32NextW, PROCESSENTRY32W, TH32CS_SNAPPROCESS},
            },
            Kernel::LIST_ENTRY,
            Memory::{
                VirtualQueryEx, MEMORY_BASIC_INFORMATION, MEM_COMMIT, MEM_IMAGE, MEM_MAPPED,
                MEM_PRIVATE, PAGE_EXECUTE, PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE,
                PAGE_READONLY, PAGE_READWRITE, PAGE_WRITECOPY,
            },
            SystemServices::IMAGE_DOS_HEADER,
            Threading::{self, PEB, PEB_LDR_DATA, PROCESS_ALL_ACCESS, PROCESS_BASIC_INFORMATION},
            WindowsProgramming::LDR_DATA_TABLE_ENTRY,
        },
    },
};

use anyhow::{anyhow, Result};

pub(crate) struct Module {
    address: usize,
    size: usize,
    full_path: String,
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

pub(crate) struct OpenProcess(HANDLE);

impl OpenProcess {
    pub(crate) fn new(pid: u32) -> Result<Self> {
        unsafe {
            let handle = Threading::OpenProcess(PROCESS_ALL_ACCESS, false, pid as u32)?;

            eprintln!("process handle is {handle:?}");

            Ok(Self(handle))
        }
    }

    pub(crate) fn read_process_memory(&self, address: usize, buf: &mut [u8]) -> Result<usize> {
        unsafe {
            let buf_ptr = buf.as_mut_ptr();
            let mut read_bytes = 0_usize;

            ReadProcessMemory(
                self.0,
                address as *const c_void,
                buf_ptr as *mut c_void,
                buf.len(),
                Some(&mut read_bytes),
            )?;

            Ok(read_bytes)
        }
    }

    fn read_from_process<T: Sized>(&self, address: usize) -> Result<T> {
        let mut value: MaybeUninit<T> = MaybeUninit::uninit();

        let slice = unsafe {
            std::slice::from_raw_parts_mut(
                value.as_mut_ptr() as *mut _ as *mut u8,
                std::mem::size_of::<T>(),
            )
        };

        let read = self.read_process_memory(address, slice)?;

        if read != std::mem::size_of::<T>() {
            Err(anyhow!("Unable to read full value"))
        } else {
            Ok(unsafe { value.assume_init() })
        }
    }

    fn remote_peb(&self) -> Result<(usize, PEB_LDR_DATA)> {
        let mut process_information = PROCESS_BASIC_INFORMATION::default();
        let mut out_len = 0_u32;
        unsafe {
            let e = NtQueryInformationProcess(
                self.0,
                ProcessBasicInformation,
                &mut process_information as *mut _ as *mut c_void,
                std::mem::size_of::<PROCESS_BASIC_INFORMATION>() as u32,
                &mut out_len,
            );
            if e != STATUS_SUCCESS {
                return Err(anyhow!("unable to get remote peb {:?}", e));
            }
        }

        let peb_address = process_information.PebBaseAddress as usize;

        let ldr_address: usize =
            self.read_from_process(peb_address + std::mem::offset_of!(PEB, Ldr))?;

        Ok((ldr_address, self.read_from_process(ldr_address)?))
    }

    pub(crate) fn modules(&self) -> Result<Vec<Module>> {
        let (remote_peb_address, remote_peb) = self.remote_peb()?;

        let head = remote_peb.InMemoryOrderModuleList;
        let head_address = remote_peb_address + offset_of!(PEB_LDR_DATA, InMemoryOrderModuleList);
        let mut current_address = head.Flink as usize;

        let mut modules = vec![];

        while current_address != head_address {
            let current: LIST_ENTRY = self.read_from_process(current_address)?;
            let entry: LDR_DATA_TABLE_ENTRY = self.read_from_process(unsafe {
                current_address - offset_of!(LDR_DATA_TABLE_ENTRY, InMemoryOrderLinks)
            })?;

            let module = Module {
                address: entry.DllBase as usize,
                size: unsafe { *(&entry.Reserved3[1] as *const _ as *const u32) } as usize,
                full_path: {
                    let len = entry.FullDllName.Length as usize / 2;
                    let mut buffer = vec![0_u16; len];
                    unsafe {
                        let mut buffer_bytes = std::slice::from_raw_parts_mut(
                            buffer.as_mut_ptr() as *mut _,
                            entry.FullDllName.Length as usize,
                        );
                        self.read_process_memory(
                            entry.FullDllName.Buffer.0 as usize,
                            &mut buffer_bytes,
                        )?;
                        String::from_utf16_lossy(std::slice::from_raw_parts(buffer.as_ptr(), len))
                    }
                },
            };

            modules.push(module);

            current_address = entry.InMemoryOrderLinks.Flink as usize;
        }

        Ok(modules)
    }

    pub(crate) fn sections(&self) -> Result<Vec<Section>> {
        let mut memory = MEMORY_BASIC_INFORMATION::default();
        memory.RegionSize = 0x1000;

        let mut address = 0_usize;

        let mut sections = vec![];

        while unsafe {
            VirtualQueryEx(
                self.0,
                Some(address as *const usize as *const c_void),
                &mut memory,
                std::mem::size_of::<MEMORY_BASIC_INFORMATION>(),
            ) != 0
                && address + memory.RegionSize > address
        } {
            if memory.State == MEM_COMMIT {
                let section_type = if memory.Type == MEM_IMAGE {
                    SectionType::Image
                } else if memory.Type == MEM_MAPPED {
                    SectionType::Mapped
                } else if memory.Type == MEM_PRIVATE {
                    SectionType::Private
                } else {
                    unreachable!()
                };

                let section = Section {
                    address,
                    len: memory.RegionSize,
                    category: if section_type == SectionType::Private {
                        SectionCategory::Heap
                    } else {
                        SectionCategory::Unknown
                    },
                    typ: section_type,
                    read: memory.Protect.contains(PAGE_READONLY),
                    write: memory.Protect.contains(PAGE_READWRITE),
                    execute: memory.Protect.contains(PAGE_EXECUTE)
                        || memory.Protect.contains(PAGE_EXECUTE_READ)
                        || memory.Protect.contains(PAGE_EXECUTE_READWRITE),
                    copy_on_write: memory.Protect.contains(PAGE_WRITECOPY),
                    guard: memory.Protect.contains(PAGE_EXECUTE),
                    name: None,
                    module_path: None,
                };

                sections.push(section);
            }

            address += memory.RegionSize;
        }

        Ok(sections)
    }

    pub(crate) fn module_sections(
        &self,
        modules: &[Module],
        sections: &mut [Section],
    ) -> Result<()> {
        for module in modules {
            let dos_header: IMAGE_DOS_HEADER = self.read_from_process(module.address)?;
            let nt_header: IMAGE_NT_HEADERS64 =
                self.read_from_process(module.address + dos_header.e_lfanew as usize)?;

            let mut section_headers = vec![
                IMAGE_SECTION_HEADER::default();
                nt_header.FileHeader.NumberOfSections as usize
            ];

            let read_len_bytes = self.read_process_memory(
                module.address
                    + dos_header.e_lfanew as usize
                    + std::mem::size_of::<IMAGE_NT_HEADERS64>(),
                unsafe {
                    std::slice::from_raw_parts_mut(
                        section_headers.as_mut_ptr() as *mut _ as *mut _,
                        section_headers.len() * std::mem::size_of::<IMAGE_SECTION_HEADER>(),
                    )
                },
            )?;

            let read_section_headers = read_len_bytes / std::mem::size_of::<IMAGE_SECTION_HEADER>();

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
}
impl Drop for OpenProcess {
    fn drop(&mut self) {
        unsafe {
            let _ = CloseHandle(self.0);
        };
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Process {
    pub(crate) pid: u32,
    pub(crate) name: String,
}

pub(crate) fn processes() -> Result<Vec<Process>> {
    let snapshot = unsafe {
        windows::Win32::System::Diagnostics::ToolHelp::CreateToolhelp32Snapshot(
            TH32CS_SNAPPROCESS,
            0,
        )
    }?;

    let mut pe = PROCESSENTRY32W::default();
    pe.dwSize = std::mem::size_of::<PROCESSENTRY32W>() as u32;

    let mut processes = vec![];

    let mut okay = unsafe { Process32FirstW(snapshot, &mut pe) };
    while okay.is_ok() {
        processes.push(Process {
            name: String::from_utf16_lossy(&pe.szExeFile),
            pid: pe.th32ProcessID,
        });

        okay = unsafe { Process32NextW(snapshot, &mut pe) };
    }

    Ok(processes)
}
