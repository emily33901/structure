use std::{
    ffi::c_void,
    mem::{offset_of, MaybeUninit},
};

use windows::{
    Wdk::System::Threading::{NtQueryInformationProcess, ProcessBasicInformation},
    Win32::{
        Foundation::{CloseHandle, HANDLE, STATUS_SUCCESS},
        System::{
            Diagnostics::Debug::ReadProcessMemory,
            Kernel::LIST_ENTRY,
            Memory::{
                VirtualQueryEx, MEMORY_BASIC_INFORMATION, MEM_COMMIT, MEM_IMAGE, MEM_MAPPED,
                MEM_PRIVATE, PAGE_EXECUTE, PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE,
                PAGE_READONLY, PAGE_READWRITE, PAGE_WRITECOPY,
            },
            Threading::{
                OpenProcess, PEB, PEB_LDR_DATA, PROCESS_ALL_ACCESS, PROCESS_BASIC_INFORMATION,
            },
            WindowsProgramming::LDR_DATA_TABLE_ENTRY,
        },
    },
};

use anyhow::{anyhow, Result};

pub(crate) struct Module {
    address: usize,
    size: usize,
    path: String,
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
}

impl SectionCategory {
    pub(crate) fn as_str(&self) -> &'static str {
        match self {
            SectionCategory::Unknown => "Unknown",
            SectionCategory::Heap => "Heap",
            SectionCategory::Private => "Private",
        }
    }
}

pub(crate) struct Section {
    pub(crate) address: usize,
    pub(crate) len: usize,

    pub(crate) category: SectionCategory,
    typ: SectionType,

    read: bool,
    write: bool,
    execute: bool,
    copy_on_write: bool,
    guard: bool,
}

pub(crate) struct Process(HANDLE);

impl Process {
    pub(crate) fn new(pid: u32) -> Result<Self> {
        unsafe {
            let handle = OpenProcess(PROCESS_ALL_ACCESS, false, pid as u32)?;

            eprintln!("process handle is {handle:?}");

            Ok(Self(handle))
        }
    }

    // TODO(emily): We want to cache this in some way
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

    fn remote_peb(&self) -> Result<*const PEB_LDR_DATA> {
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

        Ok(self.read_from_process(peb_address + std::mem::offset_of!(PEB, Ldr))?)
    }

    pub(crate) fn modules(&self) -> Result<Vec<Module>> {
        let remote_peb = self.remote_peb()?;

        let head = unsafe {
            remote_peb.byte_add(offset_of!(PEB_LDR_DATA, InMemoryOrderModuleList))
                as *const LIST_ENTRY
        };
        let mut current: *const LIST_ENTRY =
            self.read_from_process(&(unsafe { *head }).Flink as *const _ as usize)?;

        let mut modules = vec![];

        while current != head {
            let entry: LDR_DATA_TABLE_ENTRY = self.read_from_process(unsafe {
                current.byte_sub(offset_of!(LDR_DATA_TABLE_ENTRY, InMemoryOrderLinks)) as usize
            })?;

            let module = Module {
                address: entry.DllBase as usize,
                size: unsafe { *(&entry.Reserved3[1] as *const _ as *const u32) } as usize,
                path: {
                    unsafe {
                        String::from_utf16_lossy(std::slice::from_raw_parts(
                            entry.FullDllName.Buffer.0,
                            entry.FullDllName.Length as usize,
                        ))
                    }
                },
            };

            modules.push(module);

            current = entry.InMemoryOrderLinks.Flink;
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
                };

                sections.push(section);
            }

            address += memory.RegionSize;
        }

        Ok(sections)
    }
}
impl Drop for Process {
    fn drop(&mut self) {
        unsafe {
            let _ = CloseHandle(self.0);
        };
    }
}
