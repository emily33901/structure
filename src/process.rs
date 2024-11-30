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

pub(crate) struct OpenProcess(HANDLE);

impl OpenProcess {
    pub(crate) fn new(pid: u32) -> Result<Self> {
        unsafe {
            let handle = Threading::OpenProcess(PROCESS_ALL_ACCESS, false, pid as u32)?;

            eprintln!("process handle is {handle:?}");

            Ok(Self(handle))
        }
    }

    pub(crate) fn handle(&self) -> HANDLE {
        self.0
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

    pub(crate) fn remote_peb(&self) -> Result<(usize, PEB_LDR_DATA)> {
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
