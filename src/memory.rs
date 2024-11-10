use std::collections::hash_map::Entry;

use egui::{ahash::HashMap, RichText};

use crate::{
    process::{Process, Section},
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

pub(crate) fn disect_bytes(
    sections: Option<&[Section]>,
    registry: &mut Registry,
    bytes: &[u8],
    ui: &mut egui::Ui,
) -> Option<AddressResponse> {
    let mut response = None;

    let value = *interpret_as::<usize>(bytes);

    ui.horizontal(|ui| {
        ui.add(egui::Label::new(RichText::new(&format!("{}", value))));
        ui.add(egui::Label::new(RichText::new(&format!("0x{:X}", value))));

        if value != 0 {
            if let Some(sections) = sections {
                if let Ok(index) = sections.binary_search_by(|s| {
                    if value > s.address && value < (s.address + s.len) {
                        std::cmp::Ordering::Equal
                    } else {
                        s.address.cmp(&value)
                    }
                }) {
                    let section = &sections[index];
                    ui.add(
                        egui::Label::new(RichText::new(format!(
                            "-> <{}>",
                            section.category.as_str(),
                        )))
                        .selectable(false),
                    );

                    if ui
                        .add(
                            egui::Label::new(RichText::new(&format!("{:016X}", value)))
                                .sense(egui::Sense::click()),
                        )
                        .clicked()
                    {
                        response = Some(AddressResponse::AddressStruct(
                            Some(registry.find_or_register_address(value.into())),
                            None,
                        ));
                    }
                }
            }
        }
    });

    response
}

const MEMORY_PAGE_LEN: usize = 4096;

pub(crate) struct Memory<'a> {
    process: &'a Process,
    pages: HashMap<usize, Vec<u8>>,
}

impl<'a> Memory<'a> {
    pub(crate) fn new(process: &'a Process) -> Self {
        Self {
            process: process,
            pages: Default::default(),
        }
    }

    fn round_to_page(address: usize) -> usize {
        (address / MEMORY_PAGE_LEN) * MEMORY_PAGE_LEN
    }

    pub(crate) fn get(&mut self, address: usize, buffer: &mut [u8]) {
        let page_start = Self::round_to_page(address);

        let dest_len = buffer.len();

        // Do we have this page
        let pages = (address.saturating_sub(page_start)) / MEMORY_PAGE_LEN + 1;

        let mut start = 0;
        for i in 0..pages {
            let page_address = page_start + i * MEMORY_PAGE_LEN;

            let mut page = self
                .pages
                .entry(page_address)
                .or_insert_with(|| {
                    let mut buffer = vec![0; MEMORY_PAGE_LEN];
                    let _ = self.process.read_process_memory(page_address, &mut buffer);
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
