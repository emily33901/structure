use core::f32;
use std::{cell::RefCell, rc::Rc};

use egui::{ahash::HashMap, vec2, RichText};
use rand::{distributions::Alphanumeric, Rng};

use crate::{
    memory::{self, Memory},
    process::{OpenProcess, Section},
    registry::{Registry, RegistryId},
    Address, AddressResponse,
};

// TODO(emily): Need some way to collapse the view of Struct or Pointer
// either do this here or in Struct::nodes
#[derive(Debug)]
enum Node {
    U8,
    U16,
    U32,
    U64,
    Struct(Rc<RefCell<Struct>>),
    Pointer(Rc<RefCell<Struct>>),
}

impl Node {
    fn row_count(&self) -> usize {
        match self {
            Node::U64 | Node::U32 | Node::U16 | Node::U8 => 1,
            Node::Pointer(s) | Node::Struct(s) => s.borrow().row_count(),
        }
    }

    fn byte_size(&self) -> usize {
        match self {
            Node::U64 => 8,
            Node::U32 => 4,
            Node::U16 => 2,
            Node::U8 => 1,
            Node::Pointer(_) => 8,
            Node::Struct(s) => s.borrow().byte_size(),
        }
    }

    fn height(&self, ui: &egui::Ui) -> f32 {
        let item_spacing_y = ui.style().spacing.item_spacing.y;
        match self {
            Node::U8 | Node::U16 | Node::U32 | Node::U64 => NODE_UNIT_SIZE + 2.0 * item_spacing_y,
            Node::Struct(s) | Node::Pointer(s) => s.borrow().row_heights(ui).sum(),
        }
    }

    fn node_ui_inner(
        &mut self,
        ui: &mut egui::Ui,
        registry: &mut Registry,
        memory: &mut Memory<'_>,
        address: usize,
        offset_in_parent: usize,
        sections: Option<&[Section]>,
    ) -> (usize, Option<AddressResponse>) {
        let (bytes, response) = match self {
            Node::U64 => {
                ui.label("U64");

                Node::none_ui(
                    ui,
                    registry,
                    memory,
                    address,
                    offset_in_parent,
                    Some(8),
                    sections,
                )
            }
            Node::U32 => {
                ui.label("U32");

                Node::none_ui(
                    ui,
                    registry,
                    memory,
                    address,
                    offset_in_parent,
                    Some(4),
                    sections,
                )
            }
            Node::U16 => {
                ui.label("U16");

                Node::none_ui(
                    ui,
                    registry,
                    memory,
                    address,
                    offset_in_parent,
                    Some(2),
                    sections,
                )
            }
            Node::U8 => {
                ui.label("U8");

                Node::none_ui(
                    ui,
                    registry,
                    memory,
                    address,
                    offset_in_parent,
                    Some(1),
                    sections,
                )
            }
            // TODO(emily): The layout between struct and pointer is very similar, probably identitcal.
            // Don't just copy paste it.
            Node::Struct(s) => {
                ui.label("S");

                s.clone().borrow().ui(
                    s.clone(),
                    StructUiFlags::default(),
                    ui,
                    registry,
                    memory,
                    address,
                    sections,
                )
            }
            Node::Pointer(s) => {
                ui.label("P");

                let mut buffer = [0; 8];
                memory.get(address, &mut buffer);
                let address = memory::interpret_as(&buffer);

                s.borrow().ui(
                    s.clone(),
                    StructUiFlags::default(),
                    ui,
                    registry,
                    memory,
                    *address,
                    sections,
                )
            }
        };

        // Handle replacing Struct with a different Struct
        let response = match response {
            Some(AddressResponse::Replace(new_s)) => {
                let (Node::Struct(s) | Node::Pointer(s)) = self else {
                    panic!("AddressResponse::Replace should only come from a Node::Struct or a Node::Pointer");
                };
                *s = new_s;
                None
            }
            r => r,
        };

        (bytes, response)
    }

    // NOTE(emily): address and offset are independent.
    // address is absolute and offset is where this is in a parent.
    fn ui(
        &mut self,
        ui: &mut egui::Ui,
        registry: &mut Registry,
        memory: &mut Memory<'_>,
        address: usize,
        offset_in_parent: usize,
        sections: Option<&[Section]>,
    ) -> (usize, Option<AddressResponse>) {
        let height = self.height(ui);

        let (bytes, response) = ui
            .allocate_ui_with_layout(
                vec2(ui.available_width(), height),
                egui::Layout::left_to_right(egui::Align::Min),
                |ui| {
                    ui.horizontal(|ui| {
                        self.node_ui_inner(
                            ui,
                            registry,
                            memory,
                            address,
                            offset_in_parent,
                            sections,
                        )
                    })
                    .inner
                },
            )
            .inner;

        (bytes, response)
    }

    fn context_menu(
        &self,
        registry: &mut Registry,
        memory: &mut Memory<'_>,
        address: usize,
        ui: &mut egui::Ui,
    ) -> Option<AddressResponse> {
        let mut response = None;
        match self {
            Node::Struct(s) => {
                if ui.button("Open struct in new tab").clicked() {
                    response = Some(AddressResponse::AddressStruct(
                        Some(registry.find_or_register_address(address.into())),
                        Some(s.clone()),
                    ))
                }
            }
            Node::Pointer(s) => {
                let mut buffer = [0; 8];
                memory.get(address, &mut buffer);
                let address: &usize = memory::interpret_as(&buffer);

                if ui.button("Open struct in new tab").clicked() {
                    response = Some(AddressResponse::AddressStruct(
                        Some(registry.find_or_register_address((*address).into())),
                        Some(s.clone()),
                    ))
                }
            }
            _ => {}
        }

        response
    }

    // NOTE(emily): address and offset are independent.
    // address is absolute and offset is where this is in a parent.
    fn none_ui(
        ui: &mut egui::Ui,
        registry: &mut Registry,
        memory: &mut Memory<'_>,
        address: usize,
        offset_in_parent: usize,
        size: Option<usize>,
        sections: Option<&[Section]>,
    ) -> (usize, Option<AddressResponse>) {
        let mut response = None;

        let size = size.unwrap_or(none_ui_rules(offset_in_parent));

        let mut buffer = vec![0; size];

        // TODO(emily): In here we are padding by padding the strings which works because they are monospace.
        // Ideally we should figure out how long each bit SHOULD be, given 8 bytes and then work backwards.

        memory.get(address, &mut buffer);

        ui.with_layout(egui::Layout::left_to_right(egui::Align::Min), |ui| {
            {
                let label = egui::Label::new(RichText::new(&format!("{:04X}", offset_in_parent)))
                    .selectable(false);

                ui.add(label);
            }

            ui.add_space(20.0);

            {
                let label =
                    egui::Label::new(RichText::new(&format!("{:016X}", address))).selectable(true);

                ui.add(label);
            }

            ui.add_space(20.0);

            {
                let padding = 8 - buffer.len();
                let label = egui::Label::new(RichText::new(format!(
                    "{}{}",
                    buffer
                        .iter()
                        .map(|b| format!("{}", memory::ascii_byte(b)))
                        .collect::<String>(),
                    " ".repeat(padding)
                )))
                .selectable(true);
                ui.add(label);
            }

            ui.add_space(20.0);

            {
                let padding = 8 - buffer.len();

                let label = egui::Label::new(RichText::new(format!(
                    "{}{}",
                    buffer
                        .iter()
                        .map(|b| format!("{b:02X}"))
                        .collect::<Vec<_>>()
                        .join(" "),
                    " ".repeat((padding * 2) + padding)
                )))
                .selectable(true);

                ui.add(label);
            }

            ui.add_space(20.0);

            {
                let bytes = &buffer;
                if let Some(r) = memory::disect_bytes(sections, registry, bytes, ui) {
                    response = Some(r);
                }
            }
        });

        (buffer.len(), response)
    }

    fn make_node_options(
        ui: &mut egui::Ui,
        registry: &mut Registry,
        offset: usize,
    ) -> Option<MakeNodeAction> {
        if ui.button("None").clicked() {
            return Some(MakeNodeAction::Remove(offset));
        }
        (|| {
            if ui.button("Pointer").clicked() {
                return Some((Node::Pointer(registry.default_struct()), offset));
            }
            if ui.button("Struct").clicked() {
                return Some((Node::Struct(registry.default_struct()), offset));
            }
            if ui.button("U64").clicked() {
                return Some((Node::U64, offset));
            }
            if ui.button("U32").clicked() {
                return Some((Node::U32, offset));
            }
            if ui.button("U16").clicked() {
                return Some((Node::U16, offset));
            }
            if ui.button("U8").clicked() {
                return Some((Node::U8, offset));
            }

            None
        })()
        .map(|(node, offset)| MakeNodeAction::Add(node, offset))
    }
}

type StructActionFn = dyn FnOnce(&mut Registry);

pub(crate) struct StructAction(Box<StructActionFn>);

impl StructAction {
    fn new<F: FnOnce(&mut Registry) + 'static>(f: F) -> Self {
        Self(Box::new(f))
    }

    pub(crate) fn call(self, registry: &mut Registry) {
        (self.0)(registry)
    }
}

impl std::fmt::Debug for StructAction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("StructAction").field(&"<action>").finish()
    }
}

impl From<Box<StructActionFn>> for StructAction {
    fn from(value: Box<StructActionFn>) -> Self {
        Self(value)
    }
}

impl From<StructAction> for AddressResponse {
    fn from(value: StructAction) -> Self {
        AddressResponse::Action(value)
    }
}

enum MakeNodeAction {
    Add(Node, usize),
    Remove(usize),
}

const NODE_UNIT_SIZE: f32 = 15.0;

#[derive(Default)]
pub(crate) struct StructUiFlags {
    pub(crate) top_level: bool,
}

#[derive(Debug)]
pub(crate) struct Struct {
    size: usize,
    nodes: HashMap<usize, RefCell<Node>>,
    pub(crate) name: String,
}

impl Default for Struct {
    fn default() -> Self {
        Self {
            size: Default::default(),
            nodes: Default::default(),
            name: "Default struct".into(),
        }
    }
}

impl Struct {
    fn row_heights(&self, ui: &egui::Ui) -> StructRowHeightIterator<'_> {
        StructRowHeightIterator {
            nodes: &self.nodes,
            cur_offset: 0,
            size: self.size,
            item_spacing_y: ui.spacing().item_spacing.y,
        }
    }

    fn bytes_for_row(&self, row_idx: usize) -> usize {
        let mut bytes = 0;
        let mut rows = 0;
        while rows < row_idx {
            rows += 1;

            if let Some(node) = self.nodes.get(&bytes) {
                bytes += node.borrow().byte_size();
            } else {
                bytes += none_ui_rules(bytes);
            }
        }

        bytes
    }

    pub(crate) fn ui(
        &self,
        self_rc: Rc<RefCell<Self>>,
        flags: StructUiFlags,
        ui: &mut egui::Ui,
        registry: &mut Registry,
        memory: &mut Memory<'_>,
        address: usize,
        sections: Option<&[Section]>,
    ) -> (usize, Option<crate::AddressResponse>) {
        let mut response = None;

        let max_height = ui.available_height();

        {
            egui::ComboBox::new((ui.id(), address, &self.name), "")
                .selected_text(&self.name)
                .show_ui(ui, |ui| {
                    for (id, (name, s)) in &registry.structs_by_name {
                        if ui.button(format!("{name} ({id})")).clicked() {
                            response = Some(AddressResponse::Replace(s.clone()))
                        }
                    }

                    ui.separator();

                    if ui.button(format!("New struct")).clicked() {
                        response = Some(AddressResponse::Replace(registry.default_struct()))
                    }
                });

            let mut size = self.size;

            if ui
                .add(egui::DragValue::new(&mut size).range(8..=8192))
                .changed()
            {
                // TODO(emily): There should be some easy way to clean up the amount of wrapping going on here
                response = Some(
                    StructAction::new({
                        let zelf = self_rc.clone();
                        move |_| {
                            zelf.borrow_mut().size = size;
                        }
                    })
                    .into(),
                );
            }
        }

        let mut action = None;

        ui.scope(|ui| {
            let style = ui.style_mut();
            style.override_text_style = Some(egui::TextStyle::Monospace);

            let heights = self.row_heights(&ui);

            egui_extras::TableBuilder::new(ui)
                .id_salt((address, &self.name))
                .vscroll(flags.top_level)
                .max_scroll_height(max_height)
                .column(egui_extras::Column::remainder())
                .sense(egui::Sense::click())
                .body(|mut body| {
                    // body.ui_mut().style_mut().spacing.item_spacing = vec2(0.0, 0.0);

                    body.heterogeneous_rows(heights, |mut row| {
                        // TODO(emily): You need to get the number of bytes in that this row would logically be.
                        // probably by iterating like we are doing below but for everything up to this index
                        // maybe cache it so that its not abysmally slow towards the end.
                        let index = row.index();
                        let offset = self.bytes_for_row(index);

                        let (_, r) = row.col(|ui| {
                            let new_address = address.wrapping_add(offset);

                            let (bytes, r) = if let Some(node) = self.nodes.get(&offset) {
                                node.borrow_mut().ui(
                                    ui,
                                    registry,
                                    memory,
                                    new_address,
                                    offset,
                                    sections,
                                )
                            } else {
                                Node::none_ui(
                                    ui,
                                    registry,
                                    memory,
                                    new_address,
                                    offset,
                                    None,
                                    sections,
                                )
                            };

                            if let Some(r) = r {
                                response = Some(r);
                            }
                        });

                        r.context_menu(|ui| {
                            let address = address + offset;

                            if ui.button("Open address in new window").clicked() {
                                response = Some(AddressResponse::AddressStruct(
                                    Some(registry.find_or_register_address(Address::from(address))),
                                    None,
                                ))
                            }

                            if let Some(node) = self.nodes.get(&offset) {
                                ui.separator();
                                if let Some(r) =
                                    node.borrow().context_menu(registry, memory, address, ui)
                                {
                                    response = Some(r);
                                }
                            }

                            ui.separator();
                            action = Node::make_node_options(ui, registry, offset);
                        });
                    });
                });
        });

        match action {
            Some(action) => {
                response = Some(
                    StructAction::new({
                        let zelf = self_rc.clone();
                        move |_registry| {
                            let mut zelf = zelf.borrow_mut();
                            match action {
                                MakeNodeAction::Add(node, offset) => {
                                    zelf.nodes.insert(offset, RefCell::new(node));
                                }
                                MakeNodeAction::Remove(offset) => {
                                    zelf.nodes.remove(&offset);
                                }
                            }
                        }
                    })
                    .into(),
                );
            }

            None => {}
        }

        (self.size, response)
    }

    pub(crate) fn byte_size(&self) -> usize {
        self.size
    }

    fn row_count(&self) -> usize {
        let mut bytes = 0;
        let mut rows = 0;

        while bytes < self.size {
            if let Some(node) = self.nodes.get(&bytes) {
                let node = node.borrow();
                rows += node.row_count();
                bytes += node.byte_size();
            } else {
                rows += 1;
                bytes += none_ui_rules(bytes);
            }
        }

        rows
    }
}

fn none_ui_rules(bytes: usize) -> usize {
    if bytes % 8 == 0 {
        8
    } else if bytes % 8 == 4 {
        4
    } else if bytes % 4 == 2 {
        2
    } else if bytes % 2 == 1 {
        1
    } else {
        unreachable!()
    }
}

struct StructRowHeightIterator<'a> {
    nodes: &'a HashMap<usize, RefCell<Node>>,
    cur_offset: usize,
    size: usize,
    item_spacing_y: f32,
}

impl<'a> Iterator for StructRowHeightIterator<'a> {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        let cur_offset = self.cur_offset;

        if self.cur_offset >= self.size {
            return None;
        }

        if let Some(node) = self.nodes.get(&cur_offset) {
            let node = node.borrow();
            let row_count = node.row_count() as f32;
            self.cur_offset += node.byte_size();
            return Some(row_count * NODE_UNIT_SIZE + 2.0 * row_count * self.item_spacing_y);
        } else {
            self.cur_offset += none_ui_rules(cur_offset);
        }

        Some(NODE_UNIT_SIZE)
    }
}
