use core::f32;
use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use egui::{
    ahash::HashMap, collapsing_header::CollapsingState, vec2, Align, InnerResponse, Layout,
    RichText,
};
use egui_extras::{Size, StripBuilder};

use crate::{
    memory::{self, Memory},
    registry::{Registry, RegistryId},
    Address, AddressResponse, State,
};

fn glyph_width(ui: &egui::Ui, c: char) -> f32 {
    let font_id = ui
        .style()
        .override_text_style
        .as_ref()
        .unwrap()
        .resolve(ui.style());

    ui.fonts(|f| f.glyph_width(&font_id, c))
}

fn spacing(ui: &egui::Ui) -> f32 {
    let glyph_width = glyph_width(ui, '.');
    let spacing = 6.0 * glyph_width;

    spacing
}

// TODO(emily): Need some way to collapse the view of Struct or Pointer
// either do this here or in Struct::nodes
#[derive(Debug)]
pub(crate) enum Node {
    U8,
    U16,
    U32,
    U64,
    Struct(Weak<RefCell<Struct>>, RefCell<f32>),
    Pointer(Weak<RefCell<Struct>>, RefCell<f32>),
}

impl Node {
    fn row_count(&self) -> usize {
        match self {
            Self::U64 | Self::U32 | Self::U16 | Self::U8 => 1,
            Self::Pointer(s, _) | Self::Struct(s, _) => {
                s.upgrade().map(|s| s.borrow().row_count()).unwrap_or(1)
            }
        }
    }

    fn byte_size(&self) -> usize {
        match self {
            Self::U64 => 8,
            Self::U32 => 4,
            Self::U16 => 2,
            Self::U8 => 1,
            Self::Pointer(_, _) => 8,
            Self::Struct(s, _) => s
                .upgrade()
                .map(|s| {
                    let size = s.borrow().byte_size();
                    assert_ne!(size, 0);
                    size
                })
                .unwrap_or(8),
        }
    }

    fn height(&self, item_spacing_y: f32) -> f32 {
        match self {
            Self::U8 | Self::U16 | Self::U32 | Self::U64 => NODE_UNIT_ROW_HEIGHT,
            Self::Struct(s, openness) | Self::Pointer(s, openness) => {
                // NOTE(emily): Here we account for the extra padding in the egui table.
                let extra = 16.0 + item_spacing_y;

                let openness = *openness.borrow();

                if openness == 0.0 {
                    return extra;
                }

                let height = s
                    .upgrade()
                    .map(|s| {
                        let row_heights: f32 = s
                            .borrow()
                            .row_heights(item_spacing_y)
                            .map(|x| x + item_spacing_y)
                            .sum();

                        row_heights
                    })
                    .unwrap_or(NODE_UNIT_ROW_HEIGHT);

                height * openness + extra
            }
        }
    }

    fn heading_offset_and_address<F: FnOnce(&mut egui::Ui)>(
        ui: &mut egui::Ui,
        address: usize,
        offset_in_parent: usize,
        f: F,
    ) {
        let glyph_width = glyph_width(ui, '.');

        let collapsing_icon_width = ui.spacing().icon_width;

        egui_extras::StripBuilder::new(ui)
            .size(Size::exact(collapsing_icon_width))
            .size(Size::exact(glyph_width * 8.0))
            .size(Size::exact(glyph_width * 16.0 + glyph_width * 4.0))
            .horizontal(|mut strip| {
                strip.cell(f);

                strip.cell(|ui| {
                    let label =
                        egui::Label::new(RichText::new(&format!("{:04}", offset_in_parent)))
                            .selectable(false);

                    ui.add(label);
                });

                strip.cell(|ui| {
                    let label = egui::Label::new(RichText::new(&format!("{:016X}", address)))
                        .selectable(true);

                    ui.add(label);
                });
            });
    }

    fn node_heading(
        &self,
        ui: &mut egui::Ui,
        address: usize,
        offset_in_parent: usize,
        collapsing: Option<&mut CollapsingState>,
        state: &mut State<'_>,
    ) -> Option<AddressResponse> {
        let response = ui.allocate_ui_with_layout(
            vec2(ui.available_width(), NODE_UNIT_ROW_HEIGHT),
            Layout::left_to_right(Align::Center),
            |ui| {
                let mut response = None;

                let openness = collapsing
                    .as_ref()
                    .map(|collapsing| collapsing.openness(ui.ctx()).clone())
                    .unwrap_or(0.0);

                Self::heading_offset_and_address(
                    ui,
                    address,
                    offset_in_parent,
                    |ui: &mut egui::Ui| {
                        if let Some(collapsing) = collapsing {
                            let (_id, rect) =
                                ui.allocate_space(egui::Vec2::splat(ui.spacing().icon_width));
                            let response = ui.interact(
                                rect,
                                ui.id().with(collapsing.id()),
                                egui::Sense::click(),
                            );
                            if response.clicked() {
                                collapsing.toggle(ui);
                            }

                            egui::collapsing_header::paint_default_icon(ui, openness, &response);
                        }
                    },
                );

                match match self {
                    Self::Struct(s, o) => {
                        ui.label("Struct");
                        *o.borrow_mut() = openness;
                        Some((address, s))
                    }
                    Self::Pointer(s, o) => {
                        ui.label("Pointer");
                        *o.borrow_mut() = openness;
                        Some((state.memory.read(address), s))
                    }
                    _ => None,
                } {
                    Some((address, s)) => {
                        ui.add_space(spacing(ui));

                        response = response.or(s
                            .upgrade()
                            .map(|s| s.borrow().heading(s.clone(), ui, address, state))
                            .unwrap_or_else(|| {
                                ui.label("Invalid struct");
                                None
                            }));
                    }
                    None => {}
                };

                response
            },
        );

        response.inner
    }

    fn node_struct_ui_inner(
        &self,
        s: Weak<RefCell<Struct>>,
        ui: &mut egui::Ui,
        address: usize,
        offset_in_parent: usize,
        row_index_in_parent: usize,
        state: &mut State<'_>,
    ) -> (usize, Option<AddressResponse>) {
        let mut response = None;

        let Some(s) = s.upgrade() else {
            return (8, None);
        };

        let spacing = spacing(ui);

        let eid = egui::Id::new(s.borrow().id).with(row_index_in_parent);

        // TODO(emily): Seems dumb that we have this special case for doing the struct types,
        // and then call this function which takes an option for collapsing
        // that we only have in this special case? Maybe just special case all the way down
        let mut collapsing = CollapsingState::load_with_default_open(ui.ctx(), eid, false);

        response = response.or(self.node_heading(
            ui,
            address,
            offset_in_parent,
            Some(&mut collapsing),
            state,
        ));

        let (bytes, r) = collapsing
            .show_body_unindented(ui, |ui| {
                ui.with_layout(Layout::left_to_right(Align::Center), |ui| {
                    ui.add_space(spacing);

                    s.clone()
                        .borrow()
                        .ui(s.clone(), StructUiFlags::default(), ui, address, state)
                })
                .inner
            })
            .map(|r| r.inner)
            .unwrap_or((s.borrow().byte_size(), None));

        response = response.or(r);

        (bytes, response)
    }

    fn node_ui_inner(
        &self,
        ui: &mut egui::Ui,
        address: usize,
        offset_in_parent: usize,
        row_index_in_parent: usize,
        state: &mut State<'_>,
    ) -> (usize, Option<AddressResponse>) {
        match self {
            Self::Struct(s, _) => {
                let s = s.clone();
                return self.node_struct_ui_inner(
                    s.clone(),
                    ui,
                    address,
                    offset_in_parent,
                    row_index_in_parent,
                    state,
                );
            }
            Self::Pointer(s, _) => {
                let address = state.memory.read(address);
                let s = s.clone();
                return self.node_struct_ui_inner(
                    s.clone(),
                    ui,
                    address,
                    offset_in_parent,
                    row_index_in_parent,
                    state,
                );
            }
            Self::U8 | Self::U16 | Self::U32 | Self::U64 => {}
        }

        // Show Offset and address
        let mut response = None;

        response = response.or(self.node_heading(ui, address, offset_in_parent, None, state));

        let (bytes, r) = ui
            .with_layout(Layout::left_to_right(Align::Min), |ui| match self {
                Self::U64 => {
                    ui.label("U64");
                    Self::none_ui(ui, address, offset_in_parent, Some(8), state)
                }
                Self::U32 => {
                    ui.label("U32");
                    Self::none_ui(ui, address, offset_in_parent, Some(4), state)
                }
                Self::U16 => {
                    ui.label("U16");
                    Self::none_ui(ui, address, offset_in_parent, Some(2), state)
                }
                Self::U8 => {
                    ui.label("U8");
                    Self::none_ui(ui, address, offset_in_parent, Some(1), state)
                }
                Self::Struct(_, _) | Self::Pointer(_, _) => unreachable!(),
            })
            .inner;

        let response = response.or(r);

        (bytes, response)
    }

    // NOTE(emily): address and offset are independent.
    // address is absolute and offset is where this is in a parent.
    fn ui(
        &self,
        ui: &mut egui::Ui,
        address: usize,
        offset_in_parent: usize,
        row_index_in_parent: usize,
        state: &mut State<'_>,
    ) -> (usize, Option<AddressResponse>) {
        let height = self.height(ui.spacing().item_spacing.y);

        let layout = match self {
            Self::U8 | Self::U16 | Self::U32 | Self::U64 => Layout::left_to_right(Align::Center),
            Self::Struct(_, _) | Self::Pointer(_, _) => Layout::top_down(Align::Min),
        };

        ui.push_id((address, row_index_in_parent), |ui| {
            ui.allocate_ui_with_layout(vec2(ui.available_width(), height), layout, |ui| {
                self.node_ui_inner(ui, address, offset_in_parent, row_index_in_parent, state)
            })
            .inner
        })
        .inner
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
            Self::Struct(s, _) => {
                if ui.button("Open struct in new tab").clicked() {
                    response = Some(AddressResponse::AddressStruct(
                        Some(registry.find_or_register_address(address.into())),
                        s.upgrade(),
                    ))
                }
            }
            Self::Pointer(s, _) => {
                let address: usize = memory.read(address);

                if ui.button("Open struct in new tab").clicked() {
                    response = Some(AddressResponse::AddressStruct(
                        Some(registry.find_or_register_address(address.into())),
                        s.upgrade(),
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
        address: usize,
        offset_in_parent: usize,
        size: Option<usize>,
        state: &mut State<'_>,
    ) -> (usize, Option<AddressResponse>) {
        let mut response = None;

        let size = size.unwrap_or(none_ui_rules(offset_in_parent));
        let mut buffer = vec![0; size];

        // TODO(emily): In here we are padding by padding the strings which works because they are monospace.
        // Ideally we should figure out how long each bit SHOULD be, given 8 bytes and then work backwards.

        // TODO(emily): The above is extra important once we have specific UI for each node type. As each node
        // needs to where to place its value, which should be in alignment with none_ui

        let glyph_width = glyph_width(ui, '.');
        let spacing = 4.0 * glyph_width;

        state.memory.get(address, &mut buffer);

        StripBuilder::new(ui)
            .size(Size::exact(glyph_width * 8.0 + glyph_width * 4.0))
            .size(Size::exact(glyph_width * 24.0 + glyph_width * 4.0))
            .size(Size::remainder())
            .horizontal(|mut strip| {
                strip.cell(|ui| {
                    let text = buffer
                        .iter()
                        .map(|b| format!("{}", memory::ascii_byte(b)))
                        .collect::<String>();

                    let label = egui::Label::new(RichText::new(text)).selectable(true);
                    ui.add(label);
                });

                strip.cell(|ui| {
                    let text = buffer
                        .iter()
                        .map(|b| format!("{b:02X}"))
                        .collect::<Vec<_>>()
                        .join(" ");

                    let label = egui::Label::new(RichText::new(text)).selectable(true);

                    ui.add(label);
                });

                strip.cell(|ui| {
                    let bytes = &buffer;
                    if let Some(r) = memory::disect_bytes(state, bytes, ui) {
                        response = Some(r);
                    }
                });
            });
        (buffer.len(), response)
    }

    fn make_node_options(
        ui: &mut egui::Ui,
        registry: &mut Registry,
        row_index: usize,
    ) -> Option<MakeNodeAction> {
        if ui.button("None").clicked() {
            return Some(MakeNodeAction::Remove(row_index));
        }
        (|| {
            if ui.button("Pointer").clicked() {
                return Some((
                    Self::Pointer(Rc::downgrade(&registry.default_struct()), RefCell::new(0.0)),
                    row_index,
                ));
            }
            if ui.button("Struct").clicked() {
                return Some((
                    Self::Struct(Rc::downgrade(&registry.default_struct()), RefCell::new(0.0)),
                    row_index,
                ));
            }
            if ui.button("U64").clicked() {
                return Some((Self::U64, row_index));
            }
            if ui.button("U32").clicked() {
                return Some((Self::U32, row_index));
            }
            if ui.button("U16").clicked() {
                return Some((Self::U16, row_index));
            }
            if ui.button("U8").clicked() {
                return Some((Self::U8, row_index));
            }

            None
        })()
        .map(|(node, row_index)| MakeNodeAction::Add(node, row_index))
    }
}

type StructActionFn = dyn FnOnce(&mut State);

pub(crate) struct StructAction(Box<StructActionFn>);

impl StructAction {
    fn new<F: FnOnce(&mut State) + 'static>(f: F) -> Self {
        Self(Box::new(f))
    }

    pub(crate) fn call(self, state: &mut State<'_>) {
        (self.0)(state)
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

const NODE_UNIT_ROW_HEIGHT: f32 = 18.0;

#[derive(Default)]
pub(crate) struct StructUiFlags {
    pub(crate) top_level: bool,
}

#[derive(Debug, Default)]
pub(crate) struct StructLayout {
    collapse: HashMap<usize, bool>,
}

#[derive(Debug)]
pub(crate) struct Struct {
    pub(crate) layout: StructLayout,
    pub(crate) row_count: usize,
    /// Map of row to Node
    pub(crate) nodes: HashMap<usize, RefCell<Node>>,
    pub(crate) name: String,
    pub(crate) id: RegistryId,
}

pub(crate) struct StructBuilder {
    pub(crate) layout: StructLayout,
    pub(crate) row_count: usize,
    /// Map of row to Node
    pub(crate) nodes: HashMap<usize, RefCell<Node>>,
    pub(crate) name: Option<String>,
}

impl StructBuilder {
    pub(crate) fn new(
        layout: StructLayout,
        row_count: usize,
        nodes: HashMap<usize, RefCell<Node>>,
    ) -> Self {
        Self {
            layout,
            row_count,
            nodes,
            name: None,
        }
    }

    pub(crate) fn default() -> Self {
        Self {
            layout: Default::default(),
            row_count: 8,
            nodes: Default::default(),
            name: None,
        }
    }

    pub(crate) fn name(self, name: &str) -> Self {
        Self {
            name: Some(name.into()),
            ..self
        }
    }

    pub(crate) fn build(self, id: RegistryId) -> Struct {
        Struct {
            layout: self.layout,
            row_count: self.row_count,
            nodes: self.nodes,
            name: self.name.unwrap_or_else(|| format!("struct-{}", id)),
            id,
        }
    }
}

// impl Default for Struct {
//     fn default() -> Self {
//         Self {
//             layout: Default::default(),
//             row_count: 8,
//             nodes: Default::default(),
//             name: "Default struct".into(),
//         }
//     }
// }

impl Struct {
    fn row_heights(&self, item_spacing_y: f32) -> StructRowHeightIterator<'_> {
        StructRowHeightIterator {
            nodes: &self.nodes,
            cur_offset: 0,
            cur_row: 0,
            row_count: self.row_count,
            item_spacing_y,
        }
    }

    fn bytes_for_row(&self, row_index: usize) -> usize {
        let mut bytes = 0;
        for row in 0..row_index {
            if let Some(node) = self.nodes.get(&row) {
                bytes += node.borrow().byte_size();
            } else {
                bytes += none_ui_rules(bytes);
            }
        }

        bytes
    }

    pub(crate) fn heading(
        &self,
        self_rc: Rc<RefCell<Self>>,
        ui: &mut egui::Ui,
        address: usize,
        state: &mut State<'_>,
    ) -> Option<crate::AddressResponse> {
        let mut response = None;

        ui.allocate_ui_with_layout(
            vec2(ui.available_width(), NODE_UNIT_ROW_HEIGHT),
            Layout::left_to_right(Align::Center),
            |ui| {
                {
                    egui::ComboBox::new((self.id, address, &self.name), "")
                        .selected_text(&self.name)
                        .show_ui(ui, |ui| {
                            for (id, s) in &state.registry.structs {
                                if ui
                                    .add(
                                        egui::Button::new(format!("{} ({id})", s.borrow().name))
                                            .selected(Rc::ptr_eq(&self_rc, s)),
                                    )
                                    .clicked()
                                {
                                    response = Some(AddressResponse::Replace(s.clone()))
                                }
                            }

                            ui.separator();

                            if ui.button(format!("New struct")).clicked() {
                                response =
                                    Some(AddressResponse::Replace(state.registry.default_struct()))
                            }
                        });

                    let mut row_count = self.row_count;

                    if ui
                        .add(egui::DragValue::new(&mut row_count).range(1..=8192))
                        .changed()
                    {
                        // TODO(emily): There should be some easy way to clean up the amount of wrapping going on here
                        response = Some(
                            StructAction::new({
                                let zelf = self_rc.clone();
                                move |_| {
                                    zelf.borrow_mut().row_count = row_count;
                                }
                            })
                            .into(),
                        );
                    }
                }

                ui.end_row();
            },
        );

        response
    }

    pub(crate) fn ui(
        &self,
        self_rc: Rc<RefCell<Self>>,
        flags: StructUiFlags,
        ui: &mut egui::Ui,
        address: usize,
        state: &mut State<'_>,
    ) -> (usize, Option<crate::AddressResponse>) {
        let mut response = None;

        let max_height = ui.available_height();

        let mut size = 0;

        ui.with_layout(Layout::top_down(Align::Min), |ui| {
            let mut action = None;

            ui.scope(|ui| {
                let style = ui.style_mut();
                style.override_text_style = Some(egui::TextStyle::Monospace);

                let heights = self.row_heights(ui.spacing().item_spacing.y);

                egui_extras::TableBuilder::new(ui)
                    .id_salt((address, &self.name))
                    .vscroll(flags.top_level)
                    .max_scroll_height(max_height)
                    .column(egui_extras::Column::remainder())
                    .sense(egui::Sense::click())
                    .body(|body| {
                        // body.ui_mut().style_mut().spacing.item_spacing = vec2(0.0, 0.0);

                        body.heterogeneous_rows(heights, |mut row| {
                            // TODO(emily): You need to get the number of bytes in that this row would logically be.
                            // probably by iterating like we are doing below but for everything up to this index
                            // maybe cache it so that its not abysmally slow towards the end.
                            let index = row.index();
                            let offset = self.bytes_for_row(index);

                            let (_, r) = row.col(|ui| {
                                let new_address = address.wrapping_add(offset);

                                let (bytes, r) = if let Some(node) = self.nodes.get(&index) {
                                    let (bytes, response) = node.borrow().ui(ui, new_address, offset, index, state);
                                                        
                                    // Handle replacing Struct with a different Struct
                                    let response = match response {
                                        Some(AddressResponse::Replace(new_s)) => {
                                            let (Node::Struct(s, _) | Node::Pointer(s, _)) = &mut *node.borrow_mut() else {
                                                panic!("AddressResponse::Replace should only come from a Node::Struct or a Node::Pointer");
                                            };

                                            *s = Rc::downgrade(&new_s);
                                            None
                                        }
                                        r => r,
                                    };

                                    (bytes, response)

                                } else {
                                    // TODO(emily): Kind of weird that node.ui handles the heading and yet
                                    // none ui 'requires' us to rendering the heading here.
                                    ui.allocate_ui_with_layout(
                                        vec2(ui.available_width(), NODE_UNIT_ROW_HEIGHT),
                                        Layout::left_to_right(Align::Center),
                                        |ui| {
                                            Node::heading_offset_and_address(
                                                ui,
                                                address,
                                                offset,
                                                |ui| {},
                                            );
                                            Node::none_ui(ui, new_address, offset, None, state)
                                        },
                                    )
                                    .inner
                                };

                                if let Some(r) = r {
                                    response = Some(r);
                                }

                                // Accumulate bytes for the total size of this struct
                                size += bytes;
                            });

                            r.context_menu(|ui| {
                                let address = address + offset;

                                if ui.button("Open address in new window").clicked() {
                                    response = Some(AddressResponse::AddressStruct(
                                        Some(
                                            state
                                                .registry
                                                .find_or_register_address(Address::from(address)),
                                        ),
                                        None,
                                    ))
                                }

                                if let Some(node) = self.nodes.get(&offset) {
                                    ui.separator();
                                    if let Some(r) = node.borrow().context_menu(
                                        state.registry,
                                        state.memory,
                                        address,
                                        ui,
                                    ) {
                                        response = Some(r);
                                    }
                                }

                                ui.separator();
                                action = Node::make_node_options(ui, state.registry, index);
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
                                    MakeNodeAction::Remove(row) => {
                                        zelf.nodes.remove(&row);
                                    }
                                }
                            }
                        })
                        .into(),
                    );
                }

                None => {}
            }
        });

        (size, response)
    }

    pub(crate) fn byte_size(&self) -> usize {
        let mut bytes = 0;

        for row in 0..self.row_count {
            if let Some(node) = self.nodes.get(&row) {
                let node = node.borrow();
                bytes += node.byte_size();
            } else {
                bytes += none_ui_rules(bytes);
            }
        }

        bytes

        // self.size
    }

    fn row_count(&self) -> usize {
        self.row_count
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
    cur_row: usize,
    cur_offset: usize,
    row_count: usize,
    item_spacing_y: f32,
}

impl<'a> Iterator for StructRowHeightIterator<'a> {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        let cur_row = self.cur_row;
        let cur_offset = self.cur_offset;

        if cur_row >= self.row_count {
            return None;
        }

        self.cur_row += 1;

        if let Some(node) = self.nodes.get(&cur_row) {
            let node = node.borrow();
            self.cur_offset += node.byte_size();
            return Some(node.height(self.item_spacing_y));
        } else {
            self.cur_offset += none_ui_rules(cur_offset);
        }

        Some(NODE_UNIT_ROW_HEIGHT)
    }
}
