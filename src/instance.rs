mod logic_instance;
mod struct_instance;

pub use logic_instance::{LogicCallbacks, LogicInstance};
pub use struct_instance::StructInstance;

use std::{
    cell::RefCell,
    hash::{Hash, Hasher},
    rc::Rc,
};

use egui::{Align, Layout, RichText, collapsing_header::CollapsingState, vec2};
use egui_extras::{Size, StripBuilder};

use crate::{
    State,
    definition::Node,
    memory::{self, highlightable_address_text},
    node::StructUiFlags,
    pane::AddressResponse,
    registry::RegistryId,
    ui::{self, NODE_UNIT_ROW_HEIGHT},
};

#[derive(Hash, Clone)]
pub struct Location(u64);

impl Location {
    pub fn new(instance_id: RegistryId) -> Self {
        Self(instance_id.0 as u64)
    }

    pub fn progress(&self, offset_in_parent: usize) -> Self {
        let mut hasher = std::hash::DefaultHasher::new();
        self.hash(&mut hasher);
        offset_in_parent.hash(&mut hasher);

        Self(hasher.finish())
    }
}

pub struct NodeInstance {
    address: usize,
    offset_in_parent: usize,
    location: Location,

    definition: Rc<RefCell<Node>>,
}

impl NodeInstance {
    pub(crate) fn new(
        address: usize,
        offset_in_parent: usize,
        location: Location,
        definition: Rc<RefCell<Node>>,
    ) -> Self {
        Self {
            address,
            offset_in_parent,
            location,
            definition,
        }
    }

    fn struct_instance(&self, state: &RefCell<State>) -> Option<StructInstance> {
        let (definition, address) = match &*self.definition.borrow() {
            Node::Pointer(p) => {
                let p = p.upgrade().unwrap();
                Some((p, state.borrow_mut().memory.read(self.address)))
            }
            Node::Struct(p) => {
                let p = p.upgrade().unwrap();
                Some((p, self.address))
            }
            _ => None,
        }?;

        Some(StructInstance::new(
            definition,
            address,
            self.location.progress(self.offset_in_parent),
            self.offset_in_parent,
            Some(self.definition.clone()),
        ))
    }

    fn logic_instance(&self, state: &RefCell<State>) -> Option<LogicInstance> {
        let definition = match &*self.definition.borrow() {
            Node::Logic(logic) => {
                let logic = logic.upgrade()?;
                logic
            }
            _ => return None,
        };

        Some(LogicInstance::new(
            definition,
            self.address,
            self.location.progress(self.offset_in_parent),
            self.offset_in_parent,
            state,
        ))
    }

    fn row_count(&self, state: &RefCell<State>) -> usize {
        match &*self.definition.borrow() {
            Node::U8 | Node::U16 | Node::U32 | Node::U64 | Node::Pointer(_) => 1,
            Node::Struct(s) => s.upgrade().map(|s| s.borrow().row_count()).unwrap_or(1),
            Node::Logic(_) => {
                let logic_instance = self.logic_instance(state).unwrap();
                logic_instance.row_count(state)
            }
        }
    }

    pub(crate) fn byte_size(&self) -> usize {
        self.definition.borrow().byte_size()
    }

    pub fn height(&self, item_spacing_y: f32, ctx: &egui::Context, state: &RefCell<State>) -> f32 {
        // Check if this is a logic node - logic nodes support collapsing
        if let Some(logic_instance) = self.logic_instance(state) {
            let collapsing = logic_instance.collapsing(ctx);

            // Header size with extra padding (same as structs)
            let extra = 16.0 + item_spacing_y;

            let openness = collapsing.openness(ctx);

            if openness == 0.0 {
                return extra; // Just header when collapsed
            }

            // Calculate total height of all evaluated nodes
            let nodes = logic_instance.evaluate(state);
            let content_height: f32 = nodes
                .iter()
                .enumerate()
                .map(|(idx, node_rc)| {
                    let node_instance = NodeInstance::new(
                        logic_instance.address,
                        logic_instance.offset_in_parent + idx * 8, // Approximate offset
                        logic_instance.location.progress(idx * 8),
                        node_rc.clone(),
                    );
                    node_instance.height(item_spacing_y, ctx, state) + item_spacing_y
                })
                .sum();

            return content_height * openness + extra;
        }

        let Some(struct_instance) = self.struct_instance(state) else {
            return ui::NODE_UNIT_ROW_HEIGHT;
        };

        let item_spacing_y = item_spacing_y;

        let collapsing = struct_instance.collapsing(ctx);

        // NOTE(emily): Here we account for the extra padding in the egui table.
        let extra = 16.0 + item_spacing_y;

        let openness = collapsing.openness(ctx);

        if openness == 0.0 {
            return extra;
        }

        let height: f32 = struct_instance
            .row_heights(item_spacing_y, ctx, state)
            .map(|x| x + item_spacing_y)
            .sum();

        height * openness + extra
    }

    pub(crate) fn heading_offset_and_address_inner<F: FnOnce(&mut egui::Ui)>(
        ui: &mut egui::Ui,
        address: usize,
        offset: usize,
        left_f: F,
        state: &RefCell<State>,
    ) {
        let glyph_width = ui::glyph_width(ui, '.');

        let collapsing_icon_width = ui.spacing().icon_width;

        egui_extras::StripBuilder::new(ui)
            .size(Size::exact(collapsing_icon_width))
            .size(Size::exact(glyph_width * 8.0))
            .size(Size::exact(glyph_width * 16.0 + glyph_width * 4.0))
            .horizontal(|mut strip| {
                strip.cell(left_f);

                strip.cell(|ui| {
                    let label =
                        egui::Label::new(RichText::new(format!("{:04}", offset))).selectable(false);

                    ui.add(label);
                });

                strip.cell(|ui| {
                    let label = egui::Label::new(highlightable_address_text(
                        state,
                        address,
                        format!("{:016X}", address),
                    ))
                    .selectable(true);

                    if ui.add(label).hovered() {
                        state.borrow_mut().this_frame_mut().highlighted_address = Some(address);
                    }
                });
            });
    }

    fn heading_offset_and_address<F: FnOnce(&mut egui::Ui)>(
        &self,
        ui: &mut egui::Ui,
        left_f: F,
        state: &RefCell<State>,
    ) {
        Self::heading_offset_and_address_inner(
            ui,
            self.address,
            self.offset_in_parent,
            left_f,
            state,
        );
    }

    fn struct_heading(
        &self,
        ui: &mut egui::Ui,
        struct_instance: &StructInstance,
        state: &RefCell<State>,
    ) -> CollapsingState {
        // TODO(emily): Move into StructInstance probably
        let mut collapsing = struct_instance.collapsing(ui.ctx());

        ui.allocate_ui_with_layout(
            vec2(ui.available_width(), NODE_UNIT_ROW_HEIGHT),
            Layout::left_to_right(Align::Center),
            |ui| {
                let openness = collapsing.openness(ui.ctx());

                self.heading_offset_and_address(
                    ui,
                    |ui| {
                        let (_id, rect) =
                            ui.allocate_space(egui::Vec2::splat(ui.spacing().icon_width));
                        let response =
                            ui.interact(rect, ui.id().with(collapsing.id()), egui::Sense::click());
                        if response.clicked() {
                            collapsing.toggle(ui);
                        }

                        egui::collapsing_header::paint_default_icon(ui, openness, &response);
                    },
                    state,
                );

                match &*self.definition.borrow() {
                    Node::Pointer(_) => ui.label("Pointer"),
                    Node::Struct(_) => ui.label("Struct"),
                    _ => unreachable!(),
                };

                ui.add_space(ui::spacing(ui));

                struct_instance.heading(ui, state);

                ui.add_space(ui::spacing(ui));

                if ui
                    .label(highlightable_address_text(
                        state,
                        struct_instance.address,
                        format!("{:016X}", struct_instance.address),
                    ))
                    .hovered()
                {
                    state.borrow_mut().this_frame_mut().highlighted_address =
                        Some(struct_instance.address)
                }

                memory::disect_address(state, struct_instance.address, ui);
            },
        );
        collapsing
    }

    fn heading(&self, ui: &mut egui::Ui, state: &RefCell<State>) {
        ui.allocate_ui_with_layout(
            vec2(ui.available_width(), ui::NODE_UNIT_ROW_HEIGHT),
            Layout::left_to_right(Align::Center),
            |ui| {
                self.heading_offset_and_address(ui, |_ui| {}, state);
            },
        );
    }

    fn logic_heading(
        &self,
        ui: &mut egui::Ui,
        logic_instance: &LogicInstance,
        state: &RefCell<State>,
    ) -> CollapsingState {
        let mut collapsing = logic_instance.collapsing(ui.ctx());

        ui.allocate_ui_with_layout(
            vec2(ui.available_width(), NODE_UNIT_ROW_HEIGHT),
            Layout::left_to_right(Align::Center),
            |ui| {
                let openness = collapsing.openness(ui.ctx());

                self.heading_offset_and_address(
                    ui,
                    |ui| {
                        let (_id, rect) =
                            ui.allocate_space(egui::Vec2::splat(ui.spacing().icon_width));
                        let response =
                            ui.interact(rect, ui.id().with(collapsing.id()), egui::Sense::click());
                        if response.clicked() {
                            collapsing.toggle(ui);
                        }

                        egui::collapsing_header::paint_default_icon(ui, openness, &response);
                    },
                    state,
                );

                ui.label("Logic");

                ui.add_space(ui::spacing(ui));

                // Display logic name
                let logic_name = logic_instance.name();
                ui.label(&*logic_name);

                ui.add_space(ui::spacing(ui));

                // Display node count
                let nodes = logic_instance.evaluate(state);
                ui.label(format!("({} nodes)", nodes.len()));
            },
        );
        collapsing
    }

    fn node_struct_ui_inner(
        &self,
        ui: &mut egui::Ui,
        struct_instance: &StructInstance,
        state: &RefCell<State>,
    ) {
        let height = self.height(ui.spacing().item_spacing.y, ui.ctx(), state);

        ui.allocate_ui_with_layout(
            vec2(ui.available_width(), height),
            Layout::top_down(Align::Min),
            |ui| {
                let spacing = ui::spacing(ui);

                let mut collapse_state = self.struct_heading(ui, struct_instance, state);

                collapse_state.show_body_unindented(ui, |ui| {
                    ui.with_layout(Layout::left_to_right(Align::Center), |ui| {
                        ui.add_space(spacing);

                        struct_instance.ui(StructUiFlags::default(), ui, state);
                    })
                    .inner
                });
            },
        );
    }

    fn logic_instance_ui_inner(
        &self,
        ui: &mut egui::Ui,
        logic_instance: &LogicInstance,
        state: &RefCell<State>,
    ) {
        let height = self.height(ui.spacing().item_spacing.y, ui.ctx(), state);

        ui.allocate_ui_with_layout(
            vec2(ui.available_width(), height),
            Layout::top_down(Align::Min),
            |ui| {
                let spacing = ui::spacing(ui);

                let mut collapse_state = self.logic_heading(ui, logic_instance, state);

                collapse_state.show_body_unindented(ui, |ui| {
                    ui.with_layout(Layout::left_to_right(Align::Center), |ui| {
                        ui.add_space(spacing);

                        logic_instance.ui(ui, state);
                    })
                    .inner
                });
            },
        );
    }

    fn node_ui_inner(&self, ui: &mut egui::Ui, state: &RefCell<State>) {
        ui.with_layout(Layout::left_to_right(Align::Min), |ui| {
            self.heading(ui, state);

            let definition = self.definition.borrow();
            let size = match &*definition {
                Node::U64 => {
                    ui.label("U64");
                    8
                }
                Node::U32 => {
                    ui.label("U32");
                    4
                }
                Node::U16 => {
                    ui.label("U16");
                    2
                }
                Node::U8 => {
                    ui.label("U8");
                    1
                }
                Node::Logic(_) => unreachable!("Logic nodes handled separately"),
                _ => unreachable!(),
            };

            self.none_ui(ui, Some(size), state);
        });
    }

    pub(crate) fn ui(&self, ui: &mut egui::Ui, state: &RefCell<State>) {
        if let Some(struct_instance) = self.struct_instance(state) {
            self.node_struct_ui_inner(ui, &struct_instance, state);
        } else if let Some(logic_instance) = self.logic_instance(state) {
            self.logic_instance_ui_inner(ui, &logic_instance, state);
        } else {
            self.node_ui_inner(ui, state);
        }
    }

    pub(crate) fn context_menu(&self, ui: &mut egui::Ui, state: &RefCell<State>) {
        if let Some(struct_instance) = self.struct_instance(state)
            && ui.button("Open struct in new tab").clicked()
        {
            let address = state
                .borrow_mut()
                .registry
                .find_or_register_address(self.address.into());

            state.borrow_mut().response(AddressResponse::AddressStruct(
                Some(address),
                Some(struct_instance.definition),
            ))
        }
    }

    pub(crate) fn none_ui_inner(
        ui: &mut egui::Ui,
        address: usize,
        offset_in_parent: usize,
        size: Option<usize>,
        state: &RefCell<State>,
    ) {
        let size = size.unwrap_or(none_ui_rules(offset_in_parent));
        let mut buffer = vec![0; size];

        // TODO(emily): In here we are padding by padding the strings which works because they are monospace.
        // Ideally we should figure out how long each bit SHOULD be, given 8 bytes and then work backwards.

        // TODO(emily): The above is extra important once we have specific UI for each node type. As each node
        // needs to where to place its value, which should be in alignment with none_ui

        let glyph_width = ui::glyph_width(ui, '.');
        let _spacing = 4.0 * glyph_width;

        state.borrow_mut().memory.get(address, &mut buffer);

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
                    memory::disect_bytes(state, bytes, ui)
                });
            });
    }

    fn none_ui(&self, ui: &mut egui::Ui, size: Option<usize>, state: &RefCell<State>) {
        Self::none_ui_inner(ui, self.address, self.offset_in_parent, size, state)
    }

    pub(crate) fn make_node_options(
        ui: &mut egui::Ui,
        row_index: usize,
        state: &RefCell<State>,
    ) -> Option<MakeNodeAction> {
        if ui.button("None").clicked() {
            return Some(MakeNodeAction::Remove(row_index));
        }
        (|| {
            if ui.button("Pointer").clicked() {
                return Some((
                    Node::Pointer(Rc::downgrade(&state.borrow_mut().registry.default_struct())),
                    row_index,
                ));
            }
            if ui.button("Struct").clicked() {
                return Some((
                    Node::Struct(Rc::downgrade(&state.borrow_mut().registry.default_struct())),
                    row_index,
                ));
            }
            if ui.button("Logic").clicked() {
                return Some((
                    Node::Logic(Rc::downgrade(&state.borrow_mut().registry.default_logic())),
                    row_index,
                ));
            }
            if ui.button("U64").clicked() {
                return Some((Node::U64, row_index));
            }
            if ui.button("U32").clicked() {
                return Some((Node::U32, row_index));
            }
            if ui.button("U16").clicked() {
                return Some((Node::U16, row_index));
            }
            if ui.button("U8").clicked() {
                return Some((Node::U8, row_index));
            }

            None
        })()
        .map(|(node, row_index)| MakeNodeAction::Add(node, row_index))
    }
}

pub(crate) enum MakeNodeAction {
    Add(Node, usize),
    Remove(usize),
}

pub(crate) fn none_ui_rules(bytes: usize) -> usize {
    if bytes.is_multiple_of(8) {
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
