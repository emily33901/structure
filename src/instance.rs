use std::{
    cell::{Ref, RefCell},
    hash::{Hash, Hasher},
    rc::Rc,
};

use egui::{Align, Layout, RichText, collapsing_header::CollapsingState, vec2};
use egui_extras::{Column, Size, StripBuilder};

use crate::{
    Address, State,
    definition::{Logic, Node, Struct},
    memory::{self, highlightable_address_text},
    node::{StructAction, StructUiFlags},
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
    fn new(
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
        ))
    }

    fn logic_instance(&self, _state: &RefCell<State>) -> Option<LogicInstance> {
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
        ))
    }
}

pub struct StructInstance {
    address: usize,
    offset_in_parent: usize,
    location: Location,
    definition: Rc<RefCell<Struct>>,
}

impl StructInstance {
    pub fn new(
        definition: Rc<RefCell<Struct>>,
        address: usize,
        location: Location,
        offset_in_parent: usize,
    ) -> Self {
        Self {
            definition,
            offset_in_parent,
            location,
            address,
        }
    }

    fn ui_id(&self) -> egui::Id {
        egui::Id::new(&self.location)
    }

    fn collapsing(&self, ctx: &egui::Context) -> CollapsingState {
        let eid = self.ui_id();
        CollapsingState::load_with_default_open(ctx, eid, false)
    }
}

pub struct LogicInstance {
    address: usize,
    offset_in_parent: usize,
    location: Location,
    definition: Rc<RefCell<Logic>>,
    cached_nodes: RefCell<Option<Vec<Rc<RefCell<Node>>>>>,
    cached_offsets: RefCell<Option<Vec<usize>>>,
}

impl LogicInstance {
    pub fn new(
        definition: Rc<RefCell<Logic>>,
        address: usize,
        location: Location,
        offset_in_parent: usize,
    ) -> Self {
        Self {
            definition,
            offset_in_parent,
            location,
            address,
            cached_nodes: Default::default(),
            cached_offsets: Default::default(),
        }
    }

    fn ui_id(&self) -> egui::Id {
        egui::Id::new(&self.location)
    }

    fn collapsing(&self, ctx: &egui::Context) -> CollapsingState {
        let eid = self.ui_id();
        CollapsingState::load_with_default_open(ctx, eid, false)
    }

    fn id(&self) -> RegistryId {
        self.definition.borrow().id
    }

    fn name(&self) -> Ref<'_, str> {
        Ref::map(self.definition.borrow(), |definition| {
            definition.name.as_str()
        })
    }

    fn evaluate(&self, state: &RefCell<State>) -> Vec<Rc<RefCell<Node>>> {
        if let Some(cached) = self.cached_nodes.borrow().as_ref() {
            return cached.clone();
        }

        // Evaluate script
        let script = self.definition.borrow().script.clone();
        let mut scope = rhai::Scope::new();
        scope.push("address", self.address as i64);

        let result = state.borrow().script_engine.evaluate(&script, &mut scope);

        let nodes = match result {
            Ok(node_type_str) => crate::script::parse_multiple_nodes(&node_type_str)
                .into_iter()
                .map(|node| Rc::new(RefCell::new(node)))
                .collect(),
            Err(_) => vec![],
        };

        // Cache the offsets for each node
        let mut offsets = Vec::with_capacity(nodes.len());
        let mut current_offset = 0;
        for node_rc in &nodes {
            offsets.push(current_offset);
            current_offset += node_rc.borrow().byte_size();
        }

        *self.cached_nodes.borrow_mut() = Some(nodes.clone());
        *self.cached_offsets.borrow_mut() = Some(offsets);
        nodes
    }

    fn offset_for_row(&self, index: usize) -> usize {
        self.cached_offsets
            .borrow()
            .as_ref()
            .and_then(|offsets| offsets.get(index).copied())
            .unwrap_or(0)
    }

    fn row_heights<'instance, 'state, 'state_owner>(
        &'instance self,
        ctx: egui::Context,
        state: &'state RefCell<State<'state_owner>>,
        item_spacing_y: f32,
    ) -> LogicRowHeightIterator<'instance, 'state, 'state_owner> {
        let nodes = self.evaluate(state);
        LogicRowHeightIterator {
            ctx,
            logic: self,
            nodes,
            state,
            cur_row: 0,
            cur_offset: 0,
            item_spacing_y,
        }
    }

    pub fn row_count(&self, state: &RefCell<State>) -> usize {
        let nodes = self.evaluate(state);
        if nodes.is_empty() {
            return 1; // Error case, show 1 row for error message
        }
        nodes.len() // Each node gets 1 row for now (simplified)
    }

    pub fn byte_size(&self, state: &RefCell<State>) -> usize {
        self.evaluate(state)
            .iter()
            .map(|node_rc| node_rc.borrow().byte_size())
            .sum()
    }

    pub fn ui(&self, ui: &mut egui::Ui, state: &RefCell<State>) {
        let max_height = ui.available_height();
        let nodes = self.evaluate(state);

        if nodes.is_empty() {
            ui.colored_label(egui::Color32::YELLOW, "Script returned no nodes");
            return;
        }

        ui.with_layout(Layout::top_down(Align::Min), |ui| {
            let style = ui.style_mut();
            style.override_text_style = Some(egui::TextStyle::Monospace);

            let heights = self.row_heights(ui.ctx().clone(), state, ui.spacing().item_spacing.y);

            egui_extras::TableBuilder::new(ui)
                .vscroll(false)
                .max_scroll_height(max_height)
                .column(Column::remainder())
                .sense(egui::Sense::click())
                .body(|body| {
                    body.heterogeneous_rows(heights, |mut row| {
                        let index = row.index();
                        let offset = self.offset_for_row(index);

                        row.col(|ui| {
                            if index >= nodes.len() {
                                return;
                            }

                            let node_rc = &nodes[index];
                            let node_instance = NodeInstance::new(
                                self.address + offset,
                                self.offset_in_parent + offset,
                                self.location.progress(offset),
                                node_rc.clone(),
                            );

                            node_instance.ui(ui, state);
                        });
                    });
                });
        });
    }
}

struct LogicRowHeightIterator<'instance, 'state, 'state_owner> {
    ctx: egui::Context,
    logic: &'instance LogicInstance,
    nodes: Vec<Rc<RefCell<Node>>>,
    state: &'state RefCell<State<'state_owner>>,
    cur_row: usize,
    cur_offset: usize,
    item_spacing_y: f32,
}

impl<'instance, 'state, 'state_owner> Iterator
    for LogicRowHeightIterator<'instance, 'state, 'state_owner>
{
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur_row >= self.nodes.len() {
            return None;
        }

        let node_rc = &self.nodes[self.cur_row];
        let node_instance = NodeInstance::new(
            self.logic.address + self.cur_offset,
            self.logic.offset_in_parent + self.cur_offset,
            self.logic.location.progress(self.cur_offset),
            node_rc.clone(),
        );

        let height = node_instance.height(self.item_spacing_y, &self.ctx, self.state);

        self.cur_offset += node_rc.borrow().byte_size();
        self.cur_row += 1;

        Some(height)
    }
}

impl NodeInstance {
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

    fn byte_size(&self) -> usize {
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

    fn heading_offset_and_address_inner<F: FnOnce(&mut egui::Ui)>(
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

    fn ui(&self, ui: &mut egui::Ui, state: &RefCell<State>) {
        if let Some(struct_instance) = self.struct_instance(state) {
            self.node_struct_ui_inner(ui, &struct_instance, state);
        } else if let Some(logic_instance) = self.logic_instance(state) {
            self.logic_instance_ui_inner(ui, &logic_instance, state);
        } else {
            self.node_ui_inner(ui, state);
        }
    }

    fn context_menu(&self, ui: &mut egui::Ui, state: &RefCell<State>) {
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

    fn none_ui_inner(
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

    fn make_node_options(
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

enum MakeNodeAction {
    Add(Node, usize),
    Remove(usize),
}

impl StructInstance {
    fn id(&self) -> RegistryId {
        self.definition.borrow().id
    }

    fn name(&self) -> Ref<'_, str> {
        Ref::map(self.definition.borrow(), |definition| {
            definition.name.as_str()
        })
    }

    fn node(&self, row: usize, address: usize, offset: usize) -> Option<NodeInstance> {
        let definition = self.definition.borrow();
        let node_cell = definition.nodes.get(&row)?;
        // Wrap the node in an Rc to satisfy NodeInstance's requirement
        let node_rc = Rc::new(RefCell::new(node_cell.borrow().clone()));
        Some(NodeInstance::new(
            address,
            offset,
            self.location.progress(offset),
            node_rc,
        ))
    }

    fn row_count(&self) -> usize {
        self.definition.borrow().row_count()
    }

    fn row_heights<'a, 'b, 'c>(
        &'a self,
        item_spacing_y: f32,
        ctx: &egui::Context,
        state: &'b RefCell<State<'c>>,
    ) -> StructRowHeightIterator<'a, 'b, 'c> {
        StructRowHeightIterator {
            ctx: ctx.clone(),
            instance: self,
            state,
            cur_offset: 0,
            cur_row: 0,
            row_count: self.row_count(),
            item_spacing_y,
        }
    }

    fn bytes_for_row(&self, row_index: usize) -> usize {
        // TODO(emily): This is abysmal.
        let mut bytes = 0;
        for row in 0..row_index {
            if let Some(node) = self.node(row, self.address + bytes, bytes) {
                bytes += node.byte_size();
            } else {
                bytes += none_ui_rules(bytes);
            }
        }

        bytes
    }

    pub(crate) fn heading(&self, ui: &mut egui::Ui, state: &RefCell<State>) {
        ui.allocate_ui_with_layout(
            vec2(ui.available_width(), ui::NODE_UNIT_ROW_HEIGHT),
            Layout::left_to_right(Align::Center),
            |ui| {
                let self_name = self.name();
                let self_id = self.id();

                egui::ComboBox::new((self.ui_id(), "struct-replace-combo-box"), "")
                    .selected_text(&*self_name)
                    .show_ui(ui, |ui| {
                        let mut state = state.borrow_mut();
                        for (id, other_struct) in state.registry.structs.clone() {
                            let other_struct_name = other_struct.borrow().name.clone();
                            if ui
                                .add(
                                    egui::Button::new(format!("{} ({id})", other_struct_name))
                                        .selected(id == self_id),
                                )
                                .clicked()
                            {
                                state.response(AddressResponse::Replace(other_struct.clone()))
                            }
                        }

                        ui.separator();

                        if ui.button("New struct".to_string()).clicked() {
                            let default_struct = state.registry.default_struct();
                            state.response(AddressResponse::Replace(default_struct));
                        }
                    });

                let mut row_count = self.row_count();

                if ui
                    .add(egui::DragValue::new(&mut row_count).range(1..=8192))
                    .changed()
                {
                    // TODO(emily): There should be some easy way to clean up the amount of wrapping going on here
                    state.borrow_mut().response(StructAction::new({
                        let definition = self.definition.clone();
                        move |_| {
                            definition.borrow_mut().row_count = row_count;
                        }
                    }))
                }

                ui.end_row();
            },
        );
    }

    pub(crate) fn ui(&self, flags: StructUiFlags, ui: &mut egui::Ui, state: &RefCell<State>) {
        let max_height = ui.available_height();

        let mut size = 0;

        ui.with_layout(Layout::top_down(Align::Min), |ui| {
            let mut action = None;

            let style = ui.style_mut();
            style.override_text_style = Some(egui::TextStyle::Monospace);

            let heights = self.row_heights(ui.spacing().item_spacing.y, ui.ctx(), state);
            let _self_name = self.name();

            egui_extras::TableBuilder::new(ui)
                // .id_salt((address, &self_name))
                .vscroll(flags.top_level)
                .max_scroll_height(max_height)
                .column(egui_extras::Column::remainder())
                .sense(egui::Sense::click())
                .body(|body| {
                    body.heterogeneous_rows(heights, |mut row| {
                        // TODO(emily): You need to get the number of bytes in that this row would logically be.
                        // probably by iterating like we are doing below but for everything up to this index
                        // maybe cache it so that its not abysmally slow towards the end.
                        let index = row.index();
                        let offset = self.bytes_for_row(index);

                        let (_, r) = row.col(|ui| {
                            let new_address = self.address.wrapping_add(offset);

                            let Some(node) = self.node(index, new_address, offset) else {
                                // TODO(emily): Kind of weird that node.ui handles the heading and yet
                                // none ui 'requires' us to rendering the heading here.

                                ui.allocate_ui_with_layout(
                                    vec2(ui.available_width(), NODE_UNIT_ROW_HEIGHT),
                                    Layout::left_to_right(Align::Center),
                                    |ui| {
                                        NodeInstance::heading_offset_and_address_inner(
                                            ui,
                                            new_address,
                                            offset,
                                            |_ui| {},
                                            state,
                                        );

                                        NodeInstance::none_ui_inner(
                                            ui,
                                            new_address,
                                            offset,
                                            None,
                                            state,
                                        )
                                    },
                                );
                                return;
                            };

                            node.ui(ui, state);

                            let bytes = node.byte_size();

                            // Accumulate bytes for the total size of this struct
                            size += bytes;
                        });

                        r.context_menu(|ui| {
                            let address = self.address + offset;

                            if ui.button("Open address in new window").clicked() {
                                state.borrow_mut().response(AddressResponse::AddressStruct(
                                    Some(
                                        state
                                            .borrow_mut()
                                            .registry
                                            .find_or_register_address(Address::from(address)),
                                    ),
                                    None,
                                ))
                            }

                            if let Some(node) = self.node(index, address, offset) {
                                ui.separator();
                                node.context_menu(ui, state);
                            }

                            ui.separator();
                            action = NodeInstance::make_node_options(ui, index, state);
                        });
                    });
                });

            if let Some(action) = action {
                state.borrow_mut().response(StructAction::new({
                    let definition = self.definition.clone();
                    move |_registry| {
                        let mut definition = definition.borrow_mut();
                        match action {
                            MakeNodeAction::Add(node, offset) => {
                                definition.nodes.insert(offset, RefCell::new(node));
                            }
                            MakeNodeAction::Remove(row) => {
                                definition.nodes.remove(&row);
                            }
                        }
                    }
                }));
            }
        });
    }
}

struct StructRowHeightIterator<'instance, 'state, 'state_owner> {
    ctx: egui::Context,
    instance: &'instance StructInstance,
    state: &'state RefCell<State<'state_owner>>,
    cur_row: usize,
    cur_offset: usize,
    row_count: usize,
    item_spacing_y: f32,
}

impl<'a, 'b, 'c> StructRowHeightIterator<'a, 'b, 'c> {
    fn node(&self, row: usize, address: usize, offset: usize) -> Option<NodeInstance> {
        self.instance.node(row, address, offset)
    }

    fn row_height(&mut self, index: usize, address: usize, offset: usize) -> (f32, usize) {
        let Some(node) = self.node(index, address, self.cur_offset) else {
            return (ui::NODE_UNIT_ROW_HEIGHT, none_ui_rules(offset));
        };

        (
            node.height(self.item_spacing_y, &self.ctx, self.state),
            node.byte_size(),
        )
    }
}

impl<'a, 'b, 'c> Iterator for StructRowHeightIterator<'a, 'b, 'c> {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        let cur_row = self.cur_row;
        let cur_offset = self.cur_offset;

        if cur_row >= self.row_count {
            return None;
        }

        let row_index = self.cur_row;
        let address = self.instance.address + self.cur_offset;

        let (height, bytes) = self.row_height(row_index, address, cur_offset);

        self.cur_row += 1;
        self.cur_offset += bytes;

        Some(height)
    }
}

fn none_ui_rules(bytes: usize) -> usize {
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
