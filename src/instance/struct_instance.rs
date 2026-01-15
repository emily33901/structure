use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use egui::collapsing_header::CollapsingState;
use egui::{Align, Layout, vec2};

use crate::{
    Address, State,
    definition::{Node, Struct},
    node::{Action, StructUiFlags},
    pane::{AddressResponse, StructResponse},
    registry::RegistryId,
    ui::{self, NODE_UNIT_ROW_HEIGHT},
};

use super::{Location, MakeNodeAction, NodeInstance, none_ui_rules};

pub struct StructInstance {
    pub(super) address: usize,
    pub(super) offset_in_parent: usize,
    pub(super) location: Location,
    pub(super) definition: Rc<RefCell<Struct>>,
    /// The Node that contains this struct (if nested), allows updating the Weak reference
    pub(super) parent_node: Option<Rc<RefCell<Node>>>,
}

impl StructInstance {
    pub fn new(
        definition: Rc<RefCell<Struct>>,
        address: usize,
        location: Location,
        offset_in_parent: usize,
        parent_node: Option<Rc<RefCell<Node>>>,
    ) -> Self {
        Self {
            definition,
            offset_in_parent,
            location,
            address,
            parent_node,
        }
    }

    pub(super) fn ui_id(&self) -> egui::Id {
        egui::Id::new(&self.location)
    }

    pub(super) fn collapsing(&self, ctx: &egui::Context) -> CollapsingState {
        let eid = self.ui_id();
        CollapsingState::load_with_default_open(ctx, eid, false)
    }

    pub(super) fn id(&self) -> RegistryId {
        self.definition.borrow().id
    }

    pub(super) fn name(&self) -> Ref<'_, str> {
        Ref::map(self.definition.borrow(), |definition| {
            definition.name.as_str()
        })
    }

    pub(super) fn node(&self, row: usize, address: usize, offset: usize) -> Option<NodeInstance> {
        let definition = self.definition.borrow();
        let node = definition.nodes.get(&row)?;
        Some(NodeInstance::new(
            address,
            offset,
            self.location.progress(offset),
            node.clone(),
        ))
    }

    pub(super) fn row_count(&self) -> usize {
        self.definition.borrow().row_count()
    }

    pub(super) fn row_heights<'a, 'b, 'c>(
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

    pub(super) fn bytes_for_row(&self, row_index: usize, state: &RefCell<State>) -> usize {
        // TODO(emily): This is abysmal.
        let mut bytes = 0;
        for row in 0..row_index {
            if let Some(node) = self.node(row, self.address + bytes, bytes) {
                bytes += node.byte_size(state);
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
                                if let Some(parent_node) = &self.parent_node {
                                    state.response(StructResponse::Replace(
                                        Rc::downgrade(parent_node),
                                        Rc::downgrade(&other_struct),
                                    ))
                                } else {
                                    state.response(AddressResponse::Replace(other_struct.clone()))
                                }
                            }
                        }

                        ui.separator();

                        if ui.button("New struct".to_string()).clicked() {
                            let default_struct = state.registry.default_struct();
                            if let Some(parent_node) = &self.parent_node {
                                state.response(StructResponse::Replace(
                                    Rc::downgrade(parent_node),
                                    Rc::downgrade(&default_struct),
                                ))
                            } else {
                                state.response(AddressResponse::Replace(default_struct));
                            }
                        }
                    });

                let mut row_count = self.row_count();

                if ui
                    .add(egui::DragValue::new(&mut row_count).range(1..=8192))
                    .changed()
                {
                    // TODO(emily): There should be some easy way to clean up the amount of wrapping going on here
                    state.borrow_mut().response(Action::new({
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
                        let offset = self.bytes_for_row(index, state);

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

                            let bytes = node.byte_size(state);

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
                state.borrow_mut().response(Action::new({
                    let definition = self.definition.clone();
                    move |_registry| {
                        let mut definition = definition.borrow_mut();
                        match action {
                            MakeNodeAction::Add(node, offset) => {
                                definition.nodes.insert(offset, RefCell::new(node).into());
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

pub(super) struct StructRowHeightIterator<'instance, 'state, 'state_owner> {
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
            node.byte_size(self.state),
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
