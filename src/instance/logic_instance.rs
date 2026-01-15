use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use anyhow::{Result, bail};
use egui::{Align, Layout};
use egui::{collapsing_header::CollapsingState, vec2};
use egui_extras::Column;
use rhai::Dynamic;

use crate::{
    State,
    definition::{Logic, Node},
    pane::LogicResponse,
    registry::RegistryId,
    ui,
};

use super::{Location, NodeInstance};

// Logic callbacks are the result of evaluating some script. These are rhai closures.
// that hold state and return nodes, offsets, byte size etc of the struct that we are looking at.

pub struct LogicCallbacks {
    ast: rhai::AST,
    byte_size: rhai::FnPtr,
    nodes: rhai::FnPtr,
}

impl LogicCallbacks {
    pub fn new(
        script: &str,
        address: usize,
        logic_id: RegistryId,
        state: &RefCell<State>,
    ) -> Result<Self> {
        let (ast, callbacks) = {
            let mut state = state.borrow_mut();
            // Need to reborrow fields to satisfy borrow checker
            let State {
                memory,
                scratch_pad,
                script_engine,
                ..
            } = &mut *state;
            match script_engine.compile_logic_script(
                script,
                address,
                *memory,
                logic_id,
                *scratch_pad,
            ) {
                Ok(ok) => ok,
                Err(err) => bail!("failed to evaluate script: {err:#?}"),
            }
        };

        let get_fn = |name: &str| -> anyhow::Result<rhai::FnPtr> {
            let Some(callback) = callbacks.get(name) else {
                bail!("missing callback {name}");
            };

            let Some(fn_ptr) = callback.clone().try_cast::<rhai::FnPtr>() else {
                bail!("map value {name} is not a function pointer");
            };

            Ok(fn_ptr)
        };

        Ok(Self {
            ast,
            byte_size: get_fn("byte_size")?,
            nodes: get_fn("nodes")?,
        })
    }

    fn byte_size(&self, state: &RefCell<State>) -> usize {
        self.byte_size
            .call::<i64>(&state.borrow().script_engine.engine, &self.ast, ())
            .unwrap_or(0) as usize
    }

    fn nodes(&self, state: &RefCell<State>) -> Vec<RhaiLogicNode> {
        let nodes = self
            .nodes
            .call::<Vec<Dynamic>>(&state.borrow().script_engine.engine, &self.ast, ())
            .ok()
            .unwrap_or_default();

        let nodes: Vec<RhaiLogicNode> = nodes.into_iter().map(|dynamic| dynamic.cast()).collect();

        nodes
    }
}

pub struct LogicInstance {
    pub(super) address: usize,
    pub(super) offset_in_parent: usize,
    pub(super) location: Location,
    definition: Rc<RefCell<Logic>>,
    parent_node: Rc<RefCell<Node>>,

    // Script evaluation results
    callbacks: Result<LogicCallbacks>,

    // Within-frame caching
    cached_nodes: RefCell<Option<Vec<(usize, Rc<RefCell<Node>>)>>>,
}

impl LogicInstance {
    pub fn new(
        definition: Rc<RefCell<Logic>>,
        address: usize,
        location: Location,
        offset_in_parent: usize,
        state: &RefCell<State>,
        parent_node: Rc<RefCell<Node>>,
    ) -> Self {
        let logic_id = definition.borrow().id;
        let callbacks = LogicCallbacks::new(&definition.borrow().script, address, logic_id, state);

        Self {
            definition,
            offset_in_parent,
            location,
            address,
            callbacks,
            cached_nodes: Default::default(),
            parent_node,
        }
    }

    fn ui_id(&self) -> egui::Id {
        egui::Id::new(&self.location)
    }

    pub(super) fn collapsing(&self, ctx: &egui::Context) -> CollapsingState {
        let eid = self.ui_id();
        CollapsingState::load_with_default_open(ctx, eid, false)
    }

    fn id(&self) -> RegistryId {
        self.definition.borrow().id
    }

    pub(super) fn name(&self) -> Ref<'_, str> {
        Ref::map(self.definition.borrow(), |definition| {
            definition.name.as_str()
        })
    }

    pub(super) fn definition(&self) -> Rc<RefCell<Logic>> {
        self.definition.clone()
    }

    pub(super) fn evaluate(&self, state: &RefCell<State>) -> Vec<(usize, Rc<RefCell<Node>>)> {
        // TODO(emily): We do a lot of cloning here to fufil the definition of this function.
        // We would probably prefer to borrow from the cached state that we have instead of
        // cloning on every invocation. The clone are cheap however, so this is fine for now.

        let Some(callbacks) = self.callbacks.as_ref().ok() else {
            return vec![];
        };

        if let Some(cached) = self.cached_nodes.borrow().as_ref() {
            return cached.clone();
        }

        let nodes = callbacks.nodes(state);
        let nodes: Vec<(usize, Rc<RefCell<Node>>)> = nodes
            .into_iter()
            .map(|node| node.as_node_offset(state))
            .collect();

        *self.cached_nodes.borrow_mut() = Some(nodes.clone());

        nodes
    }

    pub(super) fn row_heights<'instance, 'state, 'state_owner>(
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
        let Some(callbacks) = self.callbacks.as_ref().ok() else {
            return 0;
        };

        callbacks.byte_size(state)
    }

    pub fn ui(&self, ui: &mut egui::Ui, state: &RefCell<State>) {
        let max_height = ui.available_height();
        let nodes = self.evaluate(state);

        if nodes.is_empty() {
            if let Err(err) = self.callbacks.as_ref() {
                ui.colored_label(egui::Color32::RED, format!("{err:#}"));
            } else {
                ui.colored_label(egui::Color32::YELLOW, "Script returned no nodes");
            }
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

                        let (offset, node) = &nodes[index];

                        row.col(|ui| {
                            let node_instance = NodeInstance::new(
                                self.address + offset,
                                *offset,
                                self.location.progress(*offset),
                                node.clone(),
                            );

                            node_instance.ui(ui, state);
                        });
                    });
                });
        });
    }

    pub(crate) fn heading(&self, ui: &mut egui::Ui, state: &RefCell<State>) {
        ui.allocate_ui_with_layout(
            vec2(ui.available_width(), ui::NODE_UNIT_ROW_HEIGHT),
            Layout::left_to_right(Align::Center),
            |ui| {
                let self_name = self.name();
                let self_id = self.id();

                egui::ComboBox::new((self.ui_id(), "logic-replace-combo-box"), "")
                    .selected_text(&*self_name)
                    .show_ui(ui, |ui| {
                        let mut state = state.borrow_mut();
                        for (id, other_logic) in state.registry.logics.clone() {
                            let other_logic_name = other_logic.borrow().name.clone();
                            if ui
                                .add(
                                    egui::Button::new(format!("{} ({id})", other_logic_name))
                                        .selected(id == self_id),
                                )
                                .clicked()
                            {
                                state.response(LogicResponse::Replace(
                                    Rc::downgrade(&self.parent_node),
                                    Rc::downgrade(&other_logic),
                                ))
                            }
                        }

                        ui.separator();

                        if ui.button("New struct".to_string()).clicked() {
                            let default_logic = state.registry.default_logic();
                            state.response(LogicResponse::Replace(
                                Rc::downgrade(&self.parent_node),
                                Rc::downgrade(&default_logic),
                            ))
                        }
                    });

                ui.end_row();
            },
        );
    }
}

pub(super) struct LogicRowHeightIterator<'instance, 'state, 'state_owner> {
    ctx: egui::Context,
    logic: &'instance LogicInstance,
    nodes: Vec<(usize, Rc<RefCell<Node>>)>,
    state: &'state RefCell<State<'state_owner>>,
    cur_row: usize,
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

        let (offset, node) = &self.nodes[self.cur_row];
        let node_instance = NodeInstance::new(
            self.logic.address + offset,
            *offset,
            self.logic.location.progress(*offset),
            node.clone(),
        );

        let height = node_instance.height(self.item_spacing_y, &self.ctx, self.state);

        self.cur_row += 1;

        Some(height)
    }
}

#[derive(Clone)]
pub enum RhaiLogicNodeKind {
    U64,
    U32,
    U16,
    U8,
    Utf8(usize),
    PointerUtf8(usize),
    Comment(String),
    Pointer(RegistryId),
    Struct(RegistryId),
}

#[derive(Clone)]
pub struct RhaiLogicNode {
    kind: RhaiLogicNodeKind,
    offset: usize,
}

impl RhaiLogicNode {
    fn as_node_offset(&self, state: &RefCell<State>) -> (usize, Rc<RefCell<Node>>) {
        (self.offset, Rc::new(RefCell::new(self.kind.as_node(state))))
    }
}

impl RhaiLogicNodeKind {
    fn as_node(&self, state: &RefCell<State>) -> Node {
        match self {
            RhaiLogicNodeKind::U64 => Node::U64,
            RhaiLogicNodeKind::U32 => Node::U32,
            RhaiLogicNodeKind::U16 => Node::U16,
            RhaiLogicNodeKind::U8 => Node::U8,
            RhaiLogicNodeKind::Utf8(len) => Node::Utf8(*len),
            RhaiLogicNodeKind::PointerUtf8(len) => Node::PointerUtf8(*len),
            RhaiLogicNodeKind::Comment(comment) => Node::Comment(comment.clone()),
            RhaiLogicNodeKind::Pointer(id) => Node::Pointer(
                state
                    .borrow_mut()
                    .registry
                    .find_struct(id)
                    .map(|s| Rc::downgrade(&s))
                    .unwrap_or_default(),
            ),
            RhaiLogicNodeKind::Struct(id) => Node::Struct(
                state
                    .borrow_mut()
                    .registry
                    .find_struct(id)
                    .map(|s| Rc::downgrade(&s))
                    .unwrap_or_default(),
            ),
        }
    }
}

pub mod rhai_logic_node {
    use crate::{
        instance::logic_instance::{RhaiLogicNode, RhaiLogicNodeKind},
        registry::RegistryId,
    };

    pub fn make_u64(offset: i64) -> RhaiLogicNode {
        RhaiLogicNode {
            kind: RhaiLogicNodeKind::U64,
            offset: offset as usize,
        }
    }

    pub fn make_u32(offset: i64) -> RhaiLogicNode {
        RhaiLogicNode {
            kind: RhaiLogicNodeKind::U64,
            offset: offset as usize,
        }
    }

    pub fn make_u16(offset: i64) -> RhaiLogicNode {
        RhaiLogicNode {
            kind: RhaiLogicNodeKind::U64,
            offset: offset as usize,
        }
    }

    pub fn make_u8(offset: i64) -> RhaiLogicNode {
        RhaiLogicNode {
            kind: RhaiLogicNodeKind::U64,
            offset: offset as usize,
        }
    }

    pub fn make_comment(offset: i64, comment: String) -> RhaiLogicNode {
        RhaiLogicNode {
            kind: RhaiLogicNodeKind::Comment(comment),
            offset: offset as usize,
        }
    }

    pub fn make_pointer(offset: i64, id: i64) -> RhaiLogicNode {
        RhaiLogicNode {
            kind: RhaiLogicNodeKind::Pointer(RegistryId(id as usize)),
            offset: offset as usize,
        }
    }

    pub fn make_struct(offset: i64, id: i64) -> RhaiLogicNode {
        RhaiLogicNode {
            kind: RhaiLogicNodeKind::Struct(RegistryId(id as usize)),
            offset: offset as usize,
        }
    }

    pub fn make_utf8(offset: i64, len: i64) -> RhaiLogicNode {
        RhaiLogicNode {
            kind: RhaiLogicNodeKind::Utf8(len as usize),
            offset: offset as usize,
        }
    }

    pub fn make_pointer_utf8(offset: i64, len: i64) -> RhaiLogicNode {
        RhaiLogicNode {
            kind: RhaiLogicNodeKind::PointerUtf8(len as usize),
            offset: offset as usize,
        }
    }
}
