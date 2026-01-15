use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use egui::{Align, Layout};
use egui::collapsing_header::CollapsingState;
use egui_extras::Column;

use crate::{
    State,
    definition::{Logic, Node},
    registry::RegistryId,
};

use super::{Location, NodeInstance};

pub struct LogicCallbacks {
    node_count: rhai::FnPtr,
    byte_size: rhai::FnPtr,
    get_node: rhai::FnPtr,
    get_offset: rhai::FnPtr,
}

impl LogicCallbacks {
    pub fn from_map(map: &rhai::Map) -> Result<Self, Box<rhai::EvalAltResult>> {
        let get_fn = |name: &str| -> Result<rhai::FnPtr, Box<rhai::EvalAltResult>> {
            map.get(name)
                .ok_or_else(|| format!("Missing callback: {}", name))?
                .clone()
                .try_cast::<rhai::FnPtr>()
                .ok_or_else(|| format!("{} is not a function", name).into())
        };

        Ok(Self {
            node_count: get_fn("node_count")?,
            byte_size: get_fn("byte_size")?,
            get_node: get_fn("get_node")?,
            get_offset: get_fn("get_offset")?,
        })
    }
}

pub struct LogicInstance {
    pub(super) address: usize,
    pub(super) offset_in_parent: usize,
    pub(super) location: Location,
    definition: Rc<RefCell<Logic>>,

    // Script evaluation results
    callbacks: LogicCallbacks,
    ast: rhai::AST,

    // Within-frame caching
    cached_nodes: RefCell<Option<Vec<Rc<RefCell<Node>>>>>,
    cached_offsets: RefCell<Option<Vec<usize>>>,
}

impl LogicInstance {
    pub fn new(
        definition: Rc<RefCell<Logic>>,
        address: usize,
        location: Location,
        offset_in_parent: usize,
        state: &RefCell<State>,
    ) -> Self {
        let script = definition.borrow().script.clone();
        let (ast, callbacks) = {
            let mut state_ref = state.borrow_mut();
            state_ref
                .script_engine
                .compile_logic_script(&script, address, state_ref.memory)
                .expect("Failed to compile logic script")
        };

        Self {
            definition,
            offset_in_parent,
            location,
            address,
            callbacks,
            ast,
            cached_nodes: Default::default(),
            cached_offsets: Default::default(),
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

    // Callback invocation methods
    fn call_node_count(&self, state: &RefCell<State>) -> usize {
        self.callbacks
            .node_count
            .call::<i64>(&state.borrow().script_engine.engine, &self.ast, ())
            .unwrap_or(0) as usize
    }

    fn call_byte_size(&self, state: &RefCell<State>) -> usize {
        self.callbacks
            .byte_size
            .call::<i64>(&state.borrow().script_engine.engine, &self.ast, ())
            .unwrap_or(0) as usize
    }

    fn call_get_node(&self, state: &RefCell<State>, index: usize) -> Option<Node> {
        let node_str: String = self
            .callbacks
            .get_node
            .call(&state.borrow().script_engine.engine, &self.ast, (index as i64,))
            .ok()?;
        crate::script::parse_node_result(&node_str)
    }

    fn call_get_offset(&self, state: &RefCell<State>, index: usize) -> usize {
        self.callbacks
            .get_offset
            .call::<i64>(&state.borrow().script_engine.engine, &self.ast, (index as i64,))
            .unwrap_or(0) as usize
    }

    pub(super) fn evaluate(&self, state: &RefCell<State>) -> Vec<Rc<RefCell<Node>>> {
        if let Some(cached) = self.cached_nodes.borrow().as_ref() {
            return cached.clone();
        }

        let node_count = self.call_node_count(state);

        let mut nodes = Vec::with_capacity(node_count);
        let mut offsets = Vec::with_capacity(node_count);

        for i in 0..node_count {
            if let Some(node) = self.call_get_node(state, i) {
                offsets.push(self.call_get_offset(state, i));
                nodes.push(Rc::new(RefCell::new(node)));
            }
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
        self.call_byte_size(state)
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
