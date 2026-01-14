use std::{cell::RefCell, rc::Rc};

use egui::{Theme, vec2};
use egui_tiles::{Tile, TileId, Tiles};
use memory::Memory;
use pe::{Module, Section};
use process::{OpenProcess, Process};
use project::{Layout, Project};
use registry::Registry;
use rtti::RttiCache;
use script::ScriptEngine;

use crate::pane::{AddChild, AddressResponse, Pane, PaneResponse};

pub mod definition;
pub mod instance;
mod memory;
mod node;
pub mod pane;
mod pe;
mod process;
mod project;
mod registry;
mod rtti;
mod script;
mod storage;
pub mod ui;

#[derive(Debug)]
pub struct Address(String, usize);

impl Default for Address {
    fn default() -> Self {
        Self("Default address".into(), Default::default())
    }
}

impl From<usize> for Address {
    fn from(value: usize) -> Self {
        Self("Default address".into(), value)
    }
}

impl std::ops::Deref for Address {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl std::ops::DerefMut for Address {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.1
    }
}

impl Address {
    fn name(&self) -> &str {
        &self.0
    }

    fn name_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

const TEST_CONSTANT_STRING: &str = "Nice data section string";

struct Test {
    value: u64,
    test: String,
    test_constant_string: &'static str,
}

impl Default for Test {
    fn default() -> Self {
        Self {
            value: 0,
            test: "Nice test string".into(),
            test_constant_string: TEST_CONSTANT_STRING,
        }
    }
}

struct TreeBehaviorOptions {
    simplification_options: egui_tiles::SimplificationOptions,
    tab_bar_height: f32,
    gap_width: f32,
    pane_response: Option<(TileId, PaneResponse)>,
}

impl Default for TreeBehaviorOptions {
    fn default() -> Self {
        Self {
            simplification_options: egui_tiles::SimplificationOptions {
                all_panes_must_have_tabs: true,
                ..Default::default()
            },
            tab_bar_height: 24.0,
            gap_width: 4.0,
            pane_response: None,
        }
    }
}

struct TreeBehavior<'a, 'b> {
    options: &'a mut TreeBehaviorOptions,
    state: &'a RefCell<State<'b>>,
}

const PANE_INNER_PAD: f32 = 4.0;

impl<'a, 'b> egui_tiles::Behavior<Pane> for TreeBehavior<'a, 'b> {
    fn pane_ui(
        &mut self,
        ui: &mut egui::Ui,
        tile_id: egui_tiles::TileId,
        pane: &mut Pane,
    ) -> egui_tiles::UiResponse {
        self.state.borrow_mut().this_frame_mut().current_tile_id = Some(tile_id);

        ui.allocate_ui_with_layout(
            vec2(ui.available_width() - PANE_INNER_PAD, ui.available_height()),
            egui::Layout::left_to_right(egui::Align::Min),
            |ui| {
                ui.add_space(PANE_INNER_PAD);

                ui.allocate_ui_with_layout(
                    vec2(ui.available_width(), ui.available_height() - PANE_INNER_PAD),
                    egui::Layout::top_down(egui::Align::Min),
                    |ui| {
                        ui.add_space(PANE_INNER_PAD);

                        ui.push_id(tile_id, |ui| {
                            pane.ui(ui, self.state);
                        });
                    },
                );
            },
        );

        egui_tiles::UiResponse::None
    }

    fn tab_title_for_pane(&mut self, pane: &Pane) -> egui::WidgetText {
        egui::RichText::new(pane.title()).into()
    }

    fn tab_bar_height(&self, _style: &egui::Style) -> f32 {
        self.options.tab_bar_height
    }

    fn gap_width(&self, _style: &egui::Style) -> f32 {
        self.options.gap_width
    }

    fn simplification_options(&self) -> egui_tiles::SimplificationOptions {
        self.options.simplification_options
    }

    fn is_tab_closable(&self, _tiles: &Tiles<Pane>, _tile_id: TileId) -> bool {
        true
    }

    fn on_tab_close(
        &mut self,
        tiles: &mut egui_tiles::Tiles<Pane>,
        tile_id: egui_tiles::TileId,
    ) -> bool {
        if let Some(tile) = tiles.get(tile_id) {
            match tile {
                Tile::Pane(pane) => {
                    // Single pane removal
                    let tab_title = self.tab_title_for_pane(pane);
                    eprintln!("Closing tab: {}, tile ID: {tile_id:?}", tab_title.text());
                }
                Tile::Container(container) => {
                    // Container removal
                    eprintln!("Closing container: {:?}", container.kind());
                    let children_ids = container.children();
                    for child_id in children_ids {
                        if let Some(Tile::Pane(pane)) = tiles.get(*child_id) {
                            let tab_title = self.tab_title_for_pane(pane);
                            eprintln!("Closing tab: {}, tile ID: {tile_id:?}", tab_title.text());
                        }
                    }
                }
            }
        }

        // Proceed to removing the tab
        true
    }

    fn top_bar_right_ui(
        &mut self,
        _tiles: &egui_tiles::Tiles<Pane>,
        ui: &mut egui::Ui,
        tile_id: egui_tiles::TileId,
        _tabs: &egui_tiles::Tabs,
        _scroll_offset: &mut f32,
    ) {
        let r = ui.button("➕");

        let mut response = None;
        if r.clicked() {
            response = Some(AddChild::AddressStruct(None, None));
        }

        r.context_menu(|ui| {
            if ui.button("Address").clicked() {
                response = Some(AddChild::AddressStruct(None, None));
            }
            if ui.button("Address list").clicked() {
                response = Some(AddChild::AddressList);
            }
            if ui.button("Struct list").clicked() {
                response = Some(AddChild::StructList);
            }
            if ui.button("Process list").clicked() {
                response = Some(AddChild::ProcessList);
            }
        });

        if let Some(add_child) = response {
            self.options.pane_response = Some((tile_id, PaneResponse::AddChild(add_child)));
        }
    }
}

#[derive(Default)]
pub struct FrameState {
    // TODO(emily): This is hacky
    current_tile_id: Option<TileId>,
    highlighted_address: Option<usize>,
    response: Option<(TileId, PaneResponse)>,
}

impl FrameState {
    fn response(&mut self, new_response: impl Into<PaneResponse>) {
        let tile_id = self
            .current_tile_id
            .expect("trying to set a response but not rendering a pane?");

        self.response = Some((tile_id, new_response.into()))
    }
}

pub struct State<'a> {
    registry: &'a mut Registry,
    memory: &'a mut Memory<'a>,
    sections: &'a [Section],
    modules: &'a [Module],
    rtti: &'a mut RttiCache,
    script_engine: &'a ScriptEngine,
    processes: &'a [Process],
    process: Option<&'a Process>,

    this_frame: Option<FrameState>,
    last_frame: &'a FrameState,

    test: &'a Test,
}

impl<'a> State<'a> {
    fn this_frame_mut(&mut self) -> &mut FrameState {
        self.this_frame.as_mut().unwrap()
    }

    fn response(&mut self, new_response: impl Into<PaneResponse>) {
        self.this_frame_mut().response(new_response);
    }

    fn take_this_frame(&mut self) -> FrameState {
        self.this_frame.take().unwrap()
    }
}

struct App {
    open_process: Option<OpenProcess>,
    process: Option<Process>,

    project: Project,
    sections: Option<Vec<Section>>,
    modules: Option<Vec<Module>>,
    rtti: RttiCache,
    script_engine: ScriptEngine,
    processes: Vec<Process>,

    test: Box<Test>,

    tree_options: TreeBehaviorOptions,

    this_frame: FrameState,
}

impl Default for App {
    fn default() -> Self {
        let mut registry = Registry::default();
        let default_pane = registry.default_pane();

        let project = Project::new(Layout::new(default_pane), registry);

        Self {
            open_process: Default::default(),
            process: Default::default(),
            project,
            sections: Default::default(),
            modules: Default::default(),
            test: Default::default(),
            tree_options: Default::default(),
            processes: Default::default(),
            rtti: Default::default(),
            script_engine: Default::default(),
            this_frame: Default::default(),
        }
    }
}

impl App {
    fn new(cc: &eframe::CreationContext) -> Self {
        cc.egui_ctx.set_theme(Theme::Dark);

        #[cfg(debug_assertions)]
        cc.egui_ctx.style_mut(|style| {
            style.debug.debug_on_hover = true;
            style.debug.hover_shows_next = true;
        });

        // cc.egui_ctx.set_debug_on_hover(true);

        let mut fonts = egui::FontDefinitions::default();
        fonts.font_data.insert(
            "Geist".to_owned(),
            egui::FontData::from_static(include_bytes!("../resource/Geist-Light.ttf")),
        );

        fonts.font_data.insert(
            "NotoSansMono".to_owned(),
            egui::FontData::from_static(include_bytes!("../resource/NotoSansMono-Regular.ttf")),
        );

        fonts
            .families
            .entry(egui::FontFamily::Proportional)
            .or_default()
            .insert(0, "Geist".to_owned());

        fonts
            .families
            .entry(egui::FontFamily::Monospace)
            .or_default()
            .insert(0, "NotoSansMono".to_owned());

        cc.egui_ctx.set_fonts(fonts);

        eprintln!("structure");
        Self::default()
    }

    fn process_changed(&mut self, new_process: Process) {
        let pid = new_process.pid;
        self.process = Some(new_process);

        self.open_process = match OpenProcess::new(pid) {
            Ok(process) => Some(process),
            Err(err) => {
                eprintln!("Cannot open process {err}");
                return;
            }
        };
    }

    fn handle_response(
        state: &RefCell<State>,
        layout: &mut Layout,
        this_frame: &mut FrameState,
        unhandled_response: &mut Option<(TileId, PaneResponse)>,
    ) {
        if let Some((from, response)) = this_frame.response.take() {
            match response {
                // TODO(emily): We should probably check whether this address is already somewhere
                // and then open that?
                PaneResponse::AddressStructResponse(AddressResponse::AddressStruct(address, s)) => {
                    layout.add_child(
                        state.borrow_mut().registry,
                        from,
                        AddChild::AddressStruct(s, address),
                    );
                }
                PaneResponse::AddressStructResponse(AddressResponse::Replace(new_s)) => {
                    *unhandled_response = Some((from, PaneResponse::AddressStructResponse(
                        AddressResponse::Replace(new_s),
                    )));
                }
                PaneResponse::AddressStructResponse(AddressResponse::Action(action)) => {
                    action.call(state);
                }
                PaneResponse::OpenAddress(address) => {
                    layout.add_child(
                        state.borrow_mut().registry,
                        from,
                        AddChild::AddressStruct(None, Some(address)),
                    );
                }
                PaneResponse::OpenStruct(s) => layout.add_child(
                    state.borrow_mut().registry,
                    from,
                    AddChild::AddressStruct(Some(s), None),
                ),
                PaneResponse::ProcessSelected(new_process) => {
                    *unhandled_response = Some((from, PaneResponse::ProcessSelected(new_process)))
                }
                PaneResponse::AddChild(child) => {
                    layout.add_child(state.borrow_mut().registry, from, child)
                }
                PaneResponse::Close => {
                    eprintln!("Ignoring close");
                }
            }
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.test.value += 1;

        egui::TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Save").clicked() {
                        storage::save_to_disk(&self.project, std::path::Path::new("project.json"))
                            .unwrap();
                    }
                    if ui.button("Load").clicked() {
                        self.project =
                            storage::load_from_disk(std::path::Path::new("project.json")).unwrap();
                    }
                })
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            // TODO(emily): At the moment we refresh all pages every frame.
            let mut memory = if let Some(process) = self.open_process.as_ref() {
                Memory::new_process(process)
            } else {
                Memory::new_null()
            };

            // TODO(emily): Don't need to do these things each frame
            // TODO(emily): Handle these higher up than in process, so that we can make use of Memory
            // and its paging
            {
                if let Some(_process) = self.open_process.as_ref() {
                    self.modules = pe::modules(&mut memory).ok();
                    self.sections = pe::sections(&mut memory).ok();

                    if let Some((modules, sections)) =
                        self.modules.as_ref().zip(self.sections.as_mut())
                    {
                        pe::module_sections(&mut memory, modules, sections).unwrap();
                    }
                }

                self.processes = process::processes().unwrap_or_default();
            }

            if let Some(process) = &self.process {
                ui.heading(format!("{} ({})", process.name, process.pid));
            } else {
                ui.heading("No process selected");
            }

            let state = RefCell::new(State {
                registry: &mut self.project.registry,
                memory: &mut memory,
                sections: self.sections.as_deref().unwrap_or(&[]),
                modules: self.modules.as_deref().unwrap_or(&[]),
                rtti: &mut self.rtti,
                script_engine: &self.script_engine,
                processes: self.processes.as_slice(),
                process: self.process.as_ref(),

                test: &self.test,
                last_frame: &self.this_frame,
                this_frame: Some(Default::default()),
            });

            let mut behavior = TreeBehavior {
                options: &mut self.tree_options,
                state: &state,
            };

            let layout = &mut self.project.layout;

            layout.tree.ui(&mut behavior, ui);

            drop(behavior);

            let mut this_frame = state.borrow_mut().take_this_frame();

            // TODO(emily): Hacky but in order to update our process we need to drop state
            // so that we have access to &mut self again. Screams of bad design.
            let mut unhandled_response = None;
            Self::handle_response(&state, layout, &mut this_frame, &mut unhandled_response);

            drop(state);

            match unhandled_response {
                Some((_from, PaneResponse::ProcessSelected(new_process))) => {
                    self.process_changed(new_process);
                } 
                Some((from, PaneResponse::AddressStructResponse(AddressResponse::Replace(new_s)))) => {
                    let egui_tiles::Tile::Pane(pane) =
                        self.project.layout.tree.tiles.get_mut(from).unwrap()
                    else {
                        panic!(
                            "Only expect AddressStructResponse(AddressResponse::Replace) to come from a Pane"
                        )
                    };

                    let Pane::AddressStruct {
                        r#struct: s,
                        address: _address,
                    } = pane
                    else {
                        panic!();
                    };

                    *s = Rc::downgrade(&new_s);
                }
                None => {}
                x => todo!("only expect unhandled process selected response but got {x:?}"),
            }

            self.this_frame = this_frame;
        });
    }
}

fn main() {
    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "Structure memory dissector",
        native_options,
        Box::new(|cc| Ok(Box::new(App::new(cc)))),
    )
    .unwrap();
}
