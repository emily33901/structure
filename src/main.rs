use std::{cell::RefCell, collections::HashMap, rc::Rc};

use egui::UiBuilder;
use egui_extras::Column;
use egui_tiles::{Tile, TileId, Tiles};
use memory::Memory;
use node::{Struct, StructAction, StructUiFlags};
use process::{Module, OpenProcess, Process, Section};
use project::{Layout, Project};
use registry::Registry;
use rtti::RttiCache;

mod memory;
mod node;
mod process;
mod project;
mod registry;
mod rtti;
mod storage;

#[derive(Debug)]
struct Address(String, usize);

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
            test_constant_string: &TEST_CONSTANT_STRING,
        }
    }
}

#[derive(Debug)]
enum AddChild {
    AddressStruct(Option<Rc<RefCell<Struct>>>, Option<Rc<RefCell<Address>>>),
    AddressList,
    StructList,
    ProcessList,
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

struct TreeBehavior<'a> {
    options: &'a mut TreeBehaviorOptions,

    state: State<'a>,
}

const PANE_INNER_PAD: f32 = 4.0;

impl<'a> egui_tiles::Behavior<Pane> for TreeBehavior<'a> {
    fn pane_ui(
        &mut self,
        ui: &mut egui::Ui,
        tile_id: egui_tiles::TileId,
        pane: &mut Pane,
    ) -> egui_tiles::UiResponse {
        ui.with_layout(egui::Layout::left_to_right(egui::Align::Min), |ui| {
            ui.add_space(PANE_INNER_PAD);
            ui.with_layout(egui::Layout::top_down(egui::Align::Min), |ui| {
                ui.add_space(PANE_INNER_PAD);

                if let Some(pane_response) = pane.ui(ui, &mut self.state) {
                    self.options.pane_response = Some((tile_id, pane_response))
                }
            });
        });

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

#[derive(Debug)]
enum PaneResponse {
    AddressStructResponse(AddressResponse),
    // TODO(emily): OpenAddress and OpenStruct can just be AddChild
    OpenAddress(Rc<RefCell<Address>>),
    OpenStruct(Rc<RefCell<Struct>>),
    ProcessSelected(Process),
    AddChild(AddChild),
}

enum Pane {
    AddressStruct {
        address: Rc<RefCell<Address>>,
        r#struct: Rc<RefCell<Struct>>,
    },
    StructList,
    AddressList,
    ProcessList {
        matching: String,
    },
}

impl Pane {
    fn registry_list<T, FName, FValue, FResponse>(
        ui: &mut egui::Ui,
        registry_map: &mut HashMap<crate::registry::RegistryId, T>,
        render_name: FName,
        render_value: FValue,
        make_pane_response: FResponse,
        headers: &[&str; 3],
    ) -> (bool, Option<PaneResponse>)
    where
        FName: Fn(&mut egui::Ui, &T) -> bool,
        FValue: Fn(&mut egui::Ui, &T),
        FResponse: Fn(&T) -> PaneResponse,
    {
        let mut response = None;
        let mut registry_dirty = false;

        let keys: Vec<_> = registry_map.keys().collect();

        let max_height = ui.available_height();

        egui_extras::TableBuilder::new(ui)
            .max_scroll_height(max_height)
            .sense(egui::Sense::click())
            .resizable(false)
            .column(Column::auto().at_least(20.0))
            .column(Column::auto().at_least(150.0))
            .column(Column::remainder())
            .header(20.0, |mut header| {
                for i in 0..3 {
                    header.col(|ui| {
                        ui.label(headers[i]);
                    });
                }
            })
            .body(|body| {
                body.rows(15.0, keys.len(), |mut row| {
                    let index = row.index();
                    let key = keys[index];

                    let (_, r1) = row.col(|ui| {
                        ui.label(format!("{}", key.0));
                    });

                    let value: &T = registry_map.get(&key).unwrap();

                    let (_, r2) = row.col(|ui| {
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Min), |ui| {
                            render_value(ui, value);
                        });
                    });

                    let (_, r3) = row.col(|ui| {
                        // TODO(emily): This is a kinda icky hack, because we pass in the registry map as mut here
                        // we cant access it in the callbacks that we pass in. This means that we have to
                        // pass this back out to the caller.
                        // TODO(emily): Could use a similar system to AddressResponse::Action(StructAction)
                        registry_dirty = registry_dirty || render_name(ui, value);
                    });

                    // TODO(emily): I remember being able to do intersections of responses but i cant
                    // figure out how to do that now
                    if r1.clicked() || r2.clicked() || r3.clicked() {
                        response = Some(make_pane_response(value))
                    }
                });
            });

        (registry_dirty, response)
    }

    fn ui(&mut self, ui: &mut egui::Ui, state: &mut State<'_>) -> Option<PaneResponse> {
        match self {
            Pane::AddressStruct { r#struct, address } => {
                let address_name_id = { egui::Id::new(ui.id()).with("address-name") };

                ui.horizontal(|ui| {
                    ui.heading("Address");

                    ui.separator();

                    {
                        let mut address = address.borrow_mut();
                        let address = &mut **address;
                        if ui
                            .add(egui::DragValue::new(address).hexadecimal(8, false, false))
                            .labelled_by(address_name_id)
                            .changed()
                        {
                            if *address == 0 {
                                *address = state.test as *const _ as usize;
                            }
                        }
                    }

                    {
                        let name = { address.borrow().name().to_owned() };
                        egui::ComboBox::new("address-combo-box", "")
                            .selected_text(name)
                            .show_ui(ui, |ui| {
                                for (id, (name, other_address)) in &state.registry.addresses_by_name
                                {
                                    if ui.button(format!("{name} ({id})")).clicked() {
                                        *address = other_address.clone();
                                    }
                                }

                                ui.separator();

                                if ui.button("New address").clicked() {
                                    *address = state.registry.default_address();
                                }
                            });
                    }
                });

                ui.separator();

                ui.heading("Struct");

                let mut response = None;

                let r = r#struct
                    .borrow()
                    .heading(r#struct.clone(), ui, **address.borrow(), state);

                response = response.or(r);

                let (_bytes, r) = r#struct.borrow().ui(
                    r#struct.clone(),
                    StructUiFlags { top_level: true },
                    ui,
                    **address.borrow(),
                    state,
                );

                response = response.or(r);

                response.map(|br| PaneResponse::AddressStructResponse(br))
            }
            Pane::AddressList => {
                ui.heading("Addresses");

                ui.separator();

                let (registry_dirty, response) = Pane::registry_list(
                    ui,
                    &mut state.registry.addresses,
                    |ui, address| {
                        ui.text_edit_singleline(&mut address.borrow_mut().0)
                            .changed()
                    },
                    |ui, address| {
                        ui.scope(|ui| {
                            ui.style_mut().override_text_style = Some(egui::TextStyle::Monospace);
                            ui.label(format!("{:016X}", **address.borrow()));
                        });
                    },
                    |address| PaneResponse::OpenAddress(address.clone()),
                    &["id", "address", "name"],
                );
                if registry_dirty {
                    state.registry.mark_diry();
                }
                response
            }
            Pane::StructList => {
                ui.heading("Structs");

                ui.separator();

                let (registry_dirty, response) = Pane::registry_list(
                    ui,
                    &mut state.registry.structs,
                    |ui, s| ui.text_edit_singleline(&mut s.borrow_mut().name).changed(),
                    |ui, s| {
                        ui.label(format!("{}", s.borrow().byte_size()));
                    },
                    |s| PaneResponse::OpenStruct(s.clone()),
                    &["id", "size", "name"],
                );
                if registry_dirty {
                    state.registry.mark_diry();
                }
                response
            }
            Pane::ProcessList { matching } => {
                ui.horizontal(|ui| {
                    ui.heading("Processes");

                    ui.with_layout(egui::Layout::right_to_left(egui::Align::Min), |ui| {
                        ui.add_space(8.0);
                        ui.add(egui::TextEdit::singleline(matching).hint_text("search"));
                    })
                });

                let mut process_selected = None;

                ui.separator();

                let processes: Vec<_> = state
                    .processes
                    .into_iter()
                    .filter(|p| p.name.contains(matching.as_str()))
                    .collect();

                egui_extras::TableBuilder::new(ui)
                    .sense(egui::Sense::click())
                    .column(Column::auto().at_least(40.0))
                    .column(Column::remainder())
                    .header(25.0, |mut header| {
                        header.col(|ui| {
                            ui.heading("Id");
                        });
                        header.col(|ui| {
                            ui.heading("Name");
                        });
                    })
                    .body(|body| {
                        body.rows(15.0, processes.len(), |mut row| {
                            let process = processes[row.index()];

                            if let Some(active_process) = state.process {
                                row.set_selected(active_process.pid == process.pid);
                            }

                            let mut selected = row
                                .col(|ui| {
                                    ui.add(
                                        egui::Label::new(format!("{}", process.pid))
                                            .selectable(false),
                                    );
                                })
                                .1
                                .clicked();
                            selected = selected
                                || row
                                    .col(|ui| {
                                        ui.add(egui::Label::new(&process.name).selectable(false));
                                    })
                                    .1
                                    .clicked();

                            if selected {
                                process_selected = Some(process);
                            }
                        });
                    });

                process_selected
                    .map(|new_process| PaneResponse::ProcessSelected(new_process.clone()))
            }
        }
    }

    fn title(&self) -> String {
        match self {
            Pane::AddressStruct { r#struct, address } => {
                format!("{} @ {:016X}", r#struct.borrow().name, **address.borrow())
            }
            Pane::AddressList => "Address list".into(),
            Pane::StructList => "Struct list".into(),
            Pane::ProcessList { matching: _ } => "Process list".into(),
        }
    }
}

#[derive(Debug)]
enum AddressResponse {
    AddressStruct(Option<Rc<RefCell<Address>>>, Option<Rc<RefCell<Struct>>>),
    Replace(Rc<RefCell<Struct>>),
    Action(StructAction),
}

struct State<'a> {
    registry: &'a mut Registry,
    memory: &'a mut Memory<'a>,
    sections: &'a [Section],
    modules: &'a [Module],
    rtti: &'a mut RttiCache,
    processes: &'a [Process],
    process: Option<&'a Process>,
    test: &'a Test,
}

struct App {
    open_process: Option<OpenProcess>,
    process: Option<Process>,

    project: Project,
    sections: Option<Vec<Section>>,
    modules: Option<Vec<Module>>,
    rtti: RttiCache,
    processes: Vec<Process>,

    test: Box<Test>,

    tree_options: TreeBehaviorOptions,
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
        }
    }
}

impl App {
    fn new(cc: &eframe::CreationContext) -> Self {
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
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        self.test.value += 1;

        // TODO(emily): Don't need to do these things each frame
        // TODO(emily): Handle these higher up than in process, so that we can make use of Memory
        // and its paging
        {
            if let Some(process) = self.open_process.as_ref() {
                self.modules = process.modules().ok();
                self.sections = process.sections().ok();

                if let Some((modules, sections)) = self.modules.as_ref().zip(self.sections.as_mut())
                {
                    process.module_sections(modules, sections).unwrap();
                }
            }

            self.processes = process::processes().unwrap_or_default();
        }

        self.project.registry.frame();

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
            if let Some(process) = &self.process {
                ui.heading(format!("{} ({})", process.name, process.pid));
            } else {
                ui.heading("No process selected");
            }

            // TODO(emily): At the moment we refresh all pages every frame.
            let mut memory = if let Some(process) = self.open_process.as_ref() {
                Memory::new_process(process)
            } else {
                Memory::new_null()
            };

            let mut behavior = TreeBehavior {
                options: &mut self.tree_options,
                state: State {
                    registry: &mut self.project.registry,
                    memory: &mut memory,
                    sections: self.sections.as_deref().unwrap_or(&[]),
                    modules: self.modules.as_deref().unwrap_or(&[]),
                    rtti: &mut self.rtti,
                    processes: self.processes.as_slice(),
                    process: self.process.as_ref(),
                    test: &self.test,
                },
            };

            let layout = &mut self.project.layout;

            layout.tree.ui(&mut behavior, ui);

            if let Some((from, pane_response)) = behavior.options.pane_response.take() {
                match pane_response {
                    // TODO(emily): We should probably check whether this address is already somewhere
                    // and then open that?
                    PaneResponse::AddressStructResponse(AddressResponse::AddressStruct(
                        address,
                        s,
                    )) => {
                        layout.add_child(
                            &mut self.project.registry,
                            from,
                            AddChild::AddressStruct(s, address),
                        );
                    }
                    PaneResponse::AddressStructResponse(AddressResponse::Replace(new_s)) => {
                        let egui_tiles::Tile::Pane(pane) =
                            self.project.layout.tree.tiles.get_mut(from).unwrap()
                        else {
                            panic!("Only expect AddressStructResponse(AddressResponse::Replace) to come from a Pane")
                        };

                        let Pane::AddressStruct {
                            r#struct: s,
                            address: _address,
                        } = pane
                        else {
                            panic!();
                        };
                        *s = new_s;
                    }
                    PaneResponse::AddressStructResponse(AddressResponse::Action(action)) => {
                        action.call(&mut behavior.state);
                    }
                    PaneResponse::OpenAddress(address) => {
                        layout.add_child(
                            &mut self.project.registry,
                            from,
                            AddChild::AddressStruct(None, Some(address)),
                        );
                    }
                    PaneResponse::OpenStruct(s) => layout.add_child(
                        &mut self.project.registry,
                        from,
                        AddChild::AddressStruct(Some(s), None),
                    ),
                    PaneResponse::ProcessSelected(new_process) => {
                        self.process_changed(new_process);
                    }
                    PaneResponse::AddChild(child) => {
                        layout.add_child(&mut self.project.registry, from, child)
                    }
                }
            }
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
