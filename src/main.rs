use std::{
    cell::RefCell,
    collections::HashMap,
    ops::{Add, DerefMut},
    rc::{Rc, Weak},
    str::FromStr,
};

use egui_extras::Column;
use egui_tiles::{Container, Tile, TileId, Tiles};
use memory::Memory;
use node::{Struct, StructAction, StructUiFlags};
use process::{Module, Process, Section};
use rand::{distributions::Alphanumeric, Rng};
use registry::Registry;

mod memory;
mod node;
mod process;
mod registry;

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

struct Test {
    value: u64,
    test: String,
}

impl Default for Test {
    fn default() -> Self {
        Self {
            value: 0,
            test: "Nice test string".into(),
        }
    }
}

enum AddChild {
    AddressStruct(Option<Rc<RefCell<Struct>>>, Option<Rc<RefCell<Address>>>),
    AddressList,
    StructList,
}

struct TreeBehaviorOptions {
    simplification_options: egui_tiles::SimplificationOptions,
    tab_bar_height: f32,
    gap_width: f32,
    add_child: Option<(egui_tiles::TileId, AddChild)>,
}

impl Default for TreeBehaviorOptions {
    fn default() -> Self {
        Self {
            simplification_options: egui_tiles::SimplificationOptions {
                all_panes_must_have_tabs: true,
                ..Default::default()
            },
            tab_bar_height: 24.0,
            gap_width: 8.0,
            add_child: None,
        }
    }
}

struct TreeBehavior<'a> {
    options: &'a mut TreeBehaviorOptions,

    process: &'a Process,
    registry: &'a mut crate::registry::Registry,
    memory: Memory<'a>,
    sections: Option<&'a [Section]>,
    test: &'a Test,
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

                match pane.ui(
                    ui,
                    self.registry,
                    &mut self.memory,
                    self.sections,
                    self.test,
                ) {
                    // TODO(emily): We should probably check whether this address is already somewhere
                    // and then open that?
                    Some(PaneResponse::AddressStructResponse(AddressResponse::AddressStruct(
                        address,
                        s,
                    ))) => {
                        self.options.add_child =
                            Some((tile_id, AddChild::AddressStruct(s, address)))
                    }
                    Some(PaneResponse::AddressStructResponse(AddressResponse::Replace(new_s))) => {
                        let Pane::AddressStruct(s, _address) = pane else {
                            panic!();
                        };
                        *s = new_s;
                    }
                    Some(PaneResponse::AddressStructResponse(AddressResponse::Action(action))) => {
                        action.call(self.registry);
                    }

                    Some(PaneResponse::OpenAddress(address)) => {
                        self.options.add_child =
                            Some((tile_id, AddChild::AddressStruct(None, Some(address))))
                    }

                    Some(PaneResponse::OpenStruct(s)) => {
                        self.options.add_child =
                            Some((tile_id, AddChild::AddressStruct(Some(s), None)))
                    }

                    Some(r) => {
                        unreachable!();
                    }

                    None => {}
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

        if r.clicked() {
            self.options.add_child = Some((tile_id, AddChild::AddressStruct(None, None)));
        }

        r.context_menu(|ui| {
            if ui.button("Address").clicked() {
                self.options.add_child = Some((tile_id, AddChild::AddressStruct(None, None)))
            }
            if ui.button("Address list").clicked() {
                self.options.add_child = Some((tile_id, AddChild::AddressList))
            }

            if ui.button("Struct list").clicked() {
                self.options.add_child = Some((tile_id, AddChild::StructList))
            }
        });
    }
}

#[derive(Debug)]
enum PaneResponse {
    AddressStructResponse(AddressResponse),
    OpenAddress(Rc<RefCell<Address>>),
    OpenStruct(Rc<RefCell<Struct>>),
}

enum Pane {
    AddressStruct(Rc<RefCell<Struct>>, Rc<RefCell<Address>>),
    StructList,
    AddressList,
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
            .column(Column::auto().at_least(10.0))
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

    fn ui(
        &mut self,
        ui: &mut egui::Ui,
        registry: &mut crate::registry::Registry,
        memory: &mut Memory<'_>,
        sections: Option<&[Section]>,
        test: &Test,
    ) -> Option<PaneResponse> {
        match self {
            Pane::AddressStruct(r#struct, address) => {
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
                                *address = test as *const _ as usize;
                            }
                        }
                    }

                    {
                        let mut address = address.borrow_mut();
                        let text_edit =
                            egui::TextEdit::singleline(address.name_mut()).id(address_name_id);
                        ui.add(text_edit);
                    }
                });

                ui.separator();

                ui.heading("Struct");

                let (bytes, r) = r#struct.borrow().ui(
                    r#struct.clone(),
                    StructUiFlags { top_level: true },
                    ui,
                    registry,
                    memory,
                    **address.borrow(),
                    sections,
                    test,
                );

                r.map(|br| PaneResponse::AddressStructResponse(br))
            }
            Pane::AddressList => {
                let (registry_dirty, response) = Pane::registry_list(
                    ui,
                    &mut registry.addresses,
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
                    registry.mark_diry();
                }
                response
            }
            Pane::StructList => {
                let (registry_dirty, response) = Pane::registry_list(
                    ui,
                    &mut registry.structs,
                    |ui, s| ui.text_edit_singleline(&mut s.borrow_mut().name).changed(),
                    |ui, s| {
                        ui.label(format!("{}", s.borrow().byte_size()));
                    },
                    |s| PaneResponse::OpenStruct(s.clone()),
                    &["id", "size", "name"],
                );
                if registry_dirty {
                    registry.mark_diry();
                }
                response
            }
        }
    }

    fn title(&self) -> String {
        match self {
            Pane::AddressStruct(r#struct, address) => {
                format!("{} @ {:016X}", r#struct.borrow().name, **address.borrow())
            }
            Pane::AddressList => "Address list".into(),
            Pane::StructList => "Struct list".into(),
        }
    }
}

#[derive(Debug)]
enum AddressResponse {
    AddressStruct(Option<Rc<RefCell<Address>>>, Option<Rc<RefCell<Struct>>>),
    Replace(Rc<RefCell<Struct>>),
    Action(StructAction),
}
struct App {
    pid_str: String,
    pid: Option<u32>,
    process: Option<Process>,

    registry: crate::registry::Registry,
    sections: Option<Vec<Section>>,
    modules: Option<Vec<Module>>,

    test: Box<Test>,

    tree_options: TreeBehaviorOptions,
    tree: egui_tiles::Tree<Pane>,
}

impl Default for App {
    fn default() -> Self {
        let mut registry = Registry::default();
        let default_pane = registry.default_pane();

        Self {
            pid_str: Default::default(),
            pid: Default::default(),
            process: Default::default(),
            registry: registry,
            sections: Default::default(),
            modules: Default::default(),
            test: Default::default(),
            tree_options: Default::default(),
            tree: {
                let mut tiles = Tiles::default();
                let default_struct_pane = tiles.insert_pane(default_pane);

                let left_hand_lists = vec![
                    tiles.insert_pane(Pane::AddressList),
                    tiles.insert_pane(Pane::StructList),
                ];

                let lists_container = Container::new_vertical(left_hand_lists);
                let tile = tiles.insert_container(lists_container);

                let root_tile = tiles
                    .insert_container(Container::new_horizontal(vec![tile, default_struct_pane]));

                egui_tiles::Tree::new("view_tree", root_tile, tiles)
            },
        }
    }
}

impl App {
    fn new(cc: &eframe::CreationContext) -> Self {
        eprintln!("darwin");
        let mut zelf = Self {
            pid: None,
            ..Default::default()
        };

        zelf.pid_changed(std::process::id());

        zelf
    }

    fn update_pid_from_string(&mut self) {
        if let Ok(new_pid) = u32::from_str(&self.pid_str) {
            self.pid_changed(new_pid);
        }
    }

    fn pid_changed(&mut self, new_pid: u32) {
        self.pid_str = new_pid.to_string();

        if let Some(existing_pid) = self.pid.as_mut() {
            if *existing_pid == new_pid {
                return;
            }
        }

        self.pid = Some(new_pid);

        self.process = match Process::new(new_pid) {
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

        if let Some(process) = self.process.as_ref() {
            self.modules = process.modules().ok();
            self.sections = process.sections().ok();
        }

        self.registry.frame();

        egui::CentralPanel::default().show(ctx, |ui| {
            // The central panel the region left after adding TopPanel's and SidePanel's
            ui.heading("Darwin");
            ui.separator();

            if ui.text_edit_singleline(&mut self.pid_str).changed() {
                self.update_pid_from_string();
            }

            if let Some(process) = self.process.as_ref() {
                let mut behavior = TreeBehavior {
                    options: &mut self.tree_options,
                    memory: Memory::new(process),
                    process,
                    sections: self.sections.as_ref().map(|x| x.as_slice()),
                    test: &self.test,
                    registry: &mut self.registry,
                };

                self.tree.ui(&mut behavior, ui);

                if let Some((parent, add_child)) = behavior.options.add_child.take() {
                    // TODO(emily): We shouldn't do this here

                    let new_child = self.tree.tiles.insert_pane(match add_child {
                        AddChild::AddressStruct(s, address) => {
                            let s = s.unwrap_or(self.registry.default_struct());
                            let address = address.unwrap_or(self.registry.default_address());

                            Pane::AddressStruct(s, address)
                        }
                        AddChild::AddressList => Pane::AddressList,
                        AddChild::StructList => Pane::StructList,
                    });

                    // Find some parent tabs to insert this into
                    let mut cur = parent;

                    if let Some(egui_tiles::Tile::Container(egui_tiles::Container::Tabs(tabs))) =
                        self.tree.tiles.get_mut(parent)
                    {
                        tabs.add_child(new_child);
                        tabs.set_active(new_child);
                    } else {
                        // TODO(emily): Icky copy paste
                        while let Some(parent) = self.tree.tiles.parent_of(cur) {
                            if let Some(egui_tiles::Tile::Container(egui_tiles::Container::Tabs(
                                tabs,
                            ))) = self.tree.tiles.get_mut(parent)
                            {
                                tabs.add_child(new_child);
                                tabs.set_active(new_child);
                            }
                            cur = parent
                        }
                    }
                }
            }
        });
    }
}

fn main() {
    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "Darwin Memory Disector",
        native_options,
        Box::new(|cc| Ok(Box::new(App::new(cc)))),
    )
    .unwrap();
}
