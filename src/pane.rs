use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

use crate::Address;
use crate::process::Process;
use crate::registry::RegistryId;
use crate::{
    State,
    node::{Struct, StructAction, StructUiFlags},
};
use egui::ScrollArea;
use egui_extras::Column;

pub enum Pane {
    AddressStruct {
        address: Weak<RefCell<Address>>,
        r#struct: Weak<RefCell<Struct>>,
    },
    StructList,
    AddressList,
    ProcessList {
        matching: String,
    },
}

pub enum RegistryListResponse {
    Remove(RegistryId),
    PaneResponse(PaneResponse),
}

impl Pane {
    fn registry_list<T, FName, FValue, FResponse>(
        ui: &mut egui::Ui,
        registry_map: &mut HashMap<crate::registry::RegistryId, T>,
        render_name: FName,
        value_width: f32,
        render_value: FValue,
        make_pane_response: FResponse,
        headers: &[&str; 3],
    ) -> Option<RegistryListResponse>
    where
        FName: Fn(&mut egui::Ui, &T),
        FValue: Fn(&mut egui::Ui, &T),
        FResponse: Fn(&T) -> PaneResponse,
    {
        let mut response = None;

        let mut keys: Vec<_> = registry_map.keys().collect();
        keys.sort();

        let max_height = ui.available_height();

        egui_extras::TableBuilder::new(ui)
            .max_scroll_height(max_height)
            .sense(egui::Sense::click())
            .resizable(false)
            .column(Column::auto().at_least(30.0))
            .column(Column::auto().at_least(value_width))
            .column(Column::auto().at_most(160.0))
            .column(Column::remainder())
            .cell_layout(egui::Layout::left_to_right(egui::Align::Center))
            .header(25.0, |mut header| {
                for i in 0..3 {
                    header.col(|ui| {
                        ui.heading(headers[i]);
                    });
                }
            })
            .body(|body| {
                body.rows(20.0, keys.len(), |mut row| {
                    let index = row.index();
                    let key = *keys[index];

                    let (_, r1) = row.col(|ui| {
                        ui.label(format!("{}", key.0));
                    });

                    let value: &T = registry_map.get(&key).unwrap();

                    let (_, r2) = row.col(|ui| {
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            render_value(ui, value);
                        });
                    });

                    let (_, r3) = row.col(|ui| {
                        // TODO(emily): This is a kinda icky hack, because we pass in the registry map as mut here
                        // we cant access it in the callbacks that we pass in. This means that we have to
                        // pass this back out to the caller.
                        // TODO(emily): Could use a similar system to AddressResponse::Action(StructAction)
                        render_name(ui, value);
                    });

                    let (_, r4) = row.col(|ui| {
                        if ui.button("x").clicked() {
                            response = Some(RegistryListResponse::Remove(key));
                        }
                    });

                    // TODO(emily): I remember being able to do intersections of responses but i cant
                    // figure out how to do that now
                    if r1.clicked() || r2.clicked() || r3.clicked() || r4.clicked() {
                        response = Some(RegistryListResponse::PaneResponse(make_pane_response(
                            value,
                        )))
                    }
                });
            });

        response
    }

    pub fn ui(&mut self, ui: &mut egui::Ui, state: &mut State<'_>) -> Option<PaneResponse> {
        match self {
            Pane::AddressStruct {
                r#struct: weak_struct,
                address: weak_address,
            } => {
                let Some((r#struct, address)) = weak_struct.upgrade().zip(weak_address.upgrade())
                else {
                    return Some(PaneResponse::Close);
                };

                let address_name_id = { egui::Id::new("address-name") };

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
                                for (id, other_address) in &state.registry.addresses {
                                    if ui
                                        .button(format!("{} ({id})", other_address.borrow().name()))
                                        .clicked()
                                    {
                                        *weak_address = Rc::downgrade(&other_address.clone());
                                    }
                                }

                                ui.separator();

                                if ui.button("New address").clicked() {
                                    *weak_address =
                                        Rc::downgrade(&state.registry.default_address());
                                }
                            });
                    }
                });

                ui.separator();

                ui.heading("Struct");

                ScrollArea::horizontal()
                    .show(ui, |ui| {
                        let mut response = None;

                        let r = r#struct.borrow().heading(
                            r#struct.clone(),
                            ui,
                            **address.borrow(),
                            state,
                        );

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
                    })
                    .inner
            }
            Pane::AddressList => {
                ui.heading("Addresses");

                ui.separator();

                match Pane::registry_list(
                    ui,
                    &mut state.registry.addresses,
                    |ui, address| {
                        ui.text_edit_singleline(&mut address.borrow_mut().0);
                    },
                    150.0,
                    |ui, address| {
                        ui.scope(|ui| {
                            ui.style_mut().override_text_style = Some(egui::TextStyle::Monospace);
                            ui.label(format!("{:016X}", **address.borrow()));
                        });
                    },
                    |address| PaneResponse::OpenAddress(address.clone()),
                    &["id", "address", "name"],
                ) {
                    Some(RegistryListResponse::Remove(id)) => {
                        state.registry.addresses.remove(&id);
                        None
                    }
                    Some(RegistryListResponse::PaneResponse(pane_response)) => Some(pane_response),
                    None => None,
                }
            }
            Pane::StructList => {
                ui.heading("Structs");

                ui.separator();

                match Pane::registry_list(
                    ui,
                    &mut state.registry.structs,
                    |ui, s| {
                        ui.text_edit_singleline(&mut s.borrow_mut().name);
                    },
                    20.0,
                    |ui, s| {
                        ui.label(format!("{}", s.borrow().byte_size()));
                    },
                    |s| PaneResponse::OpenStruct(s.clone()),
                    &["id", "size", "name"],
                ) {
                    Some(RegistryListResponse::Remove(id)) => {
                        state.registry.structs.remove(&id);
                        None
                    }
                    Some(RegistryListResponse::PaneResponse(response)) => Some(response),
                    _ => None,
                }
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

    pub fn title(&self) -> String {
        match self {
            Pane::AddressStruct { r#struct, address } => {
                let Some((r#struct, address)) = r#struct.upgrade().zip(address.upgrade()) else {
                    return format!("Invalid struct or address");
                };

                format!("{} @ {:016X}", r#struct.borrow().name, **address.borrow())
            }
            Pane::AddressList => "Address list".into(),
            Pane::StructList => "Struct list".into(),
            Pane::ProcessList { matching: _ } => "Process list".into(),
        }
    }
}

#[derive(Debug)]
pub enum PaneResponse {
    AddressStructResponse(AddressResponse),
    // TODO(emily): OpenAddress and OpenStruct can just be AddChild
    OpenAddress(Rc<RefCell<Address>>),
    OpenStruct(Rc<RefCell<Struct>>),
    ProcessSelected(Process),
    AddChild(AddChild),
    Close,
}

#[derive(Debug)]
pub enum AddressResponse {
    AddressStruct(Option<Rc<RefCell<Address>>>, Option<Rc<RefCell<Struct>>>),
    Replace(Rc<RefCell<Struct>>),
    Action(StructAction),
}

#[derive(Debug)]
pub enum AddChild {
    AddressStruct(Option<Rc<RefCell<Struct>>>, Option<Rc<RefCell<Address>>>),
    AddressList,
    StructList,
    ProcessList,
}
