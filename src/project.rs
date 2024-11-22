use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

use egui_tiles::{Container, TileId, Tiles};

use crate::{node::Struct, registry::Registry, AddChild, Pane, State};

pub(crate) enum Collapsed {
    Collapsed,
    Expanded,
}

pub(crate) struct Layout {
    pub(crate) tree: egui_tiles::Tree<crate::Pane>,
}

impl Layout {
    pub(crate) fn new(default_pane: Pane) -> Self {
        Self {
            tree: {
                let mut tiles = Tiles::default();
                let default_struct_pane = tiles.insert_pane(default_pane);

                let left_hand_lists = vec![
                    tiles.insert_pane(Pane::AddressList),
                    tiles.insert_pane(Pane::StructList),
                    tiles.insert_pane(Pane::ProcessList {
                        matching: "".into(),
                    }),
                ];

                let lists_container = Container::new_vertical(left_hand_lists);
                let tile = tiles.insert_container(lists_container);

                let root_tile = tiles.insert_container(egui_tiles::Linear::new_binary(
                    egui_tiles::LinearDir::Horizontal,
                    [tile, default_struct_pane],
                    0.1,
                ));

                egui_tiles::Tree::new("view_tree", root_tile, tiles)
            },
        }
    }

    pub(crate) fn add_child(
        &mut self,
        registry: &mut Registry,
        parent: TileId,
        add_child: AddChild,
    ) {
        let new_child = self.tree.tiles.insert_pane(match add_child {
            AddChild::AddressStruct(s, address) => {
                let s = s.unwrap_or_else(|| registry.default_struct());
                let address = address.unwrap_or_else(|| registry.default_address());

                Pane::AddressStruct {
                    r#struct: s,
                    address,
                }
            }
            AddChild::AddressList => Pane::AddressList,
            AddChild::StructList => Pane::StructList,
            AddChild::ProcessList => Pane::ProcessList {
                matching: "".into(),
            },
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
                if let Some(egui_tiles::Tile::Container(egui_tiles::Container::Tabs(tabs))) =
                    self.tree.tiles.get_mut(parent)
                {
                    tabs.add_child(new_child);
                    tabs.set_active(new_child);
                }
                cur = parent
            }
        }
    }
}

pub(crate) struct Project {
    pub(crate) registry: Registry,
    pub(crate) layout: Layout,
}

impl Project {
    pub(crate) fn new(layout: Layout, registry: Registry) -> Self {
        Self { layout, registry }
    }
}
