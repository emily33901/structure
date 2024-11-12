use egui_tiles::{Container, Tiles};

use crate::{registry::Registry, Pane};

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
                    tiles.insert_pane(Pane::ProcessList { search: "".into() }),
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
