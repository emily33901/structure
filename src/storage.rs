use serde::{Deserialize, Serialize};

use crate::project::Project;

use anyhow::Result;

#[derive(Serialize, Deserialize)]
enum Storage {
    V1(v1::Project),
}

mod v1 {
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    use serde::{Deserialize, Serialize};

    use crate::registry::RegistryId;

    #[derive(Serialize, Deserialize)]
    pub(super) enum Node {
        U8,
        U16,
        U32,
        U64,
        Struct(RegistryId),
        Pointer(RegistryId),
    }

    impl Node {
        fn make_real(&self, registry: &crate::registry::Registry) -> crate::node::Node {
            match self {
                Node::U8 => crate::node::Node::U8,
                Node::U16 => crate::node::Node::U16,
                Node::U32 => crate::node::Node::U32,
                Node::U64 => crate::node::Node::U64,
                Node::Struct(registry_id) => crate::node::Node::Struct(Rc::downgrade(
                    registry.structs.get(registry_id).unwrap(),
                )),
                Node::Pointer(registry_id) => crate::node::Node::Pointer(Rc::downgrade(
                    registry.structs.get(registry_id).unwrap(),
                )),
            }
        }
    }

    #[derive(Serialize, Deserialize)]
    pub(super) struct Struct {
        pub(super) row_count: usize,
        pub(super) name: String,
        pub(super) nodes: HashMap<usize, Node>,
    }

    #[derive(Serialize, Deserialize)]
    pub(super) struct Address(pub(super) String, pub(super) usize);

    #[derive(Serialize, Deserialize)]
    pub(super) struct Registry {
        pub(super) next_id: usize,
        pub(super) structs: HashMap<RegistryId, Struct>,
        pub(super) addresses: HashMap<RegistryId, Address>,
    }

    #[derive(Serialize, Deserialize)]
    pub(super) enum Pane {
        AddressStruct {
            address: RegistryId,
            r#struct: RegistryId,
        },
        StructList,
        AddressList,
        ProcessList {
            matching: String,
        },
    }

    #[derive(Serialize, Deserialize)]
    pub(super) struct Layout {
        pub(super) tree: egui_tiles::Tree<Pane>,
    }

    #[derive(Serialize, Deserialize)]
    pub(super) struct Project {
        pub(super) registry: Registry,
        pub(super) layout: Layout,
    }

    impl From<Project> for crate::project::Project {
        fn from(value: Project) -> Self {
            let registry = value.registry.into();
            let layout = value.layout.make_real(&registry);

            Self { layout, registry }
        }
    }

    impl From<Registry> for crate::registry::Registry {
        fn from(value: Registry) -> Self {
            let registry = Self {
                next_id: value.next_id,
                structs: value
                    .structs
                    .iter()
                    .map(|(&k, v)| (k, Rc::new(RefCell::new(v.make_real(k)))))
                    .collect(),
                addresses: value
                    .addresses
                    .into_iter()
                    .map(|(k, v)| (k, Rc::new(RefCell::new(v.into()))))
                    .collect(),
                dirty: true,
            };

            for (id, s) in value.structs {
                s.populate_nodes(
                    &mut *registry.structs.get(&id).unwrap().borrow_mut(),
                    &registry,
                );
            }

            registry
        }
    }

    impl From<Address> for crate::Address {
        fn from(value: Address) -> Self {
            Self(value.0, value.1)
        }
    }

    impl Struct {
        fn make_real(&self, id: RegistryId) -> crate::Struct {
            crate::Struct {
                name: self.name.clone(),
                row_count: self.row_count,
                id: id,
                layout: Default::default(),
                nodes: Default::default(),
            }
        }

        fn populate_nodes(self, real: &mut crate::Struct, registry: &crate::registry::Registry) {
            real.nodes = self
                .nodes
                .into_iter()
                .map(|(k, v)| (k, RefCell::new(v.make_real(registry))))
                .collect()
        }
    }

    impl Layout {
        fn make_real(&self, registry: &crate::registry::Registry) -> crate::Layout {
            let convert = super::TreeConvert {
                registry,
                convert_pane: Box::new(move |pane, registry| match pane {
                    Pane::AddressStruct { address, r#struct } => Some(crate::Pane::AddressStruct {
                        r#struct: Rc::downgrade(&registry.structs.get(r#struct).unwrap()),
                        address: Rc::downgrade(&registry.addresses.get(address).unwrap()),
                    }),
                    Pane::StructList => Some(crate::Pane::StructList),
                    Pane::AddressList => Some(crate::Pane::AddressList),
                    Pane::ProcessList { matching } => Some(crate::Pane::ProcessList {
                        matching: matching.into(),
                    }),
                }),
            };

            let (new_tiles, new_root) = convert.convert(&self.tree);

            crate::Layout {
                tree: egui_tiles::Tree::new("layout-tree", new_root, new_tiles),
            }
        }
    }
}

impl v1::Node {
    fn new(value: &crate::node::Node, registry: &crate::Registry) -> Option<Self> {
        match value {
            crate::node::Node::U8 => Some(Self::U8),
            crate::node::Node::U16 => Some(Self::U16),
            crate::node::Node::U32 => Some(Self::U32),
            crate::node::Node::U64 => Some(Self::U64),
            crate::node::Node::Struct(s) => s
                .upgrade()
                .map(|s| Self::Struct(registry.struct_id(&s).unwrap())),
            crate::node::Node::Pointer(s) => s
                .upgrade()
                .map(|s| Self::Pointer(registry.struct_id(&s).unwrap())),
        }
    }
}

impl v1::Struct {
    fn new(from: &crate::node::Struct, registry: &crate::Registry) -> Self {
        Self {
            row_count: from.row_count,
            name: from.name.clone(),
            nodes: from
                .nodes
                .iter()
                .filter_map(|(k, v)| v1::Node::new(&*v.borrow(), registry).map(|node| (*k, node)))
                .collect(),
        }
    }
}

impl v1::Address {
    pub(crate) fn new(from: &crate::Address) -> Self {
        Self(from.0.clone(), from.1.clone())
    }
}

impl v1::Registry {
    fn new(from: &crate::Registry) -> Self {
        Self {
            next_id: from.next_id,
            structs: from
                .structs
                .iter()
                .map(|(k, v)| (*k, v1::Struct::new(&*v.borrow(), from)))
                .collect(),
            addresses: from
                .addresses
                .iter()
                .map(|(k, v)| (*k, v1::Address::new(&*v.borrow())))
                .collect(),
        }
    }
}

type ConvertPane<TPaneA, TPaneB> = Box<dyn Fn(&TPaneA, &crate::Registry) -> Option<TPaneB>>;

struct TreeConvert<'a, TPaneA, TPaneB> {
    pub(crate) registry: &'a crate::Registry,
    pub(crate) convert_pane: ConvertPane<TPaneA, TPaneB>,
}

impl<'a, TPaneA, TPaneB> TreeConvert<'a, TPaneA, TPaneB> {
    fn clone_tile(
        &self,
        old_pane: &TPaneA,
        old_tiles: &egui_tiles::Tiles<TPaneA>,
        new_tiles: &mut egui_tiles::Tiles<TPaneB>,
    ) -> Option<egui_tiles::TileId> {
        (self.convert_pane)(old_pane, &self.registry).map(|pane| new_tiles.insert_pane(pane))
    }

    fn clone_container(
        &self,
        old_container: &egui_tiles::Container,
        old_tiles: &egui_tiles::Tiles<TPaneA>,
        new_tiles: &mut egui_tiles::Tiles<TPaneB>,
    ) -> egui_tiles::TileId {
        match old_container {
            egui_tiles::Container::Tabs(tabs) => {
                let mut new_tabs = vec![];

                let mut new_active_tile = None;

                for tile in &tabs.children {
                    let new_tile = self.clone(*tile, old_tiles, new_tiles);

                    if let Some(new_tile) = new_tile {
                        if let Some(t) = tabs.active {
                            if t == *tile {
                                new_active_tile = Some(new_tile);
                            }
                        }

                        new_tabs.push(new_tile);
                    }
                }

                let mut container = egui_tiles::Tabs::new(new_tabs);
                container.active = new_active_tile;

                new_tiles.insert_container(container)
            }
            egui_tiles::Container::Linear(linear) => {
                let mut new_children = vec![];

                for tile in &linear.children {
                    if let Some(new_tile) = self.clone(*tile, old_tiles, new_tiles) {
                        new_children.push(new_tile);
                    }
                }

                let mut container = egui_tiles::Linear::new(linear.dir, new_children);
                container.shares = linear.shares.clone();

                new_tiles.insert_container(container)
            }
            egui_tiles::Container::Grid(grid) => {
                let mut new_children = vec![];

                for tile in grid.children() {
                    let new_tile = self.clone(*tile, old_tiles, new_tiles);

                    if let Some(new_tile) = new_tile {
                        new_children.push(new_tile);
                    }
                }

                let mut container = egui_tiles::Grid::new(new_children);

                container.layout = grid.layout;
                container.col_shares = grid.col_shares.clone();
                container.row_shares = grid.row_shares.clone();

                new_tiles.insert_container(container)
            }
        }
    }

    fn clone(
        &self,
        old_tile: egui_tiles::TileId,
        old_tiles: &egui_tiles::Tiles<TPaneA>,
        new_tiles: &mut egui_tiles::Tiles<TPaneB>,
    ) -> Option<egui_tiles::TileId> {
        if let Some(container) = old_tiles.get_container(old_tile) {
            Some(self.clone_container(container, old_tiles, new_tiles))
        } else if let Some(tile) = old_tiles.get_pane(&old_tile) {
            Some(self.clone_tile(tile, old_tiles, new_tiles)?)
        } else {
            unreachable!()
        }
    }

    fn convert(
        &self,
        tree: &egui_tiles::Tree<TPaneA>,
    ) -> (egui_tiles::Tiles<TPaneB>, egui_tiles::TileId) {
        let old_root = tree.root().unwrap();
        let old_tiles = &tree.tiles;

        let mut new_tiles = egui_tiles::Tiles::default();

        // TODO(emily): Should we alway even expect a root?
        let new_root = self.clone(old_root, &old_tiles, &mut new_tiles).unwrap();
        (new_tiles, new_root)
    }
}

impl v1::Layout {
    fn new(from: &crate::Layout, registry: &crate::Registry) -> Self {
        let convert = TreeConvert {
            registry,
            convert_pane: Box::new(move |pane, registry| match pane {
                crate::Pane::AddressStruct { r#struct, address } => {
                    let Some((r#struct, address)) = r#struct.upgrade().zip(address.upgrade())
                    else {
                        return None;
                    };

                    Some(v1::Pane::AddressStruct {
                        r#struct: registry.struct_id(&r#struct).unwrap(),
                        address: registry.address_id(&address).unwrap(),
                    })
                }
                crate::Pane::StructList => Some(v1::Pane::StructList),
                crate::Pane::AddressList => Some(v1::Pane::AddressList),
                crate::Pane::ProcessList { matching } => Some(v1::Pane::ProcessList {
                    matching: matching.clone(),
                }),
            }),
        };

        let (new_tiles, new_root) = convert.convert(&from.tree);

        Self {
            tree: egui_tiles::Tree::new("storage-tree", new_root, new_tiles),
        }
    }
}

impl v1::Project {
    fn new(from: &crate::project::Project) -> Self {
        Self {
            layout: v1::Layout::new(&from.layout, &from.registry),
            registry: v1::Registry::new(&from.registry),
        }
    }
}

impl From<&Project> for Storage {
    fn from(value: &Project) -> Self {
        Self::V1(v1::Project::new(value))
    }
}

impl From<Storage> for Project {
    fn from(value: Storage) -> Self {
        match value {
            Storage::V1(project) => project.into(),
        }
    }
}

pub(crate) fn save_to_disk(project: &Project, path: &std::path::Path) -> Result<()> {
    let project_storage = Storage::from(project);

    let mut f = std::fs::OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(path)?;

    serde_json::to_writer_pretty(&mut f, &project_storage)?;

    Ok(())
}

pub(crate) fn load_from_disk(path: &std::path::Path) -> Result<Project> {
    let f = std::fs::OpenOptions::new()
        .create(false)
        .truncate(false)
        .read(true)
        .open(path)?;

    let storage: Storage = serde_json::from_reader(f)?;

    Ok(storage.into())
}
