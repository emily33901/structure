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
                Node::Struct(registry_id) => {
                    crate::node::Node::Struct(registry.structs.get(registry_id).unwrap().clone())
                }
                Node::Pointer(registry_id) => {
                    crate::node::Node::Pointer(registry.structs.get(registry_id).unwrap().clone())
                }
            }
        }
    }

    #[derive(Serialize, Deserialize)]
    pub(super) struct Struct {
        pub(super) size: usize,
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
    pub(super) struct Project {
        pub(super) registry: Registry,
    }

    impl From<Project> for crate::project::Project {
        fn from(value: Project) -> Self {
            Self {
                registry: value.registry.into(),
            }
        }
    }

    impl From<Registry> for crate::registry::Registry {
        fn from(value: Registry) -> Self {
            let registry = Self {
                next_id: value.next_id,
                structs: value
                    .structs
                    .iter()
                    .map(|(k, v)| (*k, Rc::new(RefCell::new(v.make_real()))))
                    .collect(),
                addresses: value
                    .addresses
                    .into_iter()
                    .map(|(k, v)| (k, Rc::new(RefCell::new(v.into()))))
                    .collect(),
                structs_by_name: Default::default(),
                addresses_by_name: Default::default(),
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
        fn make_real(&self) -> crate::Struct {
            crate::Struct {
                name: self.name.clone(),
                size: self.size,
                ..Default::default()
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

    impl crate::node::Struct {}
}

impl v1::Node {
    fn new(value: &crate::node::Node, registry: &crate::Registry) -> Self {
        match value {
            crate::node::Node::U8 => Self::U8,
            crate::node::Node::U16 => Self::U16,
            crate::node::Node::U32 => Self::U32,
            crate::node::Node::U64 => Self::U64,
            crate::node::Node::Struct(s) => Self::Struct(registry.struct_id(s).unwrap()),
            crate::node::Node::Pointer(s) => Self::Pointer(registry.struct_id(s).unwrap()),
        }
    }
}

impl v1::Struct {
    fn new(from: &crate::node::Struct, registry: &crate::Registry) -> Self {
        Self {
            size: from.size,
            name: from.name.clone(),
            nodes: from
                .nodes
                .iter()
                .map(|(k, v)| (*k, v1::Node::new(&*v.borrow(), registry)))
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

impl v1::Project {
    fn new(from: &crate::project::Project) -> Self {
        Self {
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
    let mut f = std::fs::OpenOptions::new()
        .create(false)
        .truncate(false)
        .read(true)
        .open(path)?;

    let storage: Storage = serde_json::from_reader(f)?;

    Ok(storage.into())
}
