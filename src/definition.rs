use core::f32;
use std::{
    cell::RefCell,
    rc::Weak,
};

use egui::ahash::HashMap;

use crate::registry::RegistryId;

#[derive(Debug)]
pub(crate) enum Node {
    U8,
    U16,
    U32,
    U64,
    Struct(Weak<RefCell<Struct>>, RefCell<f32>),
    Pointer(Weak<RefCell<Struct>>, RefCell<f32>),
}

#[derive(Debug)]
pub(crate) struct Struct {
    pub(crate) row_count: usize,
    /// Map of row to Node
    pub(crate) nodes: HashMap<usize, RefCell<Node>>,
    pub(crate) name: String,
    pub(crate) id: RegistryId,
}

pub(crate) struct StructBuilder {
    pub(crate) row_count: usize,
    /// Map of row to Node
    pub(crate) nodes: HashMap<usize, RefCell<Node>>,
    pub(crate) name: Option<String>,
}

impl StructBuilder {
    pub(crate) fn new(row_count: usize, nodes: HashMap<usize, RefCell<Node>>) -> Self {
        Self {
            row_count,
            nodes,
            name: None,
        }
    }

    pub(crate) fn default() -> Self {
        Self {
            row_count: 8,
            nodes: Default::default(),
            name: None,
        }
    }

    pub(crate) fn name(self, name: &str) -> Self {
        Self {
            name: Some(name.into()),
            ..self
        }
    }

    pub(crate) fn build(self, id: RegistryId) -> Struct {
        Struct {
            row_count: self.row_count,
            nodes: self.nodes,
            name: self.name.unwrap_or_else(|| format!("struct-{}", id)),
            id,
        }
    }
}

impl Node {
    pub fn row_count(&self) -> usize {
        match self {
            Self::U64 | Self::U32 | Self::U16 | Self::U8 => 1,
            Self::Pointer(s, _) | Self::Struct(s, _) => {
                s.upgrade().map(|s| s.borrow().row_count()).unwrap_or(1)
            }
        }
    }

    pub fn byte_size(&self) -> usize {
        match self {
            Self::U64 => 8,
            Self::U32 => 4,
            Self::U16 => 2,
            Self::U8 => 1,
            Self::Pointer(_, _) => 8,
            Self::Struct(s, _) => s
                .upgrade()
                .map(|s| {
                    let size = s.borrow().byte_size();
                    assert_ne!(size, 0);
                    size
                })
                .unwrap_or(8),
        }
    }
}

impl Struct {
    pub fn byte_size(&self) -> usize {
        let mut bytes = 0;

        for row in 0..self.row_count {
            if let Some(node) = self.nodes.get(&row) {
                let node = node.borrow();
                bytes += node.byte_size();
            } else {
                bytes += node_none_byte_size_rules(bytes);
            }
        }

        bytes

        // self.size
    }

    pub fn row_count(&self) -> usize {
        self.row_count
    }
}

fn node_none_byte_size_rules(bytes: usize) -> usize {
    if bytes % 8 == 0 {
        8
    } else if bytes % 8 == 4 {
        4
    } else if bytes % 4 == 2 {
        2
    } else if bytes % 2 == 1 {
        1
    } else {
        unreachable!()
    }
}
