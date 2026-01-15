use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use egui::ahash::HashMap;

use crate::registry::RegistryId;

#[derive(Debug)]
pub enum Node {
    U8,
    U16,
    U32,
    U64,
    Struct(Weak<RefCell<Struct>>),
    Pointer(Weak<RefCell<Struct>>),
    Logic(Weak<RefCell<Logic>>),
}

#[derive(Debug)]
pub struct Struct {
    pub(crate) row_count: usize,
    /// Map of row to Node
    pub(crate) nodes: HashMap<usize, Rc<RefCell<Node>>>,
    pub(crate) name: String,
    pub(crate) id: RegistryId,
}

#[derive(Debug)]
pub struct Logic {
    pub(crate) script: String,
    pub(crate) name: String,
    pub(crate) id: RegistryId,
}

pub struct StructBuilder {
    pub(crate) row_count: usize,
    /// Map of row to Node
    pub(crate) nodes: HashMap<usize, Rc<RefCell<Node>>>,
    pub(crate) name: Option<String>,
}

impl StructBuilder {
    pub(crate) fn new(row_count: usize, nodes: HashMap<usize, Rc<RefCell<Node>>>) -> Self {
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

pub struct LogicBuilder {
    pub(crate) script: String,
    pub(crate) name: Option<String>,
}

impl LogicBuilder {
    pub(crate) fn new(script: String) -> Self {
        Self { script, name: None }
    }

    pub(crate) fn default() -> Self {
        const DEFAULT_SCRIPT: &str = r#"
        let node_count = || 7;
        let byte_size = || 56;
        let get_node = |index| "u64";
        let get_offset = |index| index * 8;
        
        #{
            node_count: node_count,
            byte_size: byte_size,
            get_node: get_node,
            get_offset: get_offset
        }
        "#;

        Self {
            script: DEFAULT_SCRIPT.to_string(),
            name: None,
        }
    }

    pub(crate) fn name(self, name: &str) -> Self {
        Self {
            name: Some(name.into()),
            ..self
        }
    }

    pub(crate) fn build(self, id: RegistryId) -> Logic {
        Logic {
            script: self.script,
            name: self.name.unwrap_or_else(|| format!("logic-{}", id)),
            id,
        }
    }
}

impl Node {
    pub fn byte_size(&self) -> usize {
        match self {
            Self::U64 => 8,
            Self::U32 => 4,
            Self::U16 => 2,
            Self::U8 => 1,
            Self::Pointer(_) => 8,
            Self::Struct(s) => s
                .upgrade()
                .map(|s| {
                    let size = s.borrow().byte_size();
                    assert_ne!(size, 0);
                    size
                })
                .unwrap_or(8),
            Self::Logic(_) => 8,
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
    if bytes.is_multiple_of(8) {
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
