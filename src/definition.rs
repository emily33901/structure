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
    Utf8(usize),
    PointerUtf8(usize),
    Comment(String),
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

impl Default for LogicBuilder {
    fn default() -> Self {
        Self {
            script: DEFAULT_SCRIPT.to_string(),
            name: None,
        }
    }
}

impl LogicBuilder {
    pub(crate) fn new(script: String) -> Self {
        Self { script, name: None }
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
            Self::Utf8(len) => *len,
            Self::Comment(_) => 0,
            Self::PointerUtf8(_) | Self::Pointer(_) => 8,
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

const DEFAULT_SCRIPT: &str = r#"
// first node is a byte that indicates whether it is a short string or not

let byte_size = || 40;

let is_short_string = memory.read_u8(address) == 0;

let nodes = if is_short_string {
    [
        make_comment(0, "short string"),
        make_utf8(8, 16),
        make_comment(24, "length"),
        make_u64(24),
        make_comment(32, "capacity"),
        make_u64(32),
    ]
} else {
    [
        make_comment(0, "long string"),
        make_pointer_utf8(8),
        make_comment(24, "length"),
        make_u64(24),
        make_comment(32, "capacity"),
        make_u64(32),
    ]
};

let nodes = || nodes;

#{
    byte_size: byte_size,
    nodes: nodes,
}
"#;
