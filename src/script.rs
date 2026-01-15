use rhai::{Engine, Scope};
use crate::definition::Node;
use crate::instance::LogicCallbacks;
use crate::memory::{Memory, RhaiMemory};

pub struct ScriptEngine {
    pub(crate) engine: Engine,
}

impl ScriptEngine {
    pub fn new() -> Self {
        let mut engine = Engine::new();

        // Register memory reading functions
        // These will be available to all scripts
        Self::register_api(&mut engine);

        Self { engine }
    }

    fn register_api(engine: &mut Engine) {
        // Register the RhaiMemory type and its methods
        engine
            .register_type::<RhaiMemory>()
            .register_fn("read_u8", RhaiMemory::read_u8)
            .register_fn("read_u16", RhaiMemory::read_u16)
            .register_fn("read_u32", RhaiMemory::read_u32)
            .register_fn("read_u64", RhaiMemory::read_u64);
    }

    pub fn compile_logic_script(
        &self,
        script: &str,
        address: usize,
        memory: &mut Memory,
    ) -> Result<(rhai::AST, LogicCallbacks), Box<rhai::EvalAltResult>> {
        let ast = self.engine.compile(script)?;
        let mut scope = rhai::Scope::new();

        // Push address and memory into scope for closures to capture
        scope.push("address", address as i64);
        scope.push("memory", RhaiMemory::new(memory));

        // Evaluate script to get the callbacks map
        let callbacks_map: rhai::Map = self.engine.eval_ast_with_scope(&mut scope, &ast)?;

        let callbacks = LogicCallbacks::from_map(&callbacks_map)?;

        Ok((ast, callbacks))
    }

    pub fn evaluate(
        &self,
        script: &str,
        scope: &mut Scope,
    ) -> Result<String, Box<rhai::EvalAltResult>> {
        // Evaluate script and return result as string
        // The string describes what node type to display
        self.engine.eval_with_scope::<String>(scope, script)
    }
}

/// Parse comma-separated node types like "u8, u16, u32" or single type like "u64"
pub fn parse_multiple_nodes(node_types: &str) -> Vec<Node> {
    node_types
        .split(',')
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .filter_map(parse_node_result)
        .collect()
}

pub fn parse_node_result(node_type: &str) -> Option<Node> {
    // Parse a single node type string into a Node
    match node_type.to_lowercase().as_str() {
        "u8" => Some(Node::U8),
        "u16" => Some(Node::U16),
        "u32" => Some(Node::U32),
        "u64" => Some(Node::U64),
        _ => None,
    }
}

impl Default for ScriptEngine {
    fn default() -> Self {
        Self::new()
    }
}
