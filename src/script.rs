use rhai::{Engine, Scope};
use crate::definition::Node;

pub struct ScriptEngine {
    engine: Engine,
}

impl ScriptEngine {
    pub fn new() -> Self {
        let mut engine = Engine::new();

        // Register memory reading functions
        // These will be available to all scripts
        Self::register_api(&mut engine);

        Self { engine }
    }

    fn register_api(_engine: &mut Engine) {
        // API functions will be registered here
        // For now, placeholder for future implementation
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

fn parse_node_result(node_type: &str) -> Option<Node> {
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
