use crate::instance::logic_instance::{RhaiLogicNode, rhai_logic_node};
use crate::memory::{Memory, RhaiMemory};
use crate::registry::RegistryId;
use crate::scratch::{RhaiScratch, ScratchPad};
use rhai::{Engine, Scope};

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
            .register_fn("read_string", RhaiMemory::read_string)
            .register_fn(
                "read_zero_terminated_string",
                RhaiMemory::read_zero_terminated_string,
            )
            .register_fn("read_u8", RhaiMemory::read_u8)
            .register_fn("read_u16", RhaiMemory::read_u16)
            .register_fn("read_u32", RhaiMemory::read_u32)
            .register_fn("read_u64", RhaiMemory::read_u64);

        engine
            .register_type_with_name::<RhaiLogicNode>("Node")
            .register_fn("make_comment", rhai_logic_node::make_comment)
            .register_fn("make_pointer", rhai_logic_node::make_pointer)
            .register_fn("make_struct", rhai_logic_node::make_struct)
            .register_fn("make_utf8", rhai_logic_node::make_utf8)
            .register_fn("make_pointer_utf8", rhai_logic_node::make_pointer_utf8)
            .register_fn("make_u64", rhai_logic_node::make_u64)
            .register_fn("make_u32", rhai_logic_node::make_u32)
            .register_fn("make_u16", rhai_logic_node::make_u16)
            .register_fn("make_u8", rhai_logic_node::make_u8);

        // Register scratch pad functions
        engine
            .register_type::<RhaiScratch>()
            .register_fn("print", RhaiScratch::print)
            .register_fn("println", RhaiScratch::println)
            .register_fn("clear", RhaiScratch::clear);
    }

    pub fn compile_logic_script(
        &self,
        script: &str,
        address: usize,
        memory: &mut Memory,
        logic_id: RegistryId,
        scratch_pad: &mut ScratchPad,
    ) -> Result<(rhai::AST, rhai::Map), Box<rhai::EvalAltResult>> {
        // Clear scratch at start of evaluation
        scratch_pad.clear(logic_id);

        let ast = self.engine.compile(script)?;
        let mut scope = rhai::Scope::new();

        // Push address and memory into scope for closures to capture
        scope.push("address", address as i64);
        scope.push("memory", RhaiMemory::new(memory));
        scope.push("scratch", RhaiScratch::new(logic_id, scratch_pad));

        // Evaluate script to get the callbacks map
        let callbacks_map: rhai::Map = self.engine.eval_ast_with_scope(&mut scope, &ast)?;

        Ok((ast, callbacks_map))
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

impl Default for ScriptEngine {
    fn default() -> Self {
        Self::new()
    }
}
