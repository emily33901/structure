use rhai::{Engine, Scope};

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

impl Default for ScriptEngine {
    fn default() -> Self {
        Self::new()
    }
}
