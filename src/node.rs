use std::cell::RefCell;

use crate::{AddressResponse, State};

type ActionFn = dyn FnOnce(&RefCell<State>);

pub(crate) struct Action(Box<ActionFn>);

impl Action {
    pub fn new(f: impl FnOnce(&RefCell<State>) + 'static) -> Self {
        Self(Box::new(f))
    }

    pub(crate) fn call(self, state: &RefCell<State>) {
        (self.0)(state)
    }
}

impl std::fmt::Debug for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("StructAction").field(&"<action>").finish()
    }
}

impl From<Box<ActionFn>> for Action {
    fn from(value: Box<ActionFn>) -> Self {
        Self(value)
    }
}

impl From<Action> for AddressResponse {
    fn from(value: Action) -> Self {
        AddressResponse::Action(value)
    }
}

#[derive(Default)]
pub(crate) struct StructUiFlags {
    pub(crate) top_level: bool,
}
