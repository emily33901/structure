use std::cell::RefCell;

use crate::{AddressResponse, State};

type StructActionFn = dyn FnOnce(&RefCell<State>);

pub(crate) struct StructAction(Box<StructActionFn>);

impl StructAction {
    pub fn new(f: impl FnOnce(&RefCell<State>) + 'static) -> Self {
        Self(Box::new(f))
    }

    pub(crate) fn call(self, state: &RefCell<State>) {
        (self.0)(state)
    }
}

impl std::fmt::Debug for StructAction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("StructAction").field(&"<action>").finish()
    }
}

impl From<Box<StructActionFn>> for StructAction {
    fn from(value: Box<StructActionFn>) -> Self {
        Self(value)
    }
}

impl From<StructAction> for AddressResponse {
    fn from(value: StructAction) -> Self {
        AddressResponse::Action(value)
    }
}

#[derive(Default)]
pub(crate) struct StructUiFlags {
    pub(crate) top_level: bool,
}

// impl Default for Struct {
//     fn default() -> Self {
//         Self {
//             layout: Default::default(),
//             row_count: 8,
//             nodes: Default::default(),
//             name: "Default struct".into(),
//         }
//     }
// }
