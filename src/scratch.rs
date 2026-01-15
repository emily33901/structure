use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::registry::RegistryId;

#[derive(Default)]
pub struct ScratchPad {
    /// Content per Logic script
    content: HashMap<RegistryId, String>,
}

impl ScratchPad {
    pub fn clear(&mut self, logic_id: RegistryId) {
        self.content.insert(logic_id, String::new());
    }

    pub fn append(&mut self, logic_id: RegistryId, text: &str) {
        self.content.entry(logic_id).or_default().push_str(text);
    }

    pub fn get(&self, logic_id: RegistryId) -> Option<&str> {
        self.content.get(&logic_id).map(|s| s.as_str())
    }
}

/// Rhai-compatible wrapper for ScratchPad that can be used in scripts.
#[derive(Clone)]
pub struct RhaiScratch {
    logic_id: RegistryId,
    inner: Rc<RefCell<*mut ScratchPad>>,
}

impl RhaiScratch {
    pub fn new(logic_id: RegistryId, scratch_pad: &mut ScratchPad) -> Self {
        Self {
            logic_id,
            inner: Rc::new(RefCell::new(scratch_pad as *mut _)),
        }
    }

    pub fn print(&mut self, text: String) {
        unsafe { (*(*self.inner.borrow_mut())).append(self.logic_id, &text) }
    }

    pub fn println(&mut self, text: String) {
        unsafe {
            let pad = &mut *(*self.inner.borrow_mut());
            pad.append(self.logic_id, &text);
            pad.append(self.logic_id, "\n");
        }
    }

    pub fn clear(&mut self) {
        unsafe { (*(*self.inner.borrow_mut())).clear(self.logic_id) }
    }
}
