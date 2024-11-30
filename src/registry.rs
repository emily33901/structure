use serde::Deserialize;
use serde::Serialize;

use super::Address;
use super::Pane;
use crate::node::Struct;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Hash, PartialEq, Eq, Copy, Clone, Serialize, Deserialize, Debug)]
pub(crate) struct RegistryId(pub usize);

impl std::fmt::Display for RegistryId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Default)]
pub(crate) struct Registry {
    pub(crate) next_id: usize,
    pub(crate) structs: HashMap<RegistryId, Rc<RefCell<Struct>>>,
    pub(crate) addresses: HashMap<RegistryId, Rc<RefCell<Address>>>,

    pub(crate) dirty: bool,
}

impl Registry {
    pub(crate) fn next_id(&mut self) -> RegistryId {
        let id = self.next_id;
        self.next_id += 1;
        RegistryId(id)
    }

    pub(crate) fn default_struct(&mut self) -> Rc<RefCell<Struct>> {
        self.register_struct(Struct::default())
    }

    pub(crate) fn register_struct(&mut self, s: Struct) -> Rc<RefCell<Struct>> {
        let id = self.next_id();
        self.structs.insert(id, RefCell::new(s).into());
        self.structs.get(&id).unwrap().clone()
    }

    pub(crate) fn default_address(&mut self) -> Rc<RefCell<Address>> {
        self.register_address(Address::default())
    }

    pub(crate) fn register_address(&mut self, address: Address) -> Rc<RefCell<Address>> {
        let id = self.next_id();
        self.addresses.insert(id, RefCell::new(address).into());
        self.addresses.get(&id).unwrap().clone()
    }

    // TODO(emily): We should have some distinction between a named address and an unamed address
    // because here we would like to be able to pass in either a named or unnamed address.
    pub(crate) fn find_or_register_address(&mut self, address: Address) -> Rc<RefCell<Address>> {
        for (key, v) in &self.addresses {
            if **v.borrow() == *address {
                return v.clone();
            }
        }

        self.register_address(address)
    }

    pub(crate) fn find_struct(&self, id: &RegistryId) -> Option<Rc<RefCell<Struct>>> {
        self.structs.get(id).cloned()
    }

    pub(crate) fn default_pane(&mut self) -> Pane {
        Pane::AddressStruct {
            r#struct: Rc::downgrade(&self.default_struct()),
            address: Rc::downgrade(&self.default_address()),
        }
    }

    pub(crate) fn struct_id(&self, s: &Rc<RefCell<Struct>>) -> Option<RegistryId> {
        for (id, other_s) in &self.structs {
            if Rc::ptr_eq(s, other_s) {
                return Some(*id);
            }
        }

        None
    }

    pub(crate) fn address_id(&self, address: &Rc<RefCell<Address>>) -> Option<RegistryId> {
        for (id, other_address) in &self.addresses {
            if Rc::ptr_eq(address, other_address) {
                return Some(*id);
            }
        }

        None
    }
}
