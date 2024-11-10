use crate::registry::Registry;

pub(crate) struct Project {
    pub(crate) registry: Registry,
}

impl Project {
    pub(crate) fn new(registry: Registry) -> Self {
        Self { registry }
    }
}
