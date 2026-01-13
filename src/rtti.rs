use std::{
    collections::{hash_map::Entry, HashMap},
    mem::offset_of,
};

use crate::memory::Memory;

#[derive(Default)]
pub(crate) struct RttiCache(HashMap<usize, Option<Rtti>>);

impl RttiCache {
    pub(crate) fn get(&mut self, address: usize, memory: &mut Memory<'_>) -> Option<&Rtti> {
        let entry = self.0.entry(address);
        match entry {
            Entry::Occupied(occupied_entry) => {
                let v = occupied_entry.into_mut();
                v.as_ref()
            }
            Entry::Vacant(vacant_entry) => {
                let rtti = rtti(address, memory);

                vacant_entry.insert(rtti).as_ref()
            }
        }
    }
}

#[derive(Debug)]
#[repr(C)]
struct RTTICompleteObjectLocator {
    signature: u32,
    offset: u32,
    constructor_displacement_offset: u32,
    type_descriptor_offset: u32,
    class_descriptor_offset: u32,
    self_offset: u32,
}

#[repr(C)]
struct RTTITypeDescriptor {
    vtable: usize,
    spare: usize,
    name_bytes: i8,
}

#[derive(Debug)]
#[repr(C)]
struct RTTIClassHierarchyDescriptor {
    signature: u32,
    attributes: u32,
    base_class_count: u32,
    array_offset: u32,
}

#[repr(C)]
struct RTTIObjectLocation {
    mdisp: u32,
    pdisp: u32,
    vdisp: u32,
}

#[repr(C)]
struct RTTIBaseClassDescriptor {
    type_descriptor_offset: u32,
    contained_bases: u32,
    object_location: RTTIObjectLocation,
    attributes: u32,
    class_descriptor_offset: u32,
}

pub(crate) struct Rtti {
    pub(crate) names: Vec<String>,
}

pub fn rtti(address: usize, memory: &mut Memory<'_>) -> Option<Rtti> {
    let complete_object_locator: RTTICompleteObjectLocator = memory.read(address);

    if complete_object_locator.self_offset == 0
        || complete_object_locator.class_descriptor_offset == 0
    {
        return None;
    }

    let base_address = address - complete_object_locator.self_offset as usize;

    let class_hierarchy_descriptor_address =
        base_address + complete_object_locator.class_descriptor_offset as usize;

    let class_hierarchy_descriptor: RTTIClassHierarchyDescriptor =
        memory.read(class_hierarchy_descriptor_address);

    let base_class_array_address = base_address + class_hierarchy_descriptor.array_offset as usize;

    // TODO(emily): We probably want some smartish way to check that this is a valid RTTIClassHierarchyDescriptor
    // before trying to allocate memory here.

    let mut base_class_offsets = vec![0_u32; class_hierarchy_descriptor.base_class_count as usize];

    let base_class_offsets_bytes = unsafe {
        std::slice::from_raw_parts_mut(
            base_class_offsets.as_mut_ptr() as *mut u8,
            class_hierarchy_descriptor.base_class_count as usize * std::mem::size_of::<u32>(),
        )
    };

    memory.get(base_class_array_address, base_class_offsets_bytes);

    let mut rtti = Rtti { names: vec![] };

    for base_class_offset in base_class_offsets {
        let base_class_descriptor: RTTIBaseClassDescriptor =
            memory.read(base_address + base_class_offset as usize);

        // Read some memory that contains the name in it

        let mut buffer = [0_u8; 1000];

        let type_descriptor_address =
            base_address + base_class_descriptor.type_descriptor_offset as usize;
        memory.get(
            type_descriptor_address + offset_of!(RTTITypeDescriptor, name_bytes),
            &mut buffer,
        );

        let Ok(type_name) =
            unsafe { std::ffi::CStr::from_ptr(buffer.as_ptr() as *const _) }.to_str()
        else {
            eprintln!("ignoring offset {base_class_offset} because it is not valid utf-8");
            continue;
        };

        rtti.names.push(demangle_msvc_typeinfo_name(type_name));
    }

    Some(rtti)
}

fn demangle_msvc_typeinfo_name(name: &str) -> String {
    if name.len() > 4 {
        let name_as_destructor = format!("?{}", &name[4..]);
        let name = symbolic_demangle::demangle(&name_as_destructor);
        name.to_string()
    } else {
        format!("!{}", name)
    }
}
