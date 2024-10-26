use crate::AssemblyType;
use std::collections::HashMap;

pub enum AsmEntry {
    Obj { ty: AssemblyType, is_static: bool },
    Func { defined: bool, bytes_required: i32 },
}

pub struct AsmTable {
    pub symbols: HashMap<String, AsmEntry>,
}

impl AsmTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::with_capacity(20),
        }
    }

    pub fn add_func(&mut self, name: String, defined: bool) {
        self.symbols.insert(
            name,
            AsmEntry::Func {
                defined,
                bytes_required: 0,
            },
        );
    }

    pub fn add_obj(&mut self, name: String, ty: AssemblyType, is_static: bool) {
        self.symbols.insert(name, AsmEntry::Obj { ty, is_static });
    }

    pub fn set_bytes_required(&mut self, name: &String, bytes_required: i32) {
        if self.symbols.contains_key(name) {
            self.symbols.entry(name.clone()).and_modify(|val| {
                match val {
                    AsmEntry::Obj { .. } => {
                        panic!("Internal Error: Cannot set bytes required for an object")
                    }
                    AsmEntry::Func { bytes_required: old, .. } => {
                        *old = bytes_required;
                    }
                }
            });
        } else {
            panic!(
                "Internal Error: Function '{}' is not defined in AsmTable",
                name
            );
        }
    }

    pub fn get_bytes_required(&self, name: &String) -> i32 {
        match self.symbols.get(name) {
            None => {
                panic!(
                    "Internal Error: Function '{}' is not defined in AsmTable",
                    name
                );
            }
            Some(AsmEntry::Obj { .. }) => {
                panic!("Internal Error: '{}' is not a function in AsmTable", name);
            }
            Some(AsmEntry::Func { bytes_required, .. }) => *bytes_required,
        }
    }

    pub fn get_size(&self, name: &str) -> i32 {
        match self.symbols.get(name) {
            None => {
                panic!(
                    "Internal Error: Object '{}' is not defined in AsmTable",
                    name
                );
            }
            Some(AsmEntry::Obj {
                ty: AssemblyType::Long,
                ..
            }) => 4,
            Some(AsmEntry::Obj {
                ty: AssemblyType::Quad,
                ..
            }) => 8,
            Some(AsmEntry::Func { .. }) => {
                panic!(
                    "Internal Error: '{}' is a function, not an object in AsmTable",
                    name
                );
            }
        }
    }

    pub fn get_alignment(&self, name: &str) -> i32 {
        match self.symbols.get(name) {
            None => {
                panic!(
                    "Internal Error: Object '{}' is not defined in AsmTable",
                    name
                );
            }
            Some(AsmEntry::Obj {
                ty: AssemblyType::Long,
                ..
            }) => 4,
            Some(AsmEntry::Obj {
                ty: AssemblyType::Quad,
                ..
            }) => 8,
            Some(AsmEntry::Func { .. }) => {
                panic!(
                    "Internal Error: '{}' is a function, not an object in AsmTable",
                    name
                );
            }
        }
    }

    pub fn is_func_defined(&self, name: &str) -> bool {
        match self.symbols.get(name) {
            None => {
                panic!(
                    "Internal Error: Function '{}' is not defined in AsmTable",
                    name
                );
            }
            Some(AsmEntry::Obj { .. }) => {
                panic!("Internal Error: '{}' is not a function in AsmTable", name);
            }
            Some(AsmEntry::Func { defined, .. }) => *defined,
        }
    }

    pub fn is_static(&self, name: &str) -> bool {
        match self.symbols.get(name) {
            None => {
                panic!(
                    "Internal Error: Object '{}' is not defined in AsmTable",
                    name
                );
            }
            Some(AsmEntry::Obj { is_static, .. }) => *is_static,
            Some(AsmEntry::Func { .. }) => {
                panic!(
                    "Internal Error: '{}' is a function without static storage duration, not an object in AsmTable",
                    name
                );
            }
        }
    }
}
