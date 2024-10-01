use std::collections::HashMap;
use crate::{IdentifierAttr, InitialVal, Symbol, Type};

pub struct SymbolTable {
    pub symbols: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::with_capacity(20),
        }
    }

    pub fn add_automatic_var(&mut self, name: String, t: Type) {
        self.symbols.insert(
            name,
            Symbol {
                t,
                attrs: IdentifierAttr::Local,
            },
        );
    }

    pub fn add_static_var(&mut self, name: String, t: Type, global: bool, init: InitialVal) {
        self.symbols.insert(
            name,
            Symbol {
                t,
                attrs: IdentifierAttr::Static { init, global },
            },
        );
    }

    pub fn add_func(&mut self, name: String, t: Type, global: bool, defined: bool) {
        self.symbols.insert(
            name,
            Symbol {
                t,
                attrs: IdentifierAttr::Func {
                    defined,
                    global,
                    stack_frame_size: 0,
                },
            },
        );
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    pub fn is_global(&self, name: &str) -> bool {
        match self.get(name) {
            None => false,
            Some(symbol) => match symbol.attrs {
                IdentifierAttr::Func { global, .. } => global,
                IdentifierAttr::Static { global, .. } => global,
                IdentifierAttr::Local => false,
            },
        }
    }

    // Todo: add better error handling
    pub fn is_static(&self, name: &str) -> Result<bool, String> {
        match self.get(name) {
            None => Ok(false),
            Some(symbol) => match symbol.attrs {
                IdentifierAttr::Func { .. } => Err("Internal Error: functions don't have static storage".to_string()),
                IdentifierAttr::Static { .. } => Ok(true),
                IdentifierAttr::Local => Ok(false),
            },
        }
    }

    pub fn is_defined(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    pub fn set_bytes_required(&mut self, name: &str, bytes_required: i32) -> Result<(), String> {
        if let Some(symbol) = self.symbols.get_mut(name) {
            return match symbol.attrs {
                IdentifierAttr::Func { ref mut stack_frame_size, .. } => {
                    *stack_frame_size = bytes_required;
                    Ok(())
                }
                _ => { Err(format!("Internal Error: {} is not a function, cannot set stack frame size", name)) }
            }
        }

        Err(format!("Internal Error: {} is not a symbol, cannot set stack frame size", name))
    }

    pub fn get_bytes_required(&self, name: &str) -> Result<i32, String> {
        if let Some(symbol) = self.symbols.get(name) {
            match symbol.attrs {
                IdentifierAttr::Func { stack_frame_size, .. } => {
                    return Ok(stack_frame_size)
                },
                _ => {
                    return Err(format!("Internal Error: '{}' is not a function", name))
                }
            }
        }

        Err(format!("Internal Error: '{}' sybol does not exist", name))
    }
}
