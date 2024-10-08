use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Long,
    Func(FuncType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncType {
    pub param_types: Vec<Type>,
    pub return_type: Box<Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InitialVal {
    Tentative,
    Initial(Constant),
    NoInit,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IdentifierAttr {
    Func {
        defined: bool,
        global: bool,
        stack_frame_size: i32,
    },
    Static {
        init: InitialVal,
        global: bool,
    },
    Local,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    pub t: Type,
    pub attrs: IdentifierAttr,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Constant {
    Int(i32),
    Long(i64),
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Int(val) => {
                write!(f, "{}", val)
            }
            Constant::Long(val) => {
                write!(f, "{}", val)
            }
        }
    }
}

pub fn get_common_type(first: Type, second: Type) -> Type {
    if first == second {
        first
    } else {
        Type::Long
    }
}
