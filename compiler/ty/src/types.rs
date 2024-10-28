use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Long,
    UInt,
    ULong,
    Func(FuncType),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Int => "int",
                Type::Long => "long",
                Type::UInt => "unsigned int",
                Type::ULong => "unsigned long",
                Type::Func(_) => "function",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncType {
    pub param_types: Vec<Type>,
    pub return_type: Box<Type>,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum InitialVal {
    Tentative,
    Initial(StaticInit),
    NoInit,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum StaticInit {
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
}

impl Display for StaticInit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StaticInit::Int(i) => {
                write!(f, "{}", i)
            }
            StaticInit::Long(i) => {
                write!(f, "{}", i)
            }
            StaticInit::UInt(i) => {
                write!(f, "{}", i)
            }
            StaticInit::ULong(i) => {
                write!(f, "{}", i)
            }
        }
    }
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
    UInt(u32),
    Long(i64),
    ULong(u64),
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

            Constant::UInt(val) => {
                write!(f, "{}", val)
            }
            Constant::ULong(val) => {
                write!(f, "{}", val)
            }
        }
    }
}

pub fn get_common_type(first: Type, second: Type) -> Type {
    if first == second {
        return first;
    }

    if get_size(&first) == get_size(&second) {
        return if is_signed(&first) { second } else { first };
    }

    if get_size(&first) > get_size(&second) {
        first
    } else {
        second
    }
}

pub fn is_signed(ty: &Type) -> bool {
    match ty {
        Type::Int | Type::Long => true,
        Type::UInt | Type::ULong => false,
        Type::Func(_) => {
            panic!("Internal Error: Function types don't have signedness")
        }
    }
}

pub fn get_size(ty: &Type) -> i32 {
    match ty {
        Type::Int | Type::UInt => 4,
        Type::Long | Type::ULong => 8,
        Type::Func(_) => {
            panic!("Internal Error: Function types don't have a size")
        }
    }
}

pub fn get_alignment(ty: &Type) -> i32 {
    match ty {
        Type::Int | Type::UInt => 4,
        Type::Long | Type::ULong => 8,
        Type::Func(_) => {
            panic!("Internal Error: Function types don't have an alignment")
        }
    }
}

pub fn get_constant_type(constant: &Constant) -> Type {
    match constant {
        Constant::Int(_) => Type::Int,
        Constant::UInt(_) => Type::UInt,
        Constant::Long(_) => Type::Long,
        Constant::ULong(_) => Type::ULong,
    }
}