use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Long,
    UInt,
    ULong,
    Float,
    // Long Doubles will be converted to Doubles like MSVC, as Rust doesn't have an equivalent type
    Double,
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
                Type::Float => "float",
                Type::Double => "double",
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

#[derive(Debug, PartialEq, Clone)]
pub enum InitialVal {
    Tentative,
    Initial(StaticInit),
    NoInit,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StaticInit {
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
    Float(f32),
    Double(f64)
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

            StaticInit::Float(i) => {
                write!(f, "{}", i)
            }
            StaticInit::Double(i) => {
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Constant {
    Int(i32),
    UInt(u32),
    Long(i64),
    ULong(u64),
    Float(f32),
    Double(f64),
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
            Constant::Float(val) => {
                write!(f, "{}", val)
            }
            Constant::Double(val) => {
                write!(f, "{}", val)
            }
        }
    }
}

impl Display for SwitchableConstant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SwitchableConstant::Int(val) => {
                write!(f, "{}", val)
            }
            SwitchableConstant::Long(val) => {
                write!(f, "{}", val)
            }

            SwitchableConstant::UInt(val) => {
                write!(f, "{}", val)
            }
            SwitchableConstant::ULong(val) => {
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
        t @ Type::Func(_) | t @ Type::Float | t @ Type::Double => {
            panic!("Internal Error: {} types don't have signedness", t)
        }
    }
}

pub fn get_size(ty: &Type) -> i32 {
    match ty {
        Type::Int | Type::UInt | Type::Float => 4,
        Type::Long | Type::ULong | Type::Double => 8,
        Type::Func(_) => {
            panic!("Internal Error: Function types don't have a size")
        }
    }
}

pub fn get_alignment(ty: &Type) -> i32 {
    match ty {
        Type::Int | Type::UInt | Type::Float => 4,
        Type::Long | Type::ULong | Type::Double => 8,
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
        Constant::Float(_) => Type::Float,
        Constant::Double(_) => Type::Double,
    }
}
