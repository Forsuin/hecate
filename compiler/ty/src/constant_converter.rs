use crate::{Constant, Type};

trait Castable {
    fn to_i32(&self) -> i32;
    fn to_i64(&self) -> i64;
}

impl Castable for i32 {
    fn to_i32(&self) -> i32 {
        *self
    }

    fn to_i64(&self) -> i64 {
        *self as i64
    }
}

fn cast<T: Castable>(constant: T, target_type: &Type) -> Constant {
    match target_type {
        Type::Int => Constant::Int(constant.to_i32()),
        Type::UInt => Constant::UInt(constant.to_i64() as u32),
        Type::Long => Constant::Long(constant.to_i64()),
        Type::ULong => Constant::ULong(constant.to_i64() as u64),
        Type::Float => Constant::Float(constant.to_i64() as f32),
        Type::Double => Constant::Double(constant.to_i64() as f64),
        Type::Func(_) => {
            unreachable!("Internal Error: Cannot cast constant to function type")
        }
    }
}

impl Castable for u32 {
    fn to_i32(&self) -> i32 {
        *self as i32
    }

    fn to_i64(&self) -> i64 {
        *self as i64
    }
}

impl Castable for u64 {
    fn to_i32(&self) -> i32 {
        *self as i32
    }

    fn to_i64(&self) -> i64 {
        *self as i64
    }
}

impl Castable for i64 {
    fn to_i32(&self) -> i32 {
        *self as i32
    }

    fn to_i64(&self) -> i64 {
        *self
    }
}

impl Castable for f32 {
    fn to_i32(&self) -> i32 {
        *self as i32
    }

    fn to_i64(&self) -> i64 {
        *self as i64
    }
}

impl Castable for f64 {
    fn to_i32(&self) -> i32 {
        *self as i32
    }

    fn to_i64(&self) -> i64 {
        *self as i64
    }
}

pub fn const_convert(target_type: &Type, constant: Constant) -> Constant {
    match constant {
        Constant::Int(i) => cast(i, target_type),
        Constant::UInt(i) => cast(i, target_type),
        Constant::Long(i) => cast(i, target_type),
        Constant::ULong(i) => cast(i, target_type),
        Constant::Float(i) => cast(i, target_type),
        Constant::Double(i) => cast(i, target_type), 
    }
}