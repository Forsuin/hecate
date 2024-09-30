#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Func(FuncType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub param_count: usize,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum InitialVal {
    Tentative,
    Initial(i32),
    NoInit,
}

#[derive(Debug, PartialEq)]
pub enum IdentifierAttr {
    Func { defined: bool, global: bool, stack_frame_size: i32 },
    Static { init: InitialVal, global: bool },
    Local,
}

#[derive(Debug, PartialEq)]
pub struct Symbol {
    pub t: Type,
    pub attrs: IdentifierAttr,
}
