#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Func(FuncType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub param_count: usize,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum InitType {
    Declaration,
    Definition,
}

#[derive(Debug, PartialEq)]
pub struct Symbol {
    pub t: Type,
    pub init_type: InitType,
    pub stack_frame_size: i32,
}
