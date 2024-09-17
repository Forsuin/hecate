use std::fmt::{Display, Formatter};
use thiserror::Error;

#[derive(Error, Clone, Debug)]
pub struct SemErr {
    message: String,
}

impl Display for SemErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl SemErr {
    pub(crate) fn new(message: String) -> Self {
        Self { message }
    }
}

pub type SemanticResult<T> = Result<T, SemErr>;