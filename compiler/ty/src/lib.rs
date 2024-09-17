use std::collections::HashMap;
pub use types::*;

pub mod types;

pub type Scope<T> = HashMap<String, T>;

