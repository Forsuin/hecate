pub use label_loops::*;
pub use validate_labels::*;
pub use ident_resolution::*;
pub use analyze_switches::*;
pub use type_checker::*;

pub mod analyze_switches;
pub mod label_loops;
mod sem_err;
pub mod validate_labels;
pub mod ident_resolution;
pub mod type_checker;

