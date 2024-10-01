pub use analyze_switches::*;
pub use ident_resolution::*;
pub use label_loops::*;
pub use type_checker::*;
pub use validate_labels::*;

pub mod analyze_switches;
pub mod ident_resolution;
pub mod label_loops;
mod sem_err;
pub mod type_checker;
pub mod validate_labels;
