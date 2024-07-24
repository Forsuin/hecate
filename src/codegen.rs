pub mod tacky_gen;
pub mod assembly_gen;
mod replace_pseudoregs;

pub use tacky_gen::*;
pub use assembly_gen::*;
pub use replace_pseudoregs::*;

use std::fs::File;
use std::io::Write;

use crate::{Expr, Func, Function, Instruction, Operand, Program, Stmt, TranslationUnit};

// pub fn output(path: &str, assm: Program) -> std::io::Result<()> {
//     let mut output = File::create(path)?;
//
//     match assm {
//         Program { func } => {
//             emit_func(&mut output, func)?;
//             emit_stack_note(&mut output)?;
//         }
//     }
//
//     Ok(())
// }
//
// fn emit_func<W: Write>(writer: &mut W, func: Function) -> std::io::Result<()> {
//     writeln!(writer, "\t.globl {}", func.name)?;
//     writeln!(writer, "{}:", func.name)?;
//
//     for instruction in func.instructions {
//         emit_instruction(writer, instruction)?;
//     }
//
//     Ok(())
// }
//
// fn emit_instruction<W: Write>(writer: &mut W, instruction: Instruction) -> std::io::Result<()> {
//     match instruction {
//         Instruction::Mov { src, dest } => writeln!(
//             writer,
//             "\tmovl {}, {}",
//             show_operand(&src),
//             show_operand(&dest)
//         )?,
//         Instruction::Ret => writeln!(writer, "\tret")?,
//         _ => {}
//     }
//
//     Ok(())
// }
//
// fn show_operand(op: &Operand) -> String {
//     match op {
//         Operand::Register => "%eax".to_string(),
//         Operand::Imm(val) => format!("${}", val),
//     }
// }
//
// fn emit_stack_note<W: Write>(writer: &mut W) -> std::io::Result<()> {
//     writeln!(writer, ".section .note.GNU-stack,\"\",@progbits")?;
//     Ok(())
// }
