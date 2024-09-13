use std::fs::File;
use std::io::{BufWriter, Write};

use crate::tacky::*;

type IOResult = std::io::Result<()>;

pub fn debug_tacky(program: &TranslationUnit, file_name: String)-> IOResult {
    let output = File::create(file_name)?;
    let mut writer = BufWriter::new(output);

    print_func(&mut writer, &program.func)?;

    writer.flush()?;

    Ok(())
}

fn print_func<W: Write>(writer: &mut W, func: &Func) -> IOResult {
    writeln!(writer, "{}:", func.name)?;

    for instruction in &func.instructions {
        print_instruction(writer, instruction)?;
    }

    Ok(())
}

fn print_instruction<W: Write>(writer: &mut W, instr: &Instruction) -> IOResult {
    match instr {
        Instruction::Return(val) => {
            writeln!(writer, "\tReturn({})", format_val(val))
        }
        Instruction::Unary { op, src, dest } => {
            writeln!(writer, "\t{} = {}{}", format_val(dest), format_unary(op), format_val(src))
        }
        Instruction::Binary { op, first, second, dest } => {
            writeln!(writer, "\t{} = {} {} {}", format_val(dest), format_val(first), format_binary(op), format_val(second))
        }
        Instruction::Copy { src, dest } => {
            writeln!(writer, "\t{} = {}", format_val(dest), format_val(src))
        }
        Instruction::Jump { target } => {
            writeln!(writer, "\tJump({})", target)
        }
        Instruction::JumpIfZero { condition, target } => {
            writeln!(writer, "\tJumpIfZero({}, {})", format_val(condition), target)
        }
        Instruction::JumpIfNotZero { condition, target } => {
            writeln!(writer, "\tJumpIfNotZero({}, {})", format_val(condition), target)
        }
        Instruction::Label(label) => {
            writeln!(writer, "\n  {}:", label)
        }
    }
}

fn format_val(val: &Val) -> String {
    match val {
        Val::Constant(c) => {
            format!("{}", *c)
        }
        Val::Var(var) => {
            var.clone()
        }
    }
}

fn format_unary(op: &UnaryOp) -> String {
    match op {
        UnaryOp::Complement => "~".to_string(),
        UnaryOp::Negate => "-".to_string(),
        UnaryOp::Not => "!".to_string(),
    }
}

fn format_binary(op: &BinaryOp) -> String {
    match op {
        BinaryOp::Add => "+".to_string(),
        BinaryOp::Subtract => "-".to_string(),
        BinaryOp::Multiply => "*".to_string(),
        BinaryOp::Divide => "/".to_string(),
        BinaryOp::Modulo => "%".to_string(),
        BinaryOp::BitshiftLeft => "<<".to_string(),
        BinaryOp::BitshiftRight => ">>".to_string(),
        BinaryOp::BitwiseAnd => "&".to_string(),
        BinaryOp::BitwiseOr => "|".to_string(),
        BinaryOp::BitwiseXor => "^".to_string(),
        BinaryOp::Equal => "==".to_string(),
        BinaryOp::NotEqual => "!=".to_string(),
        BinaryOp::Less => "<".to_string(),
        BinaryOp::LessEqual => "<=".to_string(),
        BinaryOp::Greater => ">".to_string(),
        BinaryOp::GreaterEqual => ">=".to_string(),
    }
}
