use std::fs::File;
use std::io::Write;

use lir::*;

pub fn output(path: &str, assm: Program) -> std::io::Result<()> {
    let mut output = File::create(path)?;

    match assm {
        Program { func } => {
            emit_func(&mut output, func)?;
            emit_stack_note(&mut output)?;
        }
    }

    Ok(())
}

fn emit_func<W: Write>(writer: &mut W, func: Function) -> std::io::Result<()> {
    writeln!(writer, "\t.globl {}", func.name)?;
    writeln!(writer, "{}:", func.name)?;
    writeln!(writer, "\tpushq %rbp")?;
    writeln!(writer, "\tmovq %rsp, %rbp")?;

    for instruction in func.instructions {
        emit_instruction(writer, instruction)?;
    }

    Ok(())
}

fn emit_instruction<W: Write>(writer: &mut W, instruction: Instruction) -> std::io::Result<()> {
    match instruction {
        Instruction::Mov { src, dest } => writeln!(
            writer,
            "\tmovl {}, {}",
            show_operand(src),
            show_operand(dest)
        )?,
        Instruction::Ret => {
            writeln!(writer, "\tmovq %rbp, %rsp")?;
            writeln!(writer, "\tpopq %rbp")?;
            writeln!(writer, "\tret")?
        },
        Instruction::Unary {op, dest} => {
            writeln!(writer, "\t{} {}", show_unary(op), show_operand(dest))?;
        },
        Instruction::AllocateStack(amt) => {
            writeln!(writer, "\tsubq ${}, %rsp", amt)?;
        }
    }

    Ok(())
}

fn show_unary(op: UnaryOp) -> String {
    match op {
        UnaryOp::Neg => "negl".to_string(),
        UnaryOp::Not => "notl".to_string(),
    }
}

fn show_operand(op: Operand) -> String {
    match op {
        Operand::Register(reg) => format!("%{}", match reg {
            Register::AX => "eax",
            Register::R10 => "r10d",
        }),
        Operand::Stack(amt) => format!("{}(%rbp)", -amt),
        Operand::Imm(val) => format!("${}", val),
        Operand::Pseudo(_) => panic!("No Pseudo-registers should be in tree when outputing assembly"),
    }
}

fn emit_stack_note<W: Write>(writer: &mut W) -> std::io::Result<()> {
    writeln!(writer, ".section .note.GNU-stack,\"\",@progbits")?;
    Ok(())
}
