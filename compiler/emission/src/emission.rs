use std::fs::File;
use std::io::{BufWriter, Write};

use lir::*;

pub fn output(path: &str, assm: Program) -> std::io::Result<()> {

    let output = File::create(path)?;
    let mut writer = BufWriter::new(output);

    match assm {
        Program { func } => {
            emit_func(&mut writer, func)?;
            emit_stack_note(&mut writer)?;
        }
    }

    writer.flush().expect("Unable to write assembly to output");

    Ok(())
}

fn emit_func<W: Write>(writer: &mut W, func: Function) -> std::io::Result<()> {
    writeln!(writer, "# <function: {}>", func.name)?;
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
        Instruction::Mov { src, dest } => {
            writeln!(
                writer,
                "\tmovl {}, {}",
                show_operand(src),
                show_operand(dest)
            )?
        }
        Instruction::Ret => {
            writeln!(writer, "\n# <return>")?;
            writeln!(writer, "\tmovq %rbp, %rsp")?;
            writeln!(writer, "\tpopq %rbp")?;
            writeln!(writer, "\tret")?
        }
        Instruction::Unary { op, dest } => {
            writeln!(writer, "\t{} {}", show_unary(op), show_operand(dest))?;
        }
        Instruction::AllocateStack(amt) => {
            writeln!(writer, "\tsubq ${}, %rsp", amt)?;
        }

        Instruction::Binary { op, src, dest } => {
            writeln!(writer, "# <{:?} {:?} {:?}>", src, op, dest)?;
            writeln!(
                writer,
                "\t{} {}, {}",
                show_binary(op),
                show_operand(src),
                show_operand(dest)
            )?;
        }
        Instruction::Idiv(op) => {
            writeln!(writer, "\tidivl {}", show_operand(op))?;
        }
        Instruction::Cdq => {
            writeln!(writer, "\tcdq")?;
        }

        Instruction::Cmp(first, second) => {
            writeln!(
                writer,
                "\tcmpl {}, {}",
                show_operand(first),
                show_operand(second)
            )?;
        }
        Instruction::Jmp { label } => {
            writeln!(writer, "\tjmp .L_{}", label)?;
        }
        Instruction::JmpCond { condition, label } => {
            writeln!(writer, "\tj{} .L_{}", show_condition(condition), label)?;
        }
        Instruction::SetCond { condition, dest } => {
            writeln!(
                writer,
                "\tset{} {}",
                show_condition(condition),
                show_byte_operand(dest)
            )?;
        }
        Instruction::Label(label) => {
            writeln!(writer, "# <Label: {}>", label)?;
            writeln!(writer, ".L_{}:", label)?;
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

fn show_binary(op: BinaryOp) -> String {
    match op {
        BinaryOp::Add => "addl".to_string(),
        BinaryOp::Sub => "subl".to_string(),
        BinaryOp::Mult => "imull".to_string(),
        BinaryOp::And => "andl".to_string(),
        BinaryOp::Or => "orl".to_string(),
        BinaryOp::Xor => "xorl".to_string(),
        BinaryOp::Sal => "sall".to_string(),
        BinaryOp::Sar => "sarl".to_string(),
    }
}

fn show_byte_operand(op: Operand) -> String {
    match op {
        Operand::Register(Register::AX) => format!("%{}", "al"),
        Operand::Register(Register::DX) => format!("%{}", "dl"),
        Operand::Register(Register::R10) => format!("%{}", "r10b"),
        Operand::Register(Register::R11) => format!("%{}", "r11b"),
        _ => show_operand(op),
    }
}

fn show_operand(op: Operand) -> String {
    match op {
        Operand::Register(reg) => format!(
            "%{}",
            match reg {
                Register::AX => "eax",
                Register::CX => "ecx",
                Register::DX => "edx",
                Register::R10 => "r10d",
                Register::R11 => "r11d",
            }
        ),
        Operand::Stack(amt) => format!("{}(%rbp)", -amt),
        Operand::Imm(val) => format!("${}", val),
        Operand::Pseudo(_) => {
            panic!("No Pseudo-registers should be in tree when outputing assembly")
        }
    }
}

fn show_condition(cond: Condition) -> String {
    match cond {
        Condition::E => "e".to_string(),
        Condition::NE => "ne".to_string(),
        Condition::G => "g".to_string(),
        Condition::GE => "ge".to_string(),
        Condition::L => "l".to_string(),
        Condition::LE => "le".to_string(),
    }
}

fn emit_stack_note<W: Write>(writer: &mut W) -> std::io::Result<()> {
    writeln!(writer, ".section .note.GNU-stack,\"\",@progbits")?;
    Ok(())
}
