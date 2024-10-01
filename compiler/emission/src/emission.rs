use std::fs::File;
use std::io::{BufWriter, Write};

use lir::*;
use ty::*;

type IOResult = std::io::Result<()>;

pub fn output(path: &str, assm: Program, symbols: &SymbolTable) -> IOResult {

    let output = File::create(path)?;
    let mut writer = BufWriter::new(output);

    for decl in &assm.decls {
        emit_decl(&mut writer, decl, symbols)?;
    }
    emit_stack_note(&mut writer)?;

    writer.flush().expect("Unable to write assembly to output");

    Ok(())
}

fn emit_decl<W: Write>(writer: &mut W, decl: &Decl, symbols: &SymbolTable) -> IOResult {
    match decl {
        Decl::Func(func) => {
            emit_func(writer, func, symbols)
        }
        Decl::StaticVar(var) => {
            emit_var(writer, var)
        }
    }
}

fn emit_var<W: Write>(writer: &mut W, var: &StaticVar) -> IOResult {
    writeln!(writer, "# <static var: {}>", var.name)?;
    let init;

    if var.global { writeln!(writer, "\t.globl {}", var.name)?; }

    // which section variable goes

    if var.init == 0 {
        writeln!(writer, "\t.bss")?;
        init = format!("\t.zero 4");
    } else {
        writeln!(writer, "\t.data")?;
        init = format!("\t.long {}", var.init);
    }

    // alignment
    writeln!(writer, "\t.balign 4")?;

    writeln!(writer, "{}:", var.name)?;
    writeln!(writer, "{}", init)?;

    Ok(())
}

fn emit_func<W: Write>(writer: &mut W, func: &Func, symbols: &SymbolTable) -> IOResult {
    writeln!(writer, "# <function: {}>", func.name)?;
    if func.global { writeln!(writer, "\t.globl {}", func.name)?; }
    writeln!(writer, "\t.text")?;
    writeln!(writer, "{}:", func.name)?;
    writeln!(writer, "\tpushq %rbp")?;
    writeln!(writer, "\tmovq %rsp, %rbp")?;

    for instruction in &func.instructions {
        emit_instruction(writer, instruction, symbols)?;
    }

    Ok(())
}

fn emit_instruction<W: Write>(writer: &mut W, instruction: &Instruction, symbols: &SymbolTable) -> IOResult {
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
            // writeln!(writer, "# <Allocate {}>", amt)?;
            writeln!(writer, "\tsubq ${}, %rsp", amt)?;
        }
        Instruction::Binary { op: op @ BinaryOp::Sal | op @ BinaryOp::Sar, src, dest } => {
            writeln!(writer, "# <{:?} {:?} {:?}>", src, op, dest)?;
            writeln!(
                writer,
                "\t{} {}, {}",
                show_binary(op),
                show_byte_operand(src),
                show_operand(dest)
            )?;
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
        Instruction::DeallocateStack(amt) => {
            // writeln!(writer, "# <Deallocate: {}>", amt)?;
            writeln!(writer, "\taddq ${}, %rsp", amt)?;
        }
        Instruction::Push(op) => {
            writeln!(writer, "\tpushq {}", show_quad_op(op))?;
        }
        Instruction::Call(func) => {
            writeln!(writer, "\tcall {}", show_fun_name(func, symbols))?;
        }
    }

    Ok(())
}

fn show_fun_name(name: &String, symbols: &SymbolTable) -> String {
    if symbols.get(name).is_some() {
        name.clone()
    }
    else {
        format!("{}@PLT", name)
    }
}

fn show_quad_op(op: &Operand) -> String {
    match op {
        Operand::Register(r) => show_quad_reg(r),
        _ => show_operand(op)
    }
}

fn show_unary(op: &UnaryOp) -> String {
    match op {
        UnaryOp::Neg => "negl".to_string(),
        UnaryOp::Not => "notl".to_string(),
    }
}

fn show_binary(op: &BinaryOp) -> String {
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

fn show_byte_reg(reg: &Register) -> String {
    match reg {
            Register::AX => "%al".to_string(),
            Register::CX => "%cl".to_string(),
            Register::DX => "%dl".to_string(),
            Register::DI => "%dil".to_string(),
            Register::SI => "%sil".to_string(),
            Register::R8 => "%r8b".to_string(),
            Register::R9 => "%r9b".to_string(),
            Register::R10 => "%r10b".to_string(),
            Register::R11 => "%r11b".to_string(),
    }
}

fn show_reg(reg: &Register) -> String {
    format!(
        "%{}",
        match reg {
            Register::AX => "eax",
            Register::CX => "ecx",
            Register::DX => "edx",
            Register::DI => "edi",
            Register::SI => "esi",
            Register::R8 => "r8d",
            Register::R9 => "r9d",
            Register::R10 => "r10d",
            Register::R11 => "r11d",
        }
    )
}

fn show_quad_reg(reg: &Register) -> String {
    format!("%{}", match reg {
        Register::AX => "rax",
        Register::CX => "rcx",
        Register::DX => "rdx",
        Register::DI => "rdi",
        Register::SI => "rsi",
        Register::R8 => "r8",
        Register::R9 => "r9",
        Register::R10 => "r10",
        Register::R11 => "r11",
    })
}

fn show_byte_operand(op: &Operand) -> String {
    match op {
        Operand::Register(reg) => show_byte_reg(reg),
        _ => show_operand(op),
    }
}

fn show_operand(op: &Operand) -> String {
    match op {
        Operand::Register(reg) => show_reg(reg),
        Operand::Stack(amt) => format!("{}(%rbp)", amt),
        Operand::Imm(val) => format!("${}", val),
        Operand::Data(var) => format!("{}(%rip)", var),
        Operand::Pseudo(_) => {
            panic!("No Pseudo-registers should be in tree when outputing assembly")
        }
    }
}

fn show_condition(cond: &Condition) -> String {
    match cond {
        Condition::E => "e".to_string(),
        Condition::NE => "ne".to_string(),
        Condition::G => "g".to_string(),
        Condition::GE => "ge".to_string(),
        Condition::L => "l".to_string(),
        Condition::LE => "le".to_string(),
    }
}

fn emit_stack_note<W: Write>(writer: &mut W) -> IOResult {
    writeln!(writer, ".section .note.GNU-stack,\"\",@progbits")?;
    Ok(())
}
