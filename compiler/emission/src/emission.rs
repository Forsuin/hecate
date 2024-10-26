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
        Decl::Func(func) => emit_func(writer, func, symbols),
        Decl::StaticVar(var) => emit_static_var(writer, var),
    }
}

fn emit_static_var<W: Write>(writer: &mut W, var: &StaticVar) -> IOResult {
    writeln!(writer, "# <static var: {}>", var.name)?;

    if var.global {
        writeln!(writer, "\t.globl {}", var.name)?;
    }

    // which section variable goes
    let init = match var.init {
        StaticInit::Int(0) => {
            writeln!(writer, "\t.bss")?;
            "\t.zero 4".to_string()
        }
        StaticInit::Int(i) => {
            writeln!(writer, "\t.data")?;
            format!("\t.long {}", i)
        }
        StaticInit::Long(0) => {
            writeln!(writer, "\t.bss")?;
            "\t.zero 8".to_string()
        }
        StaticInit::Long(i) => {
            writeln!(writer, "\t.data")?;
            format!("\t.quad {}", i)
        }
    };

    // alignment
    writeln!(writer, "\t.balign {}", var.alignment)?;

    writeln!(writer, "{}:", var.name)?;
    writeln!(writer, "{}", init)?;

    Ok(())
}

fn emit_func<W: Write>(writer: &mut W, func: &Func, symbols: &SymbolTable) -> IOResult {
    writeln!(writer, "\t.text")?;
    if func.global {
        writeln!(writer, "\t.globl {}", func.name)?;
    }
    writeln!(writer, "{}:", func.name)?;
    writeln!(writer, "\tpushq %rbp")?;
    writeln!(writer, "\tmovq %rsp, %rbp")?;

    for instruction in &func.instructions {
        emit_instruction(writer, instruction, symbols)?;
    }

    Ok(())
}

fn emit_instruction<W: Write>(
    writer: &mut W,
    instruction: &Instruction,
    symbols: &SymbolTable,
) -> IOResult {
    match instruction {
        Instruction::Mov { src, dest, ty } => writeln!(
            writer,
            "\tmov{} {}, {}",
            show_type(ty),
            show_operand(src, *ty),
            show_operand(dest, *ty)
        )?,
        Instruction::Movsx { src, dest } => {
            writeln!(
                writer,
                "\tmovslq {}, {}",
                show_operand(src, AssemblyType::Long),
                show_operand(dest, AssemblyType::Quad)
            )?;
        }
        Instruction::Ret => {
            writeln!(writer, "\tmovq %rbp, %rsp")?;
            writeln!(writer, "\tpopq %rbp")?;
            writeln!(writer, "\tret")?
        }
        Instruction::Unary { op, dest, ty } => {
            writeln!(
                writer,
                "\t{}{} {}",
                show_unary(op),
                show_type(ty),
                show_operand(dest, *ty)
            )?;
        }
        Instruction::Binary {
            op: op @ BinaryOp::Sal | op @ BinaryOp::Sar,
            src,
            dest,
            ty,
        } => {
            writeln!(
                writer,
                "\t{}{} {}, {}",
                show_binary(op),
                show_type(ty),
                show_byte_operand(src),
                show_operand(dest, *ty)
            )?;
        }
        Instruction::Binary { op, src, dest, ty } => {
            writeln!(
                writer,
                "\t{}{} {}, {}",
                show_binary(op),
                show_type(ty),
                show_operand(src, *ty),
                show_operand(dest, *ty)
            )?;
        }
        Instruction::Idiv(op, ty) => {
            writeln!(writer, "\tidiv{} {}", show_type(ty), show_operand(op, *ty))?;
        }
        Instruction::Cdq(AssemblyType::Long) => {
            writeln!(writer, "\tcdq")?;
        }
        Instruction::Cdq(AssemblyType::Quad) => {
            writeln!(writer, "\tcqo")?;
        }
        Instruction::Cmp(first, second, ty) => {
            writeln!(
                writer,
                "\tcmp{} {}, {}",
                show_type(ty),
                show_operand(first, *ty),
                show_operand(second, *ty)
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
            writeln!(writer, ".L_{}:", label)?;
        }
        Instruction::Push(op) => {
            writeln!(writer, "\tpushq {}", show_operand(op, AssemblyType::Quad))?;
        }
        Instruction::Call(func) => {
            writeln!(writer, "\tcall {}", show_fun_name(func, symbols))?;
        }
    }

    Ok(())
}

fn show_type(ty: &AssemblyType) -> String {
    (match ty {
            AssemblyType::Long => {
                "l"
            }
            AssemblyType::Quad => {
                "q"
            }
        }).to_string()
}

fn show_fun_name(name: &String, symbols: &SymbolTable) -> String {
    if symbols.get(name).is_some() {
        name.clone()
    } else {
        format!("{}@PLT", name)
    }
}

fn show_unary(op: &UnaryOp) -> String {
    match op {
        UnaryOp::Neg => "neg".to_string(),
        UnaryOp::Not => "not".to_string(),
    }
}

fn show_binary(op: &BinaryOp) -> String {
    match op {
        BinaryOp::Add => "add".to_string(),
        BinaryOp::Sub => "sub".to_string(),
        BinaryOp::Mult => "imul".to_string(),
        BinaryOp::And => "and".to_string(),
        BinaryOp::Or => "or".to_string(),
        BinaryOp::Xor => "xor".to_string(),
        BinaryOp::Sal => "sal".to_string(),
        BinaryOp::Sar => "sar".to_string(),
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
        Register::SP => unreachable!("Internal Error: No byte size RSP"),
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
            Register::SP => unreachable!("Internal Error: No 32-bit RSP"),
            
        }
    )
}

fn show_quad_reg(reg: &Register) -> String {
    format!(
        "%{}",
        match reg {
            Register::AX => "rax",
            Register::CX => "rcx",
            Register::DX => "rdx",
            Register::DI => "rdi",
            Register::SI => "rsi",
            Register::R8 => "r8",
            Register::R9 => "r9",
            Register::R10 => "r10",
            Register::R11 => "r11",
            Register::SP => "rsp"
        }
    )
}

fn show_byte_operand(op: &Operand) -> String {
    match op {
        Operand::Register(reg) => show_byte_reg(reg),
        _ => show_operand(op, AssemblyType::Long),
    }
}

fn show_operand(op: &Operand, ty: AssemblyType) -> String {
    match op {
        Operand::Register(reg) => {
            match ty {
                AssemblyType::Long => show_reg(reg),
                AssemblyType::Quad => show_quad_reg(reg),
            }
        },
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
