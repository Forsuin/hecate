use crate::{Expr, Func, Function, Instruction, Operand, Program, Stmt, TranslationUnit};
use std::fs::File;
use std::io::Write;

pub fn gen_assm(ast: &TranslationUnit) -> Program {
    match &ast {
        &TranslationUnit { func } => Program {
            func: gen_func(&func),
        },
    }
}

fn gen_func(func: &Func) -> Function {
    Function {
        name: func.ident.clone(),
        instructions: gen_stmts(&func.body),
    }
}

fn gen_stmts(stmt: &Stmt) -> Vec<Instruction> {
    let expr = gen_expr(&stmt.expr);
    vec![
        Instruction::Mov {
            src: expr,
            dest: Operand::Register,
        },
        Instruction::Ret,
    ]
}

fn gen_expr(expr: &Expr) -> Operand {
    Operand::Imm(expr.val)
}

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
            show_operand(&src),
            show_operand(&dest)
        )?,
        Instruction::Ret => writeln!(writer, "\tret")?,
    }

    Ok(())
}

fn show_operand(op: &Operand) -> String {
    match op {
        Operand::Register => "%eax".to_string(),
        Operand::Imm(val) => format!("${}", val),
    }
}

fn emit_stack_note<W: Write>(writer: &mut W) -> std::io::Result<()> {
    writeln!(writer, ".section .note.GNU-stack,\"\",@progbits")?;
    Ok(())
}
