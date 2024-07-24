use crate::{assembly, tacky};

pub fn gen_assm(tacky: &tacky::TranslationUnit) -> assembly::Program {
    match &tacky {
        &tacky::TranslationUnit { func } => assembly::Program {
            func: gen_func(&func),
        },
    }
}

fn gen_func(func: &tacky::Func) -> assembly::Function {
    assembly::Function {
        name: func.name.clone(),
        instructions: gen_instructions(&func.instructions),
    }
}

fn gen_instructions(instructions: &Vec<tacky::Instruction>) -> Vec<assembly::Instruction> {
    let mut assm_instr = vec![];

    for i in instructions {
        match i {
            tacky::Instruction::Return(val) => {
                assm_instr.push(assembly::Instruction::Mov {
                    src: gen_operand(val),
                    dest: assembly::Operand::Register(assembly::Register::AX),
                });
                assm_instr.push(assembly::Instruction::Ret);
            }
            tacky::Instruction::Unary { op, src, dest } => {
                assm_instr.push(assembly::Instruction::Mov {
                    src: gen_operand(src),
                    dest: gen_operand(dest),
                });
                assm_instr.push(assembly::Instruction::Unary {
                    op: gen_unary(op),
                    dest: gen_operand(dest),
                });
            }
        }
    }

    assm_instr
}

fn gen_unary(operator: &tacky::UnaryOp) -> assembly::UnaryOp {
    match operator {
        tacky::UnaryOp::Complement => assembly::UnaryOp::Not,
        tacky::UnaryOp::Negate => assembly::UnaryOp::Neg,
    }
}

fn gen_operand(operand: &tacky::Val) -> assembly::Operand {
    match operand {
        tacky::Val::Constant(val) => assembly::Operand::Imm(*val),
        tacky::Val::Var(var) => assembly::Operand::Pseudo(var.clone()),
    }
}