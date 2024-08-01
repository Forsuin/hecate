use lir::*;
use mir::tacky;

use crate::fix_instructions::fix_invalid_instructions;
use crate::replace_pseudoregisters::PseudoReplacer;

mod fix_instructions;
mod replace_pseudoregisters;

pub fn gen_assm(tacky: &tacky::TranslationUnit) -> Program {
    match &tacky {
        &tacky::TranslationUnit { func } => {
            let prog = Program {
                func: gen_func(&func),
            };

            let mut replaced = PseudoReplacer::replace_psuedos(&prog);

            fix_invalid_instructions(&mut replaced.0, replaced.1)
        },
    }
}

fn gen_func(func: &tacky::Func) -> Function {
    Function {
        name: func.name.clone(),
        instructions: gen_instructions(&func.instructions),
    }
}

fn gen_instructions(instructions: &Vec<tacky::Instruction>) -> Vec<Instruction> {
    let mut assm_instr = vec![];

    for i in instructions {
        match i {
            tacky::Instruction::Return(val) => {
                assm_instr.push(Instruction::Mov {
                    src: gen_operand(val),
                    dest: Operand::Register(Register::AX),
                });
                assm_instr.push(Instruction::Ret);
            }
            tacky::Instruction::Unary { op, src, dest } => {
                assm_instr.push(Instruction::Mov {
                    src: gen_operand(src),
                    dest: gen_operand(dest),
                });
                assm_instr.push(Instruction::Unary {
                    op: gen_unary(op),
                    dest: gen_operand(dest),
                });
            },
            tacky::Instruction::Binary { op, first, second, dest } => {
                if matches!(op, tacky::BinaryOp::Divide | tacky::BinaryOp::Modulo) {
                    assm_instr.push(Instruction::Mov {
                        src: gen_operand(first),
                        dest: Operand::Register(Register::AX)
                    });
                    assm_instr.push(Instruction::Cdq);
                    assm_instr.push(Instruction::Idiv(gen_operand(second)));
                    assm_instr.push(Instruction::Mov {
                        src: Operand::Register(if *op == tacky::BinaryOp::Divide { Register::AX } else { Register::DX }),
                        dest: gen_operand(dest),
                    })
                } else {
                    assm_instr.push(Instruction::Mov {
                        src: gen_operand(first),
                        dest: gen_operand(dest),
                    });
                    assm_instr.push(Instruction::Binary {
                        op: gen_binary(op),
                        src: gen_operand(second),
                        dest: gen_operand(dest),
                    })
                }
            }
        }
    }

    assm_instr
}

fn gen_unary(operator: &tacky::UnaryOp) -> UnaryOp {
    match operator {
        tacky::UnaryOp::Complement => UnaryOp::Not,
        tacky::UnaryOp::Negate => UnaryOp::Neg,
    }
}

fn gen_binary(operator: &tacky::BinaryOp) -> BinaryOp {
    match operator {
        tacky::BinaryOp::Add => BinaryOp::Add,
        tacky::BinaryOp::Subtract => BinaryOp::Sub,
        tacky::BinaryOp::Multiply => BinaryOp::Mult,
        _ => panic!("Unable to convert {:#?} into assembly BinaryOp", operator),
    }
}

fn gen_operand(operand: &tacky::Val) -> Operand {
    match operand {
        tacky::Val::Constant(val) => Operand::Imm(*val),
        tacky::Val::Var(var) => Operand::Pseudo(var.clone()),
    }
}