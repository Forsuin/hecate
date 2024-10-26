use lir::symbol_table::AsmTable;
use lir::*;
use mir::tacky;
use mir::tacky::Val;
use ty::{get_alignment, IdentifierAttr, Symbol, SymbolTable, Type};

use crate::fix_instructions::fix_invalid_instructions;
use crate::replace_pseudoregisters::replace_psuedos;

mod fix_instructions;
mod replace_pseudoregisters;

macro_rules! tb {
    ($variant:ident) => {
        tacky::BinaryOp::$variant
    };
    ($head:ident | $($tail:ident)|+) => {
        tb!($head) | tb!($($tail)|+)
    };
}

const PARAM_PASSING_REGS: [Register; 6] = [
    Register::DI,
    Register::SI,
    Register::DX,
    Register::CX,
    Register::R8,
    Register::R9,
];

fn convert_symbol_table(symbol_table: &SymbolTable) -> AsmTable {
    let mut asm_table = AsmTable::new();

    for (name, symbol) in &symbol_table.symbols {
        match symbol {
            Symbol {
                t: Type::Func(_),
                attrs: IdentifierAttr::Func { defined, .. },
            } => {
                asm_table.add_func(name.clone(), *defined);
            }
            Symbol {
                t,
                attrs: IdentifierAttr::Static { .. },
            } => asm_table.add_obj(name.clone(), convert_type(t.clone()), true),
            Symbol { t, .. } => asm_table.add_obj(name.clone(), convert_type(t.clone()), false),
        }
    }

    asm_table
}

pub fn gen_assm(tacky: &tacky::TranslationUnit, symbol_table: &mut SymbolTable) -> Program {
    let mut decls = vec![];

    for decl in &tacky.decls {
        match decl {
            tacky::Decl::Func(func) => decls.push(Decl::Func(gen_func(func, symbol_table))),
            tacky::Decl::StaticVar(var) => decls.push(Decl::StaticVar(gen_static_var(var))),
        }
    }

    let prog = Program { decls };

    let mut symbol_table = convert_symbol_table(symbol_table);

    let mut replaced = replace_psuedos(&prog, &mut symbol_table);

    fix_invalid_instructions(&mut replaced, &mut symbol_table)
}

fn gen_func(func: &tacky::Func, symbols: &SymbolTable) -> Func {
    let mut instructions = gen_params(&func.params, symbols);
    instructions.append(&mut gen_instructions(&func.instructions, symbols));

    Func {
        name: func.name.clone(),
        instructions,
        global: func.global,
    }
}

fn gen_static_var(var: &tacky::StaticVar) -> StaticVar {
    StaticVar {
        name: var.name.clone(),
        global: var.global,
        alignment: get_alignment(&var.ty),
        init: var.init.clone(),
    }
}

fn gen_params(params: &Vec<String>, symbols: &SymbolTable) -> Vec<Instruction> {
    let try_split = params.split_at_checked(6);

    let (register_params, stack_params) = if let Some(split) = try_split {
        (split.0, split.1)
    } else {
        (params.as_slice(), &[] as &[String])
    };

    let mut instructions = vec![];

    // pass in registers
    for (i, param) in register_params.iter().enumerate() {
        let reg = &PARAM_PASSING_REGS[i];

        instructions.push(Instruction::Mov {
            src: Operand::Register(reg.clone()),
            dest: Operand::Pseudo(param.clone()),
            ty: get_assm_type(&Val::Var(param.clone()), symbols),
        })
    }

    // pass on stack
    for (i, param) in stack_params.iter().enumerate() {
        instructions.push(Instruction::Mov {
            // stack has to grow up
            src: Operand::Stack(16 + (8 * i) as i32),
            dest: Operand::Pseudo(param.clone()),
            ty: get_assm_type(&Val::Var(param.clone()), symbols),
        })
    }

    instructions
}

fn gen_instructions(
    instructions: &Vec<tacky::Instruction>,
    symbols: &SymbolTable,
) -> Vec<Instruction> {
    let mut assm_instr = vec![];

    for i in instructions {
        match i {
            tacky::Instruction::Return(val) => {
                assm_instr.push(Instruction::Mov {
                    src: gen_operand(val),
                    dest: Operand::Register(Register::AX),
                    ty: get_assm_type(val, symbols),
                });
                assm_instr.push(Instruction::Ret);
            }
            tacky::Instruction::Unary {
                op: tacky::UnaryOp::Not,
                src,
                dest,
            } => {
                assm_instr.push(Instruction::Cmp(
                    Operand::Imm(0),
                    gen_operand(src),
                    get_assm_type(src, symbols),
                ));
                assm_instr.push(Instruction::Mov {
                    src: Operand::Imm(0),
                    dest: gen_operand(dest),
                    ty: get_assm_type(dest, symbols),
                });
                assm_instr.push(Instruction::SetCond {
                    condition: Condition::E,
                    dest: gen_operand(dest),
                });
            }
            tacky::Instruction::Unary { op, src, dest } => {
                assm_instr.push(Instruction::Mov {
                    src: gen_operand(src),
                    dest: gen_operand(dest),
                    ty: get_assm_type(src, symbols),
                });
                assm_instr.push(Instruction::Unary {
                    op: gen_unary(op),
                    dest: gen_operand(dest),
                    ty: get_assm_type(src, symbols),
                });
            }
            tacky::Instruction::Binary {
                op,
                first,
                second,
                dest,
            } => {
                if matches!(op, tacky::BinaryOp::Divide | tacky::BinaryOp::Modulo) {
                    assm_instr.push(Instruction::Mov {
                        src: gen_operand(first),
                        dest: Operand::Register(Register::AX),
                        ty: get_assm_type(first, symbols),
                    });
                    assm_instr.push(Instruction::Cdq(get_assm_type(first, symbols)));
                    assm_instr.push(Instruction::Idiv(
                        gen_operand(second),
                        get_assm_type(first, symbols),
                    ));
                    assm_instr.push(Instruction::Mov {
                        src: Operand::Register(if *op == tacky::BinaryOp::Divide {
                            Register::AX
                        } else {
                            Register::DX
                        }),
                        dest: gen_operand(dest),
                        ty: get_assm_type(first, symbols),
                    })
                } else if matches!(
                    op,
                    tacky::BinaryOp::BitshiftLeft | tacky::BinaryOp::BitshiftRight
                ) {
                    match second {
                        Val::Constant(_) => {
                            assm_instr.push(Instruction::Mov {
                                src: gen_operand(first),
                                dest: gen_operand(dest),
                                ty: get_assm_type(first, symbols),
                            });
                            assm_instr.push(Instruction::Binary {
                                op: gen_binary(op),
                                src: gen_operand(second),
                                dest: gen_operand(dest),
                                ty: get_assm_type(first, symbols),
                            });
                        }
                        _ => {
                            assm_instr.push(Instruction::Mov {
                                src: gen_operand(first),
                                dest: gen_operand(dest),
                                ty: get_assm_type(first, symbols),
                            });
                            assm_instr.push(Instruction::Mov {
                                src: gen_operand(second),
                                dest: Operand::Register(Register::CX),
                                ty: get_assm_type(first, symbols),
                            });
                            assm_instr.push(Instruction::Binary {
                                op: gen_binary(op),
                                src: Operand::Register(Register::CX),
                                dest: gen_operand(dest),
                                ty: get_assm_type(first, symbols),
                            });
                        }
                    }
                } else if matches!(
                    op,
                    tb!(Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual)
                ) {
                    assm_instr.push(Instruction::Cmp(
                        gen_operand(second),
                        gen_operand(first),
                        get_assm_type(first, symbols),
                    ));
                    assm_instr.push(Instruction::Mov {
                        src: Operand::Imm(0),
                        dest: gen_operand(dest),
                        ty: get_assm_type(dest, symbols),
                    });
                    assm_instr.push(Instruction::SetCond {
                        condition: gen_cond(op),
                        dest: gen_operand(dest),
                    });
                } else {
                    assm_instr.push(Instruction::Mov {
                        src: gen_operand(first),
                        dest: gen_operand(dest),
                        ty: get_assm_type(first, symbols),
                    });
                    assm_instr.push(Instruction::Binary {
                        op: gen_binary(op),
                        src: gen_operand(second),
                        dest: gen_operand(dest),
                        ty: get_assm_type(first, symbols),
                    })
                }
            }
            tacky::Instruction::Copy { src, dest } => assm_instr.push(Instruction::Mov {
                src: gen_operand(src),
                dest: gen_operand(dest),
                ty: get_assm_type(src, symbols),
            }),
            tacky::Instruction::Jump { target } => assm_instr.push(Instruction::Jmp {
                label: target.clone(),
            }),
            tacky::Instruction::JumpIfZero { condition, target } => {
                assm_instr.push(Instruction::Cmp(
                    Operand::Imm(0),
                    gen_operand(condition),
                    get_assm_type(condition, symbols),
                ));
                assm_instr.push(Instruction::JmpCond {
                    condition: Condition::E,
                    label: target.clone(),
                });
            }
            tacky::Instruction::JumpIfNotZero { condition, target } => {
                assm_instr.push(Instruction::Cmp(
                    Operand::Imm(0),
                    gen_operand(condition),
                    get_assm_type(condition, symbols),
                ));
                assm_instr.push(Instruction::JmpCond {
                    condition: Condition::NE,
                    label: target.clone(),
                });
            }
            tacky::Instruction::Label(identifier) => {
                assm_instr.push(Instruction::Label(identifier.clone()))
            }
            tacky::Instruction::FunCall {
                func_name,
                args,
                dest,
            } => {
                let try_split = args.split_at_checked(6);

                let (reg_params, stack_params) = if let Some(split) = try_split {
                    (split.0, split.1)
                } else {
                    (args.as_slice(), &[] as &[Val])
                };

                let stack_padding = if stack_params.len() % 2 == 0 { 0 } else { 8 };

                if stack_padding != 0 {
                    assm_instr.push(Instruction::Binary {
                        op: BinaryOp::Sub,
                        src: Operand::Imm(stack_padding),
                        dest: Operand::Register(Register::SP),
                        ty: AssemblyType::Quad,
                    });
                }

                // Pass args in registers
                for (i, param) in reg_params.iter().enumerate() {
                    let reg = &PARAM_PASSING_REGS[i];
                    let arg = gen_operand(param);

                    assm_instr.push(Instruction::Mov {
                        src: arg,
                        dest: Operand::Register(reg.clone()),
                        ty: get_assm_type(param, symbols),
                    });
                }

                // Pass args on stack
                for param in stack_params.iter().rev() {
                    let arg = gen_operand(param);
                    
                    if matches!(arg, Operand::Register(_) | Operand::Imm(_)) {
                        assm_instr.push(Instruction::Push(arg));
                    } else {
                        let assm_type = get_assm_type(param, symbols);

                        match assm_type {
                            AssemblyType::Quad => {
                                assm_instr.push(Instruction::Push(arg));
                            }
                            AssemblyType::Long => {
                                assm_instr.push(Instruction::Mov {
                                    src: arg,
                                    dest: Operand::Register(Register::AX),
                                    ty: assm_type,
                                });
                                assm_instr.push(Instruction::Push(Operand::Register(Register::AX)));
                            }
                        }
                    }
                }

                assm_instr.push(Instruction::Call(func_name.clone()));

                let bytes_to_remove = 8 * stack_params.len() as i64 + stack_padding;

                if bytes_to_remove != 0 {
                    assm_instr.push(Instruction::Binary {
                        op: BinaryOp::Add,
                        src: Operand::Imm(bytes_to_remove),
                        dest: Operand::Register(Register::SP),
                        ty: AssemblyType::Quad,
                    });
                }
                assm_instr.push(Instruction::Mov {
                    src: Operand::Register(Register::AX),
                    dest: gen_operand(dest),
                    ty: get_assm_type(dest, symbols),
                });
            }
            tacky::Instruction::SignExtend { src, dest } => assm_instr.push(Instruction::Movsx {
                src: gen_operand(src),
                dest: gen_operand(dest),
            }),
            tacky::Instruction::Truncate { src, dest } => assm_instr.push(Instruction::Mov {
                src: gen_operand(src),
                dest: gen_operand(dest),
                ty: AssemblyType::Long,
            }),
        }
    }

    assm_instr
}

fn get_assm_type(val: &Val, symbols: &SymbolTable) -> AssemblyType {
    match val {
        Val::Constant(ty::Constant::Long(_)) => AssemblyType::Quad,
        Val::Constant(ty::Constant::Int(_)) => AssemblyType::Long,
        Val::Var(v) => {
            let t = match symbols.get(v) {
                None => {
                    panic!("Internal Error: '{}' not in symbol table", v);
                }
                Some(symbol) => symbol.t.clone(),
            };

            convert_type(t)
        }
    }
}

fn convert_type(ty: Type) -> AssemblyType {
    match ty {
        Type::Int => AssemblyType::Long,
        Type::Long => AssemblyType::Quad,
        Type::Func(_) => {
            panic!("Internal Error: Tried to convert function type into assembly type")
        }
    }
}

fn gen_unary(operator: &tacky::UnaryOp) -> UnaryOp {
    match operator {
        tacky::UnaryOp::Complement => UnaryOp::Not,
        tacky::UnaryOp::Negate => UnaryOp::Neg,
        // should never get here
        tacky::UnaryOp::Not => {
            panic!("Unable to directly convert TACKY 'Not' directly to assembly")
        }
    }
}

fn gen_binary(operator: &tacky::BinaryOp) -> BinaryOp {
    match operator {
        tacky::BinaryOp::Add => BinaryOp::Add,
        tacky::BinaryOp::Subtract => BinaryOp::Sub,
        tacky::BinaryOp::Multiply => BinaryOp::Mult,
        tacky::BinaryOp::BitwiseAnd => BinaryOp::And,
        tacky::BinaryOp::BitwiseOr => BinaryOp::Or,
        tacky::BinaryOp::BitwiseXor => BinaryOp::Xor,
        tacky::BinaryOp::BitshiftLeft => BinaryOp::Sal,
        tacky::BinaryOp::BitshiftRight => BinaryOp::Sar,
        _ => panic!("Unable to convert {:#?} into assembly BinaryOp", operator),
    }
}

fn gen_operand(operand: &Val) -> Operand {
    match operand {
        Val::Constant(ty::Constant::Int(val)) => Operand::Imm(*val as i64),
        Val::Constant(ty::Constant::Long(val)) => Operand::Imm(*val),
        Val::Var(var) => Operand::Pseudo(var.clone()),
    }
}

fn gen_cond(op: &tacky::BinaryOp) -> Condition {
    match op {
        tacky::BinaryOp::Equal => Condition::E,
        tacky::BinaryOp::NotEqual => Condition::NE,
        tacky::BinaryOp::Less => Condition::L,
        tacky::BinaryOp::LessEqual => Condition::LE,
        tacky::BinaryOp::Greater => Condition::G,
        tacky::BinaryOp::GreaterEqual => Condition::GE,
        _ => panic!("Internal Error: Not a condition operator: {:?}", op),
    }
}

fn round_away_from_zero(n: i32, x: i32) -> i32 {
    if x % n == 0 {
        x
    } else if x < 0 {
        x - n - (x % n)
    } else {
        x + n - (x % n)
    }
}
