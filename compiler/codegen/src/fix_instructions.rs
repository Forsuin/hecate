use lir::*;

pub fn fix_invalid_instructions(ast: &mut Program, stack_size: i32) -> Program {
    let func = &mut ast.func;

    func.instructions.insert(0, Instruction::AllocateStack(stack_size));

    Program {
        func: fix_func(func),
    }

}

fn fix_func(func: &Function) -> Function {
    Function {
        name: func.name.clone(),
        instructions: fix_instructions(&func.instructions),
    }
}

fn fix_instructions(instructions: &Vec<Instruction>) -> Vec<Instruction> {
    let mut fixed_instr = vec![];

    for i in instructions {
        match i {
            Instruction::Mov {src, dest} => {
                fixed_instr.push(Instruction::Mov {src: src.clone(), dest: Operand::Register(Register::R10)});
                fixed_instr.push(Instruction::Mov {src: Operand::Register(Register::R10), dest: dest.clone()});
            },
            _ => {
                fixed_instr.push(i.clone())
            }
        }
    }

    fixed_instr
}