use std::{fs::read_to_string, path::Path, process::Command};

use anyhow::{Ok, Result};
use clap::{Args, Parser as ClapParser};
use thiserror::Error;

use hecate::{gen_assm, Lexer, Parser, TokenType};

#[derive(ClapParser, Debug)]
#[command(version, about, long_about = "Runs the Hecate C compiler")]
struct CLI {
    /// Path to C source file
    path: String,

    /// "Specifies a point in compilation process for Hecate to stop, only one(1) option can be specified at a time"
    #[command(flatten)]
    stage_options: StageOptions,
}

/// Run C compiler with optional arguments
#[derive(Args, Debug)]
#[group(required = false, multiple = false)]
struct StageOptions {
    /// Stop after lexer
    #[arg(long)]
    lex: bool,

    /// Stop after parser
    #[arg(long)]
    parse: bool,

    /// Stop after assembly generation
    #[arg(long)]
    codegen: bool,

    /// Emit assemly file, but do not assemble or link it
    #[arg(short = 'S')]
    s: bool,
}

/// Which stage the compiler should stop at
enum StopStage {
    Lexer,
    Parser,
    CodeGen,
    Assembler,
}

impl StopStage {
    fn from_args(options: &StageOptions) -> Option<StopStage> {
        if options.lex {
            return Some(StopStage::Lexer);
        } else if options.parse {
            return Some(StopStage::Parser);
        } else if options.codegen {
            return Some(StopStage::CodeGen);
        } else if options.s {
            return Some(StopStage::Assembler);
        } else {
            return None;
        }
    }
}

fn main() -> Result<()> {
    let args = CLI::parse();

    let stop_stage = StopStage::from_args(&args.stage_options);

    run_driver(&args.path, &stop_stage)
}

fn run_driver(path: &str, stop_stage: &Option<StopStage>) -> Result<()> {
    let dir_path = Path::new(path);

    // source code should always be inside a directory, but better error handling can come later
    let dir = dir_path.parent().unwrap();
    let file_name = dir_path
        .file_stem()
        .unwrap()
        .to_owned()
        .into_string()
        .unwrap();

    let pp_name = format!("{}/{file_name}.i", dir.display());

    // Preprocess input
    Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(dir_path)
        .arg("-o")
        .arg(&pp_name)
        .output()
        .expect("Failed to execute preprocessor process");

    // compile
    let _compile_result = compile(&pp_name, stop_stage)?;

    //delete preprocessed file
    Command::new("sh")
        .arg("rm")
        .arg(&pp_name)
        .output()
        .expect("Failed to delete preprocessed file");

    // Assemble and link only if we don't stop during compilation
    if let Some(_) = stop_stage {
    } else {
        let assembly_path = format!("{}/{file_name}.s", dir.display());
        let output = format!("{}/{file_name}", dir.display());

        // Assemble and link
        Command::new("gcc")
            .arg(&assembly_path)
            .arg("-o")
            .arg(&output)
            .output()
            .expect("Failed to execute assembler and linker");

        //delete assembled file
        Command::new("sh")
            .arg("rm")
            .arg(&assembly_path)
            .output()
            .expect("Failed to delete assembly file");
    }

    Ok(())
}

/// Actually run our compiler stages: Lexer, Parser, Codegen
/// If no StopStage is specified, an assembly file is outputted with a ".s" extension
/// Only Lexer, Parser, and Codegen StopStages are used in this function
fn compile(path: &str, stop_stage: &Option<StopStage>) -> Result<()> {
    // This function will be responsible for actually deciding whether to output any files

    let source =
        read_to_string(path).expect(format!("Unable to read source file: {}", path).as_str());

    let mut lexer = Lexer::new(&source);

    let (tokens, errors): (Vec<_>, Vec<_>) = lexer
        .tokenize()
        .partition(|t| t.kind != TokenType::Unknown && t.kind != TokenType::InvalidIdent);

    if !errors.is_empty() {
        let mut error_msgs = Vec::new();

        for err in errors {
            error_msgs.push(format!(
                "{:?} at {}:{}:{}: '{}'",
                err.value,
                path.rsplit_once('/').unwrap().1,
                err.line,
                err.col,
                source[err.start..err.end].to_string()
            ));
        }

        return Err(CompileErr::Lexer(error_msgs).into());
    } else {
        // for t in &tokens {
        //     println!(
        //         "TokenType: {:<10}, Value: {:<15}, Lexeme: {:<15}, Location: {}:{}:{}",
        //         format!("{:?}", t.kind),
        //         format!("{:?}", t.value),
        //         format!("{:?}", source[t.start..t.end].to_string()),
        //         path.rsplit_once('/').unwrap().1,
        //         t.line,
        //         t.col,
        //     );
        // }
    }

    // for token in lexer.tokenize() {
    //     if token.kind != TokenType::Whitespace {
    //         println!("{}", token);
    //     } else {
    //         continue;
    //     }
    // }

    if let Some(StopStage::Lexer) = stop_stage {
        // tokenize() should have returned any error by now
        return Ok(());
    }

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    println!("AST:\n{:#?}", ast);

    if let Some(StopStage::Parser) = stop_stage {
        return Ok(());
    }

    let assm = gen_assm(&ast);

    println!("ASSM:\n{:#?}", assm);

    if let Some(StopStage::CodeGen) = stop_stage {
        return Ok(());
    }

    // codegen.output(output_path);

    Ok(())
}

#[allow(dead_code)]
#[derive(Error, Debug)]
enum CompileErr {
    #[error("Lexer encountered an error(s): {:#?}", .0)]
    Lexer(Vec<String>),
    #[error("Parser encountered an error: {:#?}", .0)]
    Parser(Vec<String>),
    #[error("Codegen encountered an error: {:#?}", .0)]
    CodeGen(Vec<String>),
}
