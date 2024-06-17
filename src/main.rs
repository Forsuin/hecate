use clap::{Args, Parser};
use std::env;

#[derive(Parser, Debug)]
#[command(version, about, long_about = "Runs the Hecate C compiler")]
struct CLI {
    /// "Specifies a point in compilation process for Hecate to stop, only one(1) option can be specified at a time"
    #[command(flatten)]
    stop_stage: StopStage,
}

/// Run C compiler with optional arguments
#[derive(Args, Debug)]
#[group(required = false, multiple = false)]
struct StopStage {
    /// Stop after lexer
    #[arg(long)]
    lex: bool,

    /// Stop after parser
    #[arg(long)]
    parse: bool,

    /// Stop after assembly generation
    #[arg(long)]
    codegen: bool,

    /// Stop after emitting assembly
    #[arg(short = 'S')]
    s: bool,
}

fn main() {
    let args = CLI::parse();

    let lex = args.stop_stage.lex;
    let parse = args.stop_stage.parse;
    let codegen = args.stop_stage.codegen;
    let assembly = args.stop_stage.s;

    print!(
        "Lex: {}, Parse: {}, Codegen: {}, Assembly: {}",
        lex, parse, codegen, assembly
    );
}
