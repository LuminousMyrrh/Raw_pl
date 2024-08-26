mod scan;
mod lexing;
mod utils;
mod engine;

use lexing::parser::Parser;
use engine::runtime::eval_program;
use engine::environment::Environment;

use scan::scanner::Scanner;

use std::env;
use std::fs;

fn read_file() -> String {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("USAGE: {} <file_path>", args[0]);
        std::process::exit(1);
    }
    let file_path = &args[1];
    match fs::read_to_string(file_path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error reading file {}: {}", file_path, e);
            std::process::exit(1);
        }
    }
}

fn run() {
    let source = read_file();
    let mut scanner = Scanner::new(source.clone());
    let mut parser = Parser::new(scanner.scan_tokens());
    parser.parse().unwrap();
    //let mut env = Environment::new(None);

    //eval_program(parser.parse().unwrap(), &mut env).unwrap();
}

fn main() {
    run()
}
