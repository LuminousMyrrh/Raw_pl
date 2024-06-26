mod scan;
mod parser;
mod utils;
mod run_in_time;

use run_in_time::environment::Environment;
use parser::parser::Parser;
use run_in_time::runtime::eval_program;

use scan::scanner::Scanner;

use std::env;
use std::fs;

fn read_file() -> String {
    // Get the path to the file from the command line argument
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
    let mut scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner.scan_tokens());
    let mut env = Environment::new(None);

    parser.parse().unwrap();
    eval_program(parser.parse().unwrap(), &mut env).unwrap();
}

fn main() {
    run()
}
