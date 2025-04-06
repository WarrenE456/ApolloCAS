
use std::env::args;
use std::process::exit;
use std::fs::read_to_string;
use std::io::{stdin, stdout, Write};
use std::sync::{Arc, RwLock};

use crate::scanner::Scanner;
use crate::parser::Parser;
use crate::interpreter::Interpreter;

fn curly_braces_closed(s: &String) -> bool {
    let s = s.as_bytes();
    let num_lparen = s.iter().fold(0, |acc, x| if *x == b'{' { acc + 1 } else { acc });
    let num_rparen = s.iter().fold(0, |acc, x| if *x == b'}' { acc + 1 } else { acc });
    num_lparen == num_rparen
}

pub struct Apollo<'a> {
    global: Arc<RwLock<Interpreter<'a>>>,
}

impl<'a> Apollo<'a> {
    pub fn from(global: Arc<RwLock<Interpreter<'a>>>) -> Self {
        Self { global }
    }
    fn repl(&self) {
        macro_rules! handle_error {
            ($x:expr, $l:expr) => {
                match $x {
                    Ok(line) => line,
                    Err(e) => {
                        e.display($l);
                        continue;
                    }
                }
            }
        }
        let mut lines: Vec<String> = Vec::new();
        let mut prev_line_count = 0;
        loop {
            let mut program = String::new();
            let mut num_new_lines = 0;

            loop {
                let mut line = String::new();

                if num_new_lines == 0 {
                    print!(">>>\t");
                } else {
                    print!("...\t");
                }
                let _ = stdout().flush();

                stdin().read_line(&mut line).unwrap();
                num_new_lines += 1;

                program += &line;
                lines.push(line);

                if curly_braces_closed(&program) {
                    break;
                }
            }
            
            let scanner = Scanner::from(program.as_bytes(), prev_line_count + 1);
            prev_line_count += num_new_lines;

            let toks = handle_error!(scanner.scan(), &lines);

            let parser = Parser::new(toks);
            let statement = handle_error!(parser.parse_line(), &lines);
            
            {
                let interpreter = self.global.read().unwrap();
                let val = handle_error!(interpreter.interpret(statement), &lines);
                if let Some(val) = val {
                    println!("{}", val);
                }
            }
        }
    }
    fn run_file(&self, args: Vec<String>) {
        macro_rules! handle_error {
            ($x:expr, $l:expr) => {
                match $x {
                    Ok(line) => line,
                    Err(e) => {
                        e.display($l);
                        exit(1);
                    }
                }
            }
        }

        let file_path = &args[1];
        let program = match read_to_string(file_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error: Failed to read file '{}'. {}", file_path, e.to_string()); 
                exit(1);
            }
        } + "\n";

        let program_lines = program
            .split("\n")
            .map(|s| s.to_owned())
            .collect::<Vec<_>>();
        let program = program.as_bytes();

        let scanner = Scanner::new(program);
        let toks = handle_error!(scanner.scan(), &program_lines);

        let parser = Parser::new(toks);
        let lines = handle_error!(parser.parse(), &program_lines);

        for line in lines.into_iter() {
            let interpreter = self.global.read().unwrap();
           handle_error!(interpreter.interpret(line), &program_lines); 
        }
    }
    pub fn run(&self) {
        let args = args().collect::<Vec<_>>();
        if args.len() == 1 {
            self.repl();
        }
        else if args.len() == 2 {
            self.run_file(args);
        }
        else {
            eprintln!("Usage: apallo-cas [path/to/file]");
            exit(1);
        }

    }
}
