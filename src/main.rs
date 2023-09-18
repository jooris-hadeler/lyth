use std::{env, fs};
use std::process::exit;

mod lexer;

use lexer::lex;

mod parser;

use parser::{Parser, ParserError};
use crate::lexer::{Location, TokenKind};

fn calculate_pos(loc: &Location, content: &str) -> (usize, usize) {
    let line = content.chars()
        .take(loc.start)
        .filter(|p| *p == '\n')
        .count() + 1;

    let col = &content[0..loc.start]
        .rsplit('\n')
        .next().unwrap()
        .chars()
        .count() + 1;

    (line, col)
}

fn print_usage(executable: String) {
    eprintln!("usage: {executable} <subcommand>\n");
    eprintln!("SUBCOMMANDS:");
    eprintln!("\tparse <file>");
}

fn main() {
    let mut args = env::args();

    let executable = args.next().unwrap();

    match args.next() {
        Some(string) => match string.as_str() {
            "parse" => {
                let file = args.next().unwrap_or_else(|| {
                    print_usage(executable);
                    exit(2);
                });

                let content = fs::read_to_string(&file).unwrap();

                if let Ok(tokens) = lex(&file, content.chars().peekable()) {
                    // Simple lexer error handling
                    {
                        let lexer_errors = tokens
                            .iter()
                            .filter(|tok| matches!(tok.kind, TokenKind::Illegal(..)));

                        let mut abort = false;

                        for error in lexer_errors {
                            abort = true;

                            let (line, col) = calculate_pos(&error.loc, &content);

                            match &error.kind {
                                TokenKind::Illegal(msg) => eprintln!("ERROR: {}:{}:{}: {}", error.loc.file, line, col, msg),
                                _ => unreachable!()
                            }
                        }

                        if abort {
                            exit(0);
                        }
                    }

                    let mut parser = Parser::new(tokens, file.into());

                    let ast = parser.parse_module();

                    match ast {
                        Ok(module) => println!("{:#?}", module),
                        Err(error) => match error {
                            ParserError::Message(msg) => eprintln!("ERROR: {msg}"),
                            ParserError::LocatedError { loc, msg } => {
                                let (line, col) = calculate_pos(&loc, &content);

                                eprintln!("ERROR: {}:{}:{}: {}", loc.file, line, col, msg);
                            }
                        }
                    }
                }
            }
            cmd => {
                eprintln!("ERROR: unknown subcommand `{cmd}`");
                print_usage(executable);
                exit(1);
            }
        }
        _ => {
            print_usage(executable);
            exit(1);
        }
    }
}
