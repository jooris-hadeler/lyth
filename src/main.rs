use std::fs;
use std::process::exit;
use clap::{Parser as ClapParser, Subcommand};

mod lexer;

use lexer::lex;

mod parser;

use parser::{Parser, ParserError};
use crate::lexer::{Location, TokenKind};

#[derive(ClapParser, Debug)]
#[command(author, version, about)]
struct CliParameters {
    #[command(subcommand)]
    command: CliCommand,
}

#[derive(Subcommand, Debug)]
enum CliCommand {
    Lex { file: String },
    Parse { file: String },
}

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
    eprintln!("\tlex <file>");
    eprintln!("\tparse <file>");
}

fn lex_file(file: String) {
    let content = fs::read_to_string(&file).unwrap();

    let tokens = lex(&file, content.chars().peekable());
    for token in tokens {
        println!("{:?}", token);
    }
}

fn parse_file(file: String) {
    let content = fs::read_to_string(&file).unwrap();

    let tokens = lex(&file, content.chars().peekable());
    // Simple lexer error handling
    {
        let had_errors = tokens
            .iter()
            .filter(|tok| matches!(tok.kind, TokenKind::Illegal(..)))
            .map(|error| {
                let (line, col) = calculate_pos(&error.loc, &content);

                match &error.kind {
                    TokenKind::Illegal(msg) => eprintln!("ERROR: {}:{}:{}: {}", error.loc.file, line, col, msg),
                    _ => unreachable!()
                }
            })
            .count() > 0;

        if had_errors {
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

fn main() {
    let cli = CliParameters::parse();

    match cli.command {
        CliCommand::Lex { file } => lex_file(file),
        CliCommand::Parse { file } => parse_file(file),
    }
}
