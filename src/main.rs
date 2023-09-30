use std::fs;
use std::process::exit;
use clap::{Parser as ClapParser, Subcommand};

mod lexer;

use crate::lexer::{lex, Token};
use crate::lexer::{Location, TokenKind};

mod parser;
mod typesystem;

use crate::parser::{Parser, ParserError};

/// Lyth Compiler & Build Tool
#[derive(ClapParser, Debug)]
#[command(author, version, about)]
struct Cli {
    #[command(subcommand)]
    command: SubCommands,

    /// Enable more runtime information
    #[arg(short, long, default_value_t = false)]
    verbose: bool,
}

#[derive(Subcommand, Debug)]
enum SubCommands {
    /// Lex a single file and print out the tokens
    Lex {
        /// The file to lex
        file: String
    },
    /// Parse a single file and print out the ast
    Parse {
        /// The file to parse
        file: String
    },
    /// Compile a single file
    Compile {
        /// The file to compile
        file: String,
        /// The output file path
        #[arg(short, long, default_value = "out")]
        output: Option<String>,
    },
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
        let log_error = |error: &Token| {
            let (line, col) = calculate_pos(&error.loc, &content);
            let msg = error.kind.clone().unwrap_illegal();

            eprintln!("ERROR: {}:{}:{}: {}", error.loc.file, line, col, msg)
        };

        let had_errors = tokens
            .iter()
            .filter(|tok| matches!(tok.kind, TokenKind::Illegal(..)))
            .map(log_error)
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
    let cli = Cli::parse();

    match cli.command {
        SubCommands::Lex { file } => lex_file(file),
        SubCommands::Parse { file } => parse_file(file),
        SubCommands::Compile { file, output } => println!("{file} -> {output:?}")
    }
}
