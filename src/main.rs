use std::fs;

mod lexer;
use lexer::lex;

mod parser;
use parser::Parser;

fn main() {
    let file = "examples/test.ly";
    let content = fs::read_to_string(file).unwrap();

    if let Ok(tokens) = lex(file, content.chars().peekable()) {
        /*for tok in tokens {
            if let Token {
                kind: TokenKind::Illegal(error),
                ..
            } = tok
            {
                eprintln!("Error: {}", error);
                break;
            } else {
                println!("{:?}", tok.kind);
            }
        }*/

        let mut parser = Parser::new(tokens, file.into());

        let ast = parser.parse_module();

        println!("{:#?}", ast);
    }
}
