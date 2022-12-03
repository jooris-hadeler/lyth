use std::fs::read_to_string;

use structopt::StructOpt;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser);
mod ast;
mod cli;

#[cfg(test)]
mod test;

fn main() {
    let options = cli::Options::from_args();

    let content = read_to_string(&options.input).expect("couldn't read file");
    let func = parser::ModuleParser::new()
        .parse(&module_name(&options.input), &options.input, content.as_str())
        .unwrap();

    println!("{:#?}", func);
}

fn module_name(file: &str) -> String {
    file.to_string()
        .replace("/", ".")
        .strip_suffix(".ly")
        .expect("invalid file name")
        .to_string()
}
