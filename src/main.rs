use std::fs::read_to_string;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub grammar);
mod ast;

#[cfg(test)]
mod test;

fn main() {
    let file = "test.ly";
    let mod_name = module_name(file);
    let content = read_to_string(file).expect("couldn't read file");

    let func = grammar::ModuleParser::new()
        .parse(mod_name.as_str(), file, content.as_str())
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
