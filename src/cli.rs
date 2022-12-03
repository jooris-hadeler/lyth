use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name="lythc", about="Compiler for the lyth programming language.")]
pub struct Options {
    /// nput file to build
    pub input: String,
    /// Output file 
    #[structopt(long, short="o")]
    pub output: Option<String>,
}