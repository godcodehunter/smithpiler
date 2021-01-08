mod lexer;
mod parser;
// #[cfg(test)]
// mod tests;
use crate::ast;

use std::{path::Path, io::Read};
use std::fs::File;

pub struct TranslationUnit {
    ast: ast::Ast,
    file: String,
}

pub struct Options {
    pub dump_lexer: bool,
    pub dump_ast: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self{
            dump_lexer: false,
            dump_ast: false,
        }
    }
}

pub struct Parser {
    options: Options 
}

impl Parser {
    pub fn new(options: Option<Options>) -> Self {
        Self{options: options.or(Some(Default::default())).unwrap()}
    }
    
    pub fn parse(&self, path: &str) -> Result<TranslationUnit, Box<dyn std::error::Error>> {   
        let mut file = File::open(Path::new(path))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        let input = contents.as_str();
        let tok_iter = lexer::token_iter(input, self.options.dump_lexer);
        let ast = parser::TranslationUnitParser::new().parse(input, tok_iter)?;
        if self.options.dump_ast {
            println!("{}", ast);
        }
        Ok(TranslationUnit{ast, file: path.into()})
    }

}
