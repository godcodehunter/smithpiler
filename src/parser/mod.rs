#![allow(warnings)]
mod lexer;
mod parser;
mod public_parser;
#[cfg(test)]
mod tests;
use crate::ast;

use std::{path::Path, io::Read};
use std::fs::File;

pub struct TranslationUnit {
    ast: ast::Ast,
    file: String,
}

impl std::fmt::Display for TranslationUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // f.write_fmt(format_args!("{} {}", self.file, self.ast))
        todo!()
    }
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
    
    //TODO TranslationUnit
    pub fn parse(&self, path: &str) -> Result<(), Box<dyn std::error::Error>> {   
        let mut file = File::open(Path::new(path))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        let input = contents.as_str();
        let tok_iter = lexer::LexerState::new(input);
        tok_iter.dump_lexer();
    //     // let ast = parser::TranslationUnitParser::new().parse(input, tok_iter)?;
    //     // if self.options.dump_ast {
    //     //     println!("{}", ast);
    //     // }
    //     // Ok(TranslationUnit{ast, file: path.into()})
        todo!()
    }

}
