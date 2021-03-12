#![allow(warnings)]
mod lexer;
mod parser;
mod public_parser;
#[cfg(test)]
mod tests;

use std::{path::Path, io::Read};
use std::fs::File;
use parser::TranslationUnitParser;
use crate::parser::public_parser::Parser as _;
use lang_c::ast;

pub struct Parser {

}

impl Parser {
    pub fn new() -> Self {
        Self{}
    }
    
    pub fn dump_lexer<'source>(&self, path: &'source std::path::Path) -> Result<(), Box<dyn std::error::Error>> {
        let mut file = File::open(Path::new(path))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        lexer::LexerState::new(contents.as_str()).dump_lexer();
        Ok(())
    }

    pub fn parse<'source>(&self, path: &'source std::path::Path) -> Result<ast::TranslationUnit, Box<dyn std::error::Error>> {   
        let mut file = File::open(Path::new(path))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        let input = contents.as_str();
        let external_declarations = TranslationUnitParser::new().parse(input).unwrap();

        Ok(ast::TranslationUnit{external_declarations, source: path})
    }

}
