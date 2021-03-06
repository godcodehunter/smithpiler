use crate::ast::{expr, stmt, decl, self, r#type};
use super::parser::*;
use std::mem::MaybeUninit;

macro_rules! impl_parser {
    ($parse_rule: ident, $func: ident) => {
        pub struct $parse_rule<'input>(
            Box<std::mem::MaybeUninit<ParseState<'input>>>
        );
        
        impl<'input> Parser<'input> for $parse_rule<'input> {
            type ResultValue = stmt::Statement;
            const ROOT_RULE: fn(&mut ParseState<'input>)->ParseResult<Self::ResultValue> = $func;
        
            fn new() -> Self {
                Self(Box::new_uninit())
            }
        
            fn parse(&mut self, input: &'input str) -> ParseResult<Self::ResultValue> {
                unsafe { 
                    (*self.0).write(ParseState::new(input)); 
                    Self::ROOT_RULE((*self.0).assume_init_mut())
                }
            }
        }
    };
}

pub trait Parser<'input> {
    type ResultValue;
    const ROOT_RULE: fn(&mut ParseState<'input>)->ParseResult<Self::ResultValue>;

    fn new() -> Self;
    fn parse(&mut self, input: &'input str) -> ParseResult<Self::ResultValue>;
}

impl_parser!(StatementParser, parse_statement);
impl_parser!(IterationStatementParser, parse_iteration_statement);
impl_parser!(JumpStatementParser, parse_jump_statement);


