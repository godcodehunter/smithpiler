use crate::ast::{expr::Expression, stmt::Statement, decl, self, r#type};
use super::parser::*;
use std::mem::MaybeUninit;

macro_rules! impl_parser {
    ($parse_rule: ident, $result: ty, $func: ident) => {
        pub struct $parse_rule<'input>(
            Box<std::mem::MaybeUninit<ParseState<'input>>>
        );
        
        impl<'input> Parser<'input> for $parse_rule<'input> {
            type ResultValue = $result;
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

impl_parser!(TranslationUnitParser, Vec<ast::ExternalDeclaration>, parse_translation_unit);
impl_parser!(StatementParser, Statement, parse_statement);
impl_parser!(PostfixExpressionParser, Expression, parse_postfix_expression);
impl_parser!(ExpressionParser, Expression, parse_expression);
impl_parser!(IterationStatementParser, Statement, parse_iteration_statement);
impl_parser!(JumpStatementParser, Statement, parse_jump_statement);


