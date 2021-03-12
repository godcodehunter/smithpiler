use super::lexer;
use crate::ast::{expr, stmt, decl, self};
pub use super::public_parser::*;
use std::collections::VecDeque;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone)]
pub enum LexerResult {
    UndefinedToken(usize),
    EOF,
    Token(lexer::Token),
}

#[derive(Debug, Copy, Clone)]
pub struct MatchError {
    expected: lexer::Token,
    got: LexerResult,
}


#[derive(Debug, Clone)]
pub struct MatchOrError {
    expected: Vec<lexer::Token>,
    got: LexerResult,
}

#[derive(Debug, Clone)]
pub enum ParserError {
    MatchError(MatchError),
    MatchOrError(MatchOrError),
    MatchAlternativeError(Vec<ParserError>),
}

pub type ParseResult<T> = Result<T, ParserError>;
pub trait ParseFunction<'input, T> = Fn(&mut ParseState<'input>)->ParseResult<T>;

pub struct ParseState<'input> {
    pub(crate) iter: lexer::LexerState<'input>,
}

impl<'input> ParseState<'input> {
    pub(crate) fn new(input: &'input str) -> Self {
        Self {
            iter: lexer::LexerState::new(input),
        } 
    }

    fn next(&mut self) {
        self.iter.next_meaningful();
    }

    // Gets the next token and compares it to the expected one
    fn match_token(&mut self, expected: lexer::Token) -> Result<(), ParserError>{ 
        self.next();
        let item = self.iter.current_item;
        if item.is_none() {
            return Err(ParserError::MatchError(MatchError{expected, got: LexerResult::EOF}));
        }
        let token = item.unwrap().value; 
        if token == lexer::Token::Error {
            todo!()
            // return Err(ParserError::MatchError(MatchError{expected, got: LexerResult::UndefinedToken(lexer_item)}))
        }
        if token != expected {
            return Err(ParserError::MatchError(MatchError{expected, got: LexerResult::Token(token)}));
        }
        Ok(())
    }
    
    // Try call provided parse function wrap result in option
    // If parsing success shift token stream otherwise rollback it 
    fn optional_parse<T: std::fmt::Debug>(&mut self, func: impl ParseFunction<'input, T>) -> Option<T> {
        let checkpoint = self.iter.iter.position();
        let result = func(self);
        // if result.is_err() {
        //     self.iter.iter.rewind(checkpoint);
        //     if let Err(ParserError::MatchError(MatchError{got: LexerResult::UndefinedToken(_), expected: _})) = result {
        //         return result.map(|val| Some(val));
        //     };
        //     return Ok(None);
        // }
        result.ok()
    }

    /// Try call provided call function one by one until parse result unsuccess.
    /// After all call unsuccess call rollback token stream
    fn parse_any<T: std::fmt::Debug>(&mut self, alternatives: &[Box<dyn ParseFunction<'input, T>>]) -> ParseResult<T> {
        let mut errors = Vec::<ParserError>::new();
        for alternative in alternatives {
            let checkpoint = self.iter.iter.position();
            let result = alternative(self);
            if result.is_ok() {
                return result;
            }
            self.iter.iter.rewind(checkpoint);
            errors.push(result.unwrap_err());
        }
        Err(ParserError::MatchAlternativeError(errors))
    }

    fn expect_zero_or_n<T, F: ParseFunction<'input, T>>(&mut self, func: F) -> Vec<T> {
        let mut result = Vec::<T>::new();
        loop {
            let value = func(self);
            if value.is_err() { 
                break; 
            }
            result.push(value.unwrap());
        }
        result
    }
    
    fn expect_one_or_n<T, F: ParseFunction<'input, T>>(&mut self, func: F) -> ParseResult<Vec<T>>  {
        let res = Self::expect_zero_or_n(self, func);
        if res.is_empty() {
            todo!()
        }
        Ok(res)
    }
}

fn parse_identifier(parser: &mut ParseState) -> ParseResult<String> {
    parser.match_token(lexer::Token::Identifier)?;
    Ok(parser.iter.stringify_current_token())
}

fn parse_typedef_name(parser: &mut ParseState) -> ParseResult<String> {
    parser.match_token(lexer::Token::Identifier)?;
    Ok(parser.iter.stringify_current_token())
}

fn parse_constant(parser: &mut ParseState) -> ParseResult<String>{
    parser.match_token(lexer::Token::Constant)?;
    Ok(parser.iter.stringify_current_token())
}

fn parse_string_literal(parser: &mut ParseState) -> ParseResult<String> {
    parser.match_token(lexer::Token::StringLiteral)?;
    Ok(parser.iter.stringify_current_token())
}

// Parser Rules

// External Definitions Rules

pub (crate) fn parse_translation_unit(parser: &mut ParseState) -> ParseResult<Vec<ast::ExternalDeclaration>> {
    // parser.expect_zero_or_n(parse_external_declaration)
    todo!()
}

fn parse_external_declaration(parser: &mut ParseState) -> ParseResult<ast::ExternalDeclaration> {
    parser.parse_any(&[
        Box::new(|iter| {
            let result = parse_function_definition(iter);
            result.map(|fdef| ast::ExternalDeclaration::FunctionDefinition(Box::new(fdef)))
        }),
        Box::new(|iter| {
            let result = parse_declaration(iter);
            result.map(|decl| ast::ExternalDeclaration::Declaration(Box::new(decl)))
        }),
    ])
}

fn parse_declaration_list(parser: &mut ParseState) -> ParseResult<Vec<decl::Declaration>> {
    parser.expect_one_or_n(parse_declaration)
}

fn parse_function_definition(parser: &mut ParseState) -> ParseResult<ast::FunctionDefinition> {
    let specifiers = parse_declaration_specifiers(parser)?;
    let declarator = parse_declarator(parser)?;
    let declaration_list = parser.optional_parse(parse_declaration_list); 
    let compound_statement = parse_compound_statement(parser)?;

    Ok(ast::FunctionDefinition::new(specifiers, declarator, declaration_list, compound_statement))
}

// Declarations Rules

fn parse_declaration(parser: &mut ParseState) -> ParseResult<decl::Declaration> {
    parser.parse_any(&[
        Box::new(|iter| {
            let declaration = parse_declaration_specifiers(iter)?; 
            let init = iter.optional_parse(parse_init_declarator_list); 
            iter.match_token(lexer::Token::Semicolon)?;
            Ok(decl::Declaration::Other(decl::Other{specifiers: declaration, init_declarators: init}))
        }),
        Box::new(|iter| {
            let result = parse_static_assert_declaration(iter)?;
            Ok(decl::Declaration::StaticAssert(result))
        }),
    ])
}

fn parse_declaration_specifiers(parser: &mut ParseState) -> ParseResult<Vec<decl::DeclarationSpecifier>> {
    parser.expect_one_or_n(|iter: &mut ParseState| {
        iter.parse_any(&[Box::new(|iter| {
                let storage_class_specifier = parse_storage_class_specifier(iter)?;
                Ok(decl::DeclarationSpecifier::StorageClassSpecifier(storage_class_specifier))
            }),
            Box::new(|iter| {
                let type_specifier = parse_type_specifier(iter)?;
                Ok(decl::DeclarationSpecifier::TypeSpecifier(type_specifier))
            }),
            Box::new(|iter| {
                let type_qualifier = parse_type_qualifier(iter)?;
                Ok(decl::DeclarationSpecifier::TypeQualifier(type_qualifier))
            }),
            Box::new(|iter| {
                let function_specifier = parse_function_specifier(iter)?;
                Ok(decl::DeclarationSpecifier::FunctionSpecifier(function_specifier))
            }),
            Box::new(|iter| {
                let alignment_specifier = parse_alignment_specifier(iter)?;
                Ok(decl::DeclarationSpecifier::AlignmentSpecifier(alignment_specifier))
            })]
        )
    })
}   

fn parse_init_declarator_list(parser: &mut ParseState) -> ParseResult<Vec<decl::InitDeclarator>> {
    let first = parse_init_declarator(parser)?;
    let result = parser.expect_zero_or_n(|iter: &mut ParseState| {
        iter.match_token(lexer::Token::Comma)?;
        parse_init_declarator(iter)
    });
    return Ok(result);
}

fn parse_init_declarator(parser: &mut ParseState) -> ParseResult<decl::InitDeclarator> {
    parser.parse_any(&[
        Box::new(|iter| {
            let declarator = parse_declarator(iter)?;
            Ok(decl::InitDeclarator{declarator, initializer: None})
        }),
        Box::new(|iter| {
            let declarator = parse_declarator(iter)?;
            iter.match_token(lexer::Token::Assign);
            let initializer = parse_initializer(iter)?;
            Ok(decl::InitDeclarator{declarator, initializer: Some(initializer)})
        })
    ])
}

fn parse_storage_class_specifier(parser: &mut ParseState) -> ParseResult<decl::StorageClassSpecifier> {
    parser.next();
    use lexer::Token::*;
    match parser.iter.current_item.unwrap().value {
        Typedef => Ok(decl::StorageClassSpecifier::Typedef),
        Extern => Ok(decl::StorageClassSpecifier::Extern),
        Static => Ok(decl::StorageClassSpecifier::Static),
        ThreadLocal => Ok(decl::StorageClassSpecifier::ThreadLocal),
        Auto => Ok(decl::StorageClassSpecifier::Auto),
        Register => Ok(decl::StorageClassSpecifier::Register),
        _ => Err(ParserError::MatchOrError(
            MatchOrError{
                expected: vec![
                    Typedef,
                    Extern,
                    Static,
                    ThreadLocal,
                    Auto,
                    Register,
                ],
                got: todo!(),
            }
        )
    ),
    }
}

fn parse_type_specifier(parser: &mut ParseState) -> ParseResult<decl::TypeSpecifier> {
    parser.parse_any(&[Box::new(|iter| {
            iter.next();
            use lexer::Token::*;
            match iter.iter.current_item.unwrap().value {
                Void => Ok(decl::TypeSpecifier::Void),
                Char => Ok(decl::TypeSpecifier::Char),
                Short => Ok(decl::TypeSpecifier::Short),
                Int => Ok(decl::TypeSpecifier::Int),
                Long => Ok(decl::TypeSpecifier::Long),
                Float => Ok(decl::TypeSpecifier::Float),
                Double =>  Ok(decl::TypeSpecifier::Double),
                Signed =>  Ok(decl::TypeSpecifier::Signed),
                Unsigned =>  Ok(decl::TypeSpecifier::Unsigned),
                Bool =>  Ok(decl::TypeSpecifier::Bool),
                Complex =>  Ok(decl::TypeSpecifier::Complex),
                _ => Err(ParserError::MatchOrError(
                        MatchOrError{
                            expected: vec![
                                Void,
                                Char,
                                Short,
                                Int,
                                Long,
                                Float,
                                Double,
                                Signed,
                                Unsigned,
                                Bool,
                                Complex,
                            ],
                            got: todo!(),
                        }
                    )
                )
            }
        }),
        Box::new(|iter| {
            let atomic_type_specifier = parse_atomic_type_specifier(iter)?;
            return Ok(decl::TypeSpecifier::AtomicTypeSpecifier(atomic_type_specifier));
        }),
        Box::new(|iter| { 
            let struct_or_union_specifier = parse_struct_or_union_specifier(iter)?;
            return Ok(decl::TypeSpecifier::StructOrUnionSpecifier(struct_or_union_specifier));
        }),
        Box::new(|iter|{ 
            let enum_specifier = parse_enum_specifier(iter)?;
            return Ok(decl::TypeSpecifier::EnumSpecifier(enum_specifier));
        }),
        Box::new(|iter| { 
            let typedef_name = parse_typedef_name(iter)?;
            return Ok(decl::TypeSpecifier::TypedefName(typedef_name));
        })
    ])
}

fn parse_struct_or_union_specifier(parser: &mut ParseState) -> ParseResult<decl::StructOrUnionSpecifier> {
    parser.parse_any(&[
        Box::new(|iter| {
            parse_struct_or_union(iter); 
            parse_identifier(iter); 
            iter.match_token(lexer::Token::LBrace)?;
            parse_struct_declaration_list(iter); 
            iter.match_token(lexer::Token::RBrace)?;
            todo!()
        }),
        Box::new(|iter| {
            parse_struct_or_union(iter); 
            parse_identifier(iter);
            todo!()
        })
    ])
}

fn parse_struct_or_union(parser: &mut ParseState) -> ParseResult<decl::ObjKind> {
    parser.next();
    use lexer::Token::*;
    match parser.iter.current_item.unwrap().value {
        Struct => Ok(decl::ObjKind::Struct),
        Union => Ok(decl::ObjKind::Union),
        _ => Err(ParserError::MatchOrError(
            MatchOrError{
                expected: vec![
                    Struct,
                    Union,
                ],
                got: todo!(),
            }
        )
    ),
    }
}

fn parse_struct_declaration_list(parser: &mut ParseState) -> ParseResult<Vec<decl::StructDeclarator>> {
    // parser.expect_one_or_n(parse_struct_declaration)
    todo!()
}

fn parse_struct_declaration(parser: &mut ParseState) -> ParseResult<decl::StructDeclaration> {
    parser.parse_any(&[
        Box::new(|iter| {
            let qualifier = parse_specifier_qualifier_list(iter)?; 
            let declarator = parse_struct_declarator_list(iter)?; 
            iter.match_token(lexer::Token::Semicolon)?;
            todo!()
        }),
        Box::new(|iter| {
            let static_assert_declaration = parse_static_assert_declaration(iter)?;
            // Ok(decl::StructDeclaration::StaticAssert(static_assert_declaration))
            todo!()
        })
    ])
}

fn parse_specifier_qualifier_list(parser: &mut ParseState) -> ParseResult<decl::SpecifierQualifierList> {
    parse_type_specifier(parser); 
    parse_type_qualifier(parser);
    todo!()
}

fn parse_struct_declarator_list(parser: &mut ParseState) -> ParseResult<Vec<decl::StructDeclarator>> {
    // parse_struct_declarator(parser);
    // parser.expect_zero_or_n(|iter| {
    //     parser.match_token(lexer::Token::Comma);
    //     parse_struct_declarator(parser);
    // })
    todo!()
}

fn parse_struct_declarator(parser: &mut ParseState) -> ParseResult<decl::StructDeclarator> {
    parser.parse_any(&[
        Box::new(|iter| { 
            parse_declarator(iter);
            todo!()
        }),
        Box::new(|iter| {  
            parse_declarator(iter);
            iter.match_token(lexer::Token::Colon);
            parse_constant_expression(iter); 
            todo!()
        })
    ])
}

fn parse_enum_specifier(parser: &mut ParseState) -> ParseResult<decl::EnumSpecifier> {
    parser.match_token(lexer::Token::Enum);
    parse_identifier(parser)?;
    parser.match_token(lexer::Token::LBrace);
    // ("{" EnumeratorList ","? "}")?
    parser.match_token(lexer::Token::Comma);
    parser.match_token(lexer::Token::RBrace);
    todo!()
}

fn parse_enumerator_list(parser: &mut ParseState) -> ParseResult<decl::Enumerator> {
    // parse_enumerator() 
    // ("," parse_enumerator())
    todo!()
}

fn parse_enumeration_constant(parser: &mut ParseState) -> ParseResult<String> {
    // parse_identifier()
    todo!()
}

fn parse_enumerator(parser: &mut ParseState) -> ParseResult<decl::Enumerator> {
    let identifier = parse_enumeration_constant(parser)?; 
    parser.match_token(lexer::Token::Assign);
    let expression = parse_constant_expression(parser)?; 
    Ok(decl::Enumerator{identifier, expression})
}

fn parse_atomic_type_specifier(parser: &mut ParseState) -> ParseResult<decl::AtomicTypeSpecifier> {
    parser.match_token(lexer::Token::Atomic);
    parser.match_token(lexer::Token::LParenthesis);
    parse_type_name(parser);
    parser.match_token(lexer::Token::RParenthesis);
    // parse_atomic_type_Specifier{typename}
    todo!()
}

fn parse_type_qualifier(parser: &mut ParseState) -> ParseResult<decl::TypeQualifier> {
    parser.next();
    use lexer::Token::*;
    match parser.iter.current_item.unwrap().value  {
        Const => Ok(decl::TypeQualifier::Const),
        Restrict => Ok(decl::TypeQualifier::Restrict),
        Volatile => Ok(decl::TypeQualifier::Volatile),
        Atomic => Ok(decl::TypeQualifier::Atomic),
        _ => Err(ParserError::MatchOrError(
                MatchOrError{
                    expected: vec![
                        Const,
                        Restrict,
                        Volatile,
                        Atomic,
                    ],
                    got: todo!(),
                }
            )
        )
    }
}

fn parse_function_specifier(parser: &mut ParseState) -> ParseResult<decl::FunctionSpecifier> {
    parser.next();
    use lexer::Token::*;
    match parser.iter.current_item.unwrap().value {
        Inline => Ok(decl::FunctionSpecifier::Inline),
        Noreturn => Ok(decl::FunctionSpecifier::Noreturn),
        _ => Err(ParserError::MatchOrError(
                MatchOrError{
                    expected: vec![
                        Inline,
                        Noreturn,
                    ],
                    got: todo!(),
                }
            )
        )
    }
}

fn parse_alignment_specifier(parser: &mut ParseState) -> ParseResult<decl::AlignmentSpecifier> {
//     // lexer::Token::Alignas; 
//     // lexer::Token::LParenthesis; 
//     // TypeName(); 
//     // lexer::Token::RParenthesis;
    
//     // lexer::Token::Alignas; 
//     // lexer::Token::LParenthesis; 
//     // ConstantExpression(); 
//     // lexer::Token::RParenthesis;
    todo!()
}

fn parse_declarator(parser: &mut ParseState) -> ParseResult<decl::Declarator> {
    parse_pointer(parser); 
    parse_direct_declarator(parser);
    todo!()
}

fn parse_direct_declarator(parser: &mut ParseState) -> ParseResult<decl::DirectDeclarator> {
    parser.parse_any( &[
        Box::new(|iter| {
            let identifier = parse_identifier(iter)?;
            Ok(decl::DirectDeclarator::FunctionDeclarators)
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::LParenthesis);
            let declarator = parse_declarator(iter)?; 
            iter.match_token(lexer::Token::RParenthesis);
            Ok(decl::DirectDeclarator::FunctionDeclarators)
        }),
        Box::new(|iter| {
            parse_direct_declarator(iter);
            iter.match_token(lexer::Token::LBracket); 
            parse_type_qualifier_list(iter); 
            parse_assignment_expression(iter); 
            iter.match_token(lexer::Token::RBracket);
            Ok(decl::DirectDeclarator::FunctionDeclarators)
        }),
        Box::new(|iter| {
            parse_direct_declarator(iter); 
            iter.match_token(lexer::Token::LBracket);
            iter.match_token(lexer::Token::Static);
            parse_type_qualifier_list(iter); 
            parse_assignment_expression(iter); 
            iter.match_token(lexer::Token::RBracket);
            Ok(decl::DirectDeclarator::FunctionDeclarators)
        }),
        Box::new(|iter| {
            parse_direct_declarator(iter); 
            iter.match_token(lexer::Token::LBracket);
            parse_type_qualifier_list(iter); 
            iter.match_token(lexer::Token::Static);
            parse_assignment_expression(iter); 
            iter.match_token(lexer::Token::RBracket);
            Ok(decl::DirectDeclarator::FunctionDeclarators)
        }), 
        Box::new(|iter| {
            parse_direct_declarator(iter); 
            iter.match_token(lexer::Token::LBracket);
            parse_type_qualifier_list(iter); 
            iter.match_token(lexer::Token::Asterisk);
            iter.match_token(lexer::Token::RBracket); 
            Ok(decl::DirectDeclarator::FunctionDeclarators)
        }),
        Box::new(|iter| {
            parse_direct_declarator(iter); 
            iter.match_token(lexer::Token::LParenthesis);
            parse_parameter_type_list(iter); 
            iter.match_token(lexer::Token::RParenthesis);
            Ok(decl::DirectDeclarator::FunctionDeclarators)
        }),
        Box::new(|iter| {
            parse_direct_declarator(iter); 
            iter.match_token(lexer::Token::LParenthesis);
            parse_identifier_list(iter); 
            iter.match_token(lexer::Token::RParenthesis);
            Ok(decl::DirectDeclarator::FunctionDeclarators)
        })
    ])
}

fn parse_pointer(parser: &mut ParseState) -> ParseResult<decl::Pointer> {
    parser.match_token(lexer::Token::Asterisk);
    parse_type_qualifier_list(parser);
    todo!()
}

fn parse_type_qualifier_list(parser: &mut ParseState) -> ParseResult<decl::TypeQualifier> {
    parse_type_qualifier(parser);
    todo!()
}

fn parse_parameter_type_list(parser: &mut ParseState) -> ParseResult<decl::ParameterTypeList> {
    // decl::ParameterTypeList{}
    todo!()
}

fn parse_parameter_list(parser: &mut ParseState) -> ParseResult<Vec<decl::ParameterDeclaration>> {
    todo!()
}

fn parse_parameter_declaration(parser: &mut ParseState) -> ParseResult<decl::ParameterDeclaration> {
    // decl::ParameterDeclaration{}
    // decl::ParameterDeclaration{}
    todo!()
}

fn parse_identifier_list(parser: &mut ParseState) -> ParseResult<Vec<String>> {
    // parse_identifier(); ("," parse_identifier())*
    todo!()
}

fn parse_type_name(parser: &mut ParseState) -> ParseResult<String> {
    // parse_specifier_qualifier_list(); parse_abstract_declarator()?;
    todo!()
}

fn parse_abstract_declarator(parser: &mut ParseState) -> ParseResult<decl::AbstractDeclarator> {
    parse_pointer(parser);
    parse_pointer(parser); 
    parse_direct_abstract_declarator(parser);
    todo!()
}

fn parse_direct_abstract_declarator(parser: &mut ParseState) -> ParseResult<decl::DirectAbstractDeclarator> {
//     "(" parse_abstract_declarator() ")"
// //     => todo!(),
// parse_direct_abstract_declarator? "[" parse_type_qualifier_list? parse_assignment_expression? "]"
// //     => todo!(),
// parse_direct_abstract_declarator? "[" "static" parse_type_qualifier_list? parse_assignment_xpression "]"
// //     => todo!(),
// parse_directAbstractDeclarator? "[" parse_type_qualifier_list "static" parse_assignment_expression "]"
// //     => todo!(),
// parse_direct_abstract_declarator? "[" "*" "]"
// //     => todo!(),
// parse_direct_abstract_declarator? "(" parse_parameter_type_list? ")"
    todo!()
}

fn parse_initializer(parser: &mut ParseState) -> ParseResult<decl::Initializer> {
    parser.parse_any(&[
        Box::new(|iter| {
            parse_assignment_expression(iter);
            todo!()
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::LBrace);  
            parse_initializer_list(iter);
            iter.match_token(lexer::Token::Comma); 
            iter.match_token(lexer::Token::RBrace); 
            todo!()
        })
    ])
}

fn parse_initializer_list(parser: &mut ParseState) -> ParseResult<decl::InitializerList> {
    let designation = parser.optional_parse(parse_designation);
    parse_initializer(parser);
    // parser.expect_zero_or_n(|iter: &mut ParseState|{
    //     iter.match_token(lexer::Token::Comma);
    //     let designation = parser.optional_parse(parse_designation);
    //     parse_initializer(iter);
    // });
    todo!()
}

fn parse_designation(parser: &mut ParseState) -> ParseResult<Vec<decl::Designator>> {
    let designator_list= parse_designator_list(parser);
    parser.match_token(lexer::Token::Assign)?;
    designator_list
}

fn parse_designator_list(parser: &mut ParseState) ->  ParseResult<Vec<decl::Designator>> {
    parser.expect_one_or_n(parse_designator)
}

fn parse_designator(parser: &mut ParseState) -> ParseResult<decl::Designator> {
    parser.parse_any(&[
        Box::new(|iter| {
            iter.match_token(lexer::Token::LBracket);
            let expr = parse_constant_expression(iter)?; 
            iter.match_token(lexer::Token::RBracket);
            return Ok(decl::Designator::new_index(expr));
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::Dot);
            let ident = parse_identifier(iter)?;
            return Ok(decl::Designator::new_dot(ident));
        })
    ])
}

fn parse_static_assert_declaration(parser: &mut ParseState) -> ParseResult<decl::StaticAssert> {
    parser.match_token(lexer::Token::StaticAssert);
    parser.match_token(lexer::Token::LParenthesis);
    let predicate = parse_constant_expression(parser)?; 
    parser.match_token(lexer::Token::Comma);
    let message = parse_string_literal(parser)?; 
    parser.match_token(lexer::Token::RParenthesis);
    parser.match_token(lexer::Token::Semicolon);
    return Ok(decl::StaticAssert{predicate, message});
}

// Expression Rules

// primary_expression: 
// identifier
// | constant
// | string_literal
// | "(" expression ")"
// | generic_selection
fn parse_primary_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_primary_expression");

    parser.parse_any(&[
        Box::new(|iter| {
            let ident = parse_identifier(iter)?;
            Ok(expr::Expression::Identifier(ident))
        }),
        // Box::new(|iter| {
        //     let constant = parse_constant(iter)?;
        //     Ok(expr::Expression::Literal(expr::Literal::Str(expr::StringLiteral{
        //         value: constant,
        //         prefix: None,
        //     })))
        // }),
        // Box::new(|iter| {
        //     let string = parse_string_literal(iter)?;
        //     Ok(expr::Expression::Literal(expr::Literal::Str(expr::StringLiteral{
        //         value: string,
        //         prefix: None,
        //     })))
        // }),
        // Box::new(|iter| {
        //     iter.match_token(lexer::Token::LParenthesis);
        //     let expression = parse_expression(iter)?; 
        //     iter.match_token(lexer::Token::RParenthesis);
        //     Ok(expression)
        // }),
        // Box::new(|iter| {
        //     let generic_selection = parse_generic_selection(iter)?;
        //     Ok(expr::Expression::GenericSelection(Box::new(generic_selection)))
        // })
    ])
}

// generic_selection:
// "_Generic" "(" assignment_expression "," parse_generic_assoc_list ")"
fn parse_generic_selection(parser: &mut ParseState) -> ParseResult<expr::GenericSelectionExpr> {
    println!("parse_generic_selection");

    parser.match_token(lexer::Token::Generic);
    parser.match_token(lexer::Token::LParenthesis);
    let expression = parse_assignment_expression(parser)?; 
    parser.match_token(lexer::Token::Comma);
    let association_list = parse_generic_assoc_list(parser)?; 
    parser.match_token(lexer::Token::RParenthesis);
    Ok(expr::GenericSelectionExpr{expression, association_list})
}

// parse_generic_assoc_list: 
// generic_association ("," generic_association)*
fn parse_generic_assoc_list(parser: &mut ParseState) -> ParseResult<Vec<expr::GenericAssociation>> {
    println!("parse_generic_assoc_list");

    let first = parse_generic_association(parser)?;
    let result = parser.expect_zero_or_n(|iter: &mut ParseState| {
        iter.match_token(lexer::Token::Comma)?;
        parse_generic_association(iter)
    });
    todo!()
}

// generic_association:
// type_name ":" assignment_expression
// | "default" ":" assignment_expression
fn parse_generic_association(parser: &mut ParseState) -> ParseResult<expr::GenericAssociation> {
    println!("parse_generic_association");

    parser.parse_any(&[
        Box::new(|iter| {
            parse_type_name(iter); 
            iter.match_token(lexer::Token::Colon)?; 
            let expression = parse_assignment_expression(iter)?;
            return Ok(expr::GenericAssociation{expression});
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::Default)?;
            iter.match_token(lexer::Token::Colon)?;
            let expression = parse_assignment_expression(iter)?;
            return Ok(expr::GenericAssociation{expression});
        })
    ])
}

//  Original rule:
//
//  postfix_expression:
//  primary_expression
//  | postfix_expression "[" expression "]" 
//  | postfix_expression "(" parse_argument_expression_list? ")"
//  | postfix_expression "." identifier
//  | postfix_expression "->" identifier
//  | postfix_expression "++"
//  | postfix_expression "--"
//  | ( type_name ) "{" initializer_list "}"
//  | ( type_name ) "{" initializer_list "," "}"
//  
//  Rewritten:
//
//  C: A B*
//
//  A:
//  primary_expression
//  | ( type_name ) "{" initializer_list "}"
//  | ( type_name ) "{" initializer_list "," "}"
//  
//  B:
//  "[" expression "]"
//  | "(" parse_argument_expression_list? ")"
//  | "." identifier
//  | "->" identifier
//  | "++"
//  | "--"
pub (crate) fn parse_postfix_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_postfix_expression");

    let expression = parser.parse_any( &[
        Box::new(|iter| parse_primary_expression(iter)),
        Box::new(|iter| {
            iter.match_token(lexer::Token::LParenthesis);
            parse_type_name(iter); 
            iter.match_token(lexer::Token::RParenthesis);
            iter.match_token(lexer::Token::LBrace);
            let init_list = parse_initializer_list(iter)?; 
            iter.match_token(lexer::Token::RParenthesis);
            return Ok(expr::CompoundLiteral::new(Box::new(init_list)));
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::LParenthesis);
            parse_type_name(iter); 
            iter.match_token(lexer::Token::RParenthesis);
            iter.match_token(lexer::Token::LBrace);
            let init_list = parse_initializer_list(iter)?; 
            iter.match_token(lexer::Token::Dot);
            iter.match_token(lexer::Token::RBrace);
            return Ok(expr::CompoundLiteral::new(Box::new(init_list)));
        }),
    ])?;
    
    let mut operator = parser.parse_any(&[
        // Box::new(|iter| {
        //     iter.match_token(lexer::Token::LBracket)?;
        //     let rhs = parse_expression(iter)?;
        //     iter.match_token(lexer::Token::RBracket)?;
        //     Ok(expr::TwoOperandsExpr::new_subscript(expr::Expression::None, rhs))
        // }),
        // Box::new(|iter| {
        //     iter.match_token(lexer::Token::LParenthesis);
        //     let args = iter.optional_parse(parse_argument_expression_list); 
        //     iter.match_token(lexer::Token::RParenthesis);
        //     Ok(expr::CallExpr::new(expr::Expression::None, args.or(Default::default()).unwrap()))
        // }),
        Box::new(|iter| { 
            iter.match_token(lexer::Token::Dot)?;
            let ident = parse_identifier(iter)?;    
            let rhs = expr::Expression::Identifier(ident);
            Ok(expr::TwoOperandsExpr::new_access(expr::Expression::None, rhs))
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::Arrow);
            let ident = parse_identifier(iter)?;
            let rhs = expr::Expression::Identifier(ident);
            Ok(expr::TwoOperandsExpr::new_ptr_access(expr::Expression::None, rhs))
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::TwoPluses)?;
            Ok(expr::OneOperandExpr::new_postincrement(expr::Expression::None))
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::TwoMinuses)?;
            Ok(expr::OneOperandExpr::new_postdecrement(expr::Expression::None))
        }),
    ])?;
    
    match operator {
        expr::Expression::Call(ref mut expr) => expr.callie = expression,
        expr::Expression::TwoOperands(ref mut expr) => expr.lhs = expression,
        expr::Expression::OneOperand(ref mut expr) => expr.value = expression,
        _ => panic!()
    }

    Ok(operator)
}

// argument_expression_list: 
// assignment_expression ("," assignment_expression)*
fn parse_argument_expression_list(parser: &mut ParseState) -> ParseResult<Vec<expr::Expression>> {
    println!("parse_argument_expression_list");
    // parser.parse_any(&[
    //     Box::new(|iter| parse_assignment_expression(iter)),
    //     Box::new(|iter| {
    //         iter.match_token(lexer::Token::Comma)?;
    //         parse_assignment_expression(iter)
    //     })
    // ])
    todo!()
}

// unary_expression: 
// postfix_expression
// | "++" unary_expression
// | "--" unary_expression
// | unary_operator unary_expression
// | "sizeof" unary_expression
// | "sizeof" "(" type_name ")"
// | "_Alignof" "(" type_name ")"
//
// C: A* B
//
// A:
// | "++" 
// | "--" 
// | unary_operator 
// | "sizeof" 
// | "sizeof" "(" type_name ")"
// | "_Alignof" "(" type_name ")"
//
// B:
// postfix_expression
// | "sizeof" "(" type_name ")"
// | "_Alignof" "(" type_name ")"
fn parse_unary_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_unary_expression");

    parser.parse_any(&[
        Box::new(|iter| parse_postfix_expression(iter)),
        Box::new(|iter| {
            iter.match_token(lexer::Token::TwoPluses)?;
            let unary_expression= parse_unary_expression(iter)?;
            return Ok(expr::OneOperandExpr::new_increment(unary_expression));
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::TwoMinuses)?;
            let unary_expression = parse_unary_expression(iter)?;
            return Ok(expr::OneOperandExpr::new_decrement(unary_expression));
        }),
        Box::new(|iter| {
            let op = parse_unary_operator(iter)?; 
            let cast_expr = parse_cast_expression(iter)?;
            todo!()
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::Sizeof);
            let unary_expression = parse_unary_expression(iter)?; 
            return Ok(expr::Sizeof::new());
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::Sizeof);
            iter.match_token(lexer::Token::LParenthesis);
            let type_name = parse_type_name(iter)?;
            iter.match_token(lexer::Token::RParenthesis);
            return Ok(expr::Sizeof::new());
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::Alignof);
            iter.match_token(lexer::Token::LParenthesis);
            let type_name = parse_type_name(iter)?; 
            iter.match_token(lexer::Token::RParenthesis);
            return Ok(expr::AlignOf::new());
        })
    ])
}

// unary_operator: "&" | "*" | "+" | "-" | "~" | "!"
fn parse_unary_operator(parser: &mut ParseState) -> ParseResult<expr::UnaryOp> {
    println!("parse_unary_operator");

    parser.next();
    use lexer::Token::*;
    match parser.iter.current_item.unwrap().value {
        Ampersand => Ok(expr::UnaryOp::Address),
        Asterisk => Ok(expr::UnaryOp::Indirection),
        Plus => Ok(expr::UnaryOp::Positive),
        Minus => Ok(expr::UnaryOp::Negative),
        Tilde => Ok(expr::UnaryOp::BitwiseNot),
        ExclamationMark => Ok(expr::UnaryOp::LogicalNot),
        _ => Err(ParserError::MatchOrError(
                MatchOrError{
                    expected: vec![
                        Ampersand, 
                        Asterisk,
                        Plus,
                        Minus,
                        Tilde,
                        ExclamationMark,
                    ],
                    got: todo!(),
                }
            )
        ),
    }
}

// cast_expression:
// unary_expression
// | "(" type_name ")" cast_expression
//
// C: A* B
//
// A: "(" type_name ")" 
// B: unary_expression
fn parse_cast_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_cast_expression");
    
    let ops = parser.expect_zero_or_n(|iter: &mut ParseState| {
        iter.match_token(lexer::Token::LParenthesis)?;
        parse_type_name(iter);  
        iter.match_token(lexer::Token::RParenthesis);
        let expr = parse_cast_expression(iter)?;
        return Ok(expr::CastExr::new((), expr));
    });

    parse_unary_expression(parser)
}

// multiplicative_expression:
// cast_expression
// | multiplicative_expression "*" cast_expression
// | multiplicative_expression "/" cast_expression
// | multiplicative_expression "%" cast_expression
//
fn parse_multiplicative_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_multiplicative_expression");
    parser.parse_any(&[
        Box::new(|iter| parse_cast_expression(iter)),
        Box::new(|iter| {
            let lhs = parse_multiplicative_expression(iter)?; 
            iter.match_token(lexer::Token::Asterisk)?;
            let rhs = parse_cast_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_mul(lhs, rhs));
        }),
        Box::new(|iter| {
            let lhs = parse_multiplicative_expression(iter)?; 
            iter.match_token(lexer::Token::Slash)?;
            let rhs = parse_cast_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_div(lhs, rhs));
        }),
        Box::new(|iter| {
            let lhs = parse_multiplicative_expression(iter)?; 
            iter.match_token(lexer::Token::Percent)?; 
            let rhs = parse_cast_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_rem(lhs, rhs));
        })
    ])
}

// additive_expression:
//  multiplicative_expression 
//  | additive_expression "+" multiplicative_expression
//  | additiveExpression "-" multiplicative_expression
fn parse_additive_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_additive_expression");
    parser.parse_any(&[
        Box::new(|iter| parse_multiplicative_expression(iter)),
        Box::new(|iter| {
            let lhs = parse_additive_expression(iter)?; 
            iter.match_token(lexer::Token::Plus)?; 
            let rhs = parse_multiplicative_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_add(lhs, rhs));
        }),
        Box::new(|iter| {
            let lhs = parse_additive_expression(iter)?; 
            iter.match_token(lexer::Token::Minus)?;  
            let rhs = parse_multiplicative_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_sub(lhs, rhs));
        })
    ])
}

// shift_expression:
// | additive_expression
// | shift_expression "<<" additive_expression
// | shift_expression ">>" additive_expression
fn parse_shift_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_shift_expression");
    parser.parse_any(&[
        Box::new(|iter| parse_additive_expression(iter)),
        Box::new(|iter| {
            let lhs = parse_shift_expression(iter)?;
            iter.match_token(lexer::Token::TwoSmaller)?; 
            let rhs = parse_additive_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_left_shift(lhs, rhs));
        }),
        Box::new(|iter| {
            let lhs = parse_shift_expression(iter)?; 
            iter.match_token(lexer::Token::TwoLager)?; 
            let rhs = parse_additive_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_right_shift(lhs, rhs));
        })
    ])
}

// relational_expression:
// shift_expression
// | relational_expression "<" shift_expression
// | relational_expression ">" shift_expression
// | relational_expression "<=" shift_expression
// | relational_expression ">=" shift_expression
fn parse_relational_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_relational_expression");
    parser.parse_any(&[
        Box::new(|iter| parse_shift_expression(iter)),
        Box::new(|iter| {
            let lhs = parse_relational_expression(iter)?; 
            iter.match_token(lexer::Token::Smaller)?; 
            let rhs = parse_shift_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_le(lhs, rhs));
        }),
        Box::new(|iter| {
            let lhs = parse_relational_expression(iter)?; 
            iter.match_token(lexer::Token::Lager)?; 
            let rhs = parse_shift_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_ge(lhs, rhs));
        }),
        Box::new(|iter| {
            let lhs = parse_relational_expression(iter)?; 
            iter.match_token(lexer::Token::LeOrEq)?; 
            let rhs = parse_shift_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_ge_or_eq(lhs, rhs));
        }),
        Box::new(|iter| {
            let lhs = parse_relational_expression(iter)?; 
            iter.match_token(lexer::Token::GeOrEq)?; 
            let rhs = parse_shift_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_ge_or_eq(lhs, rhs));
        })
    ])
}

// equality_expression:
// relational_expression
// | equality_expression "==" relational_expression
// | equality_expression "!=" relational_expression
fn parse_equality_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_equality_expression");
    parser.parse_any(&[
        Box::new(|iter| parse_relational_expression(iter)),
        Box::new(|iter| {
            let lhs = parse_equality_expression(iter)?; 
            iter.match_token(lexer::Token::Equal)?; 
            let rhs = parse_relational_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_eq(lhs, rhs));
        }),
        Box::new(|iter| {
            let lhs = parse_equality_expression(iter)?; 
            iter.match_token(lexer::Token::NotEq)?;  
            let rhs = parse_relational_expression(iter)?; 
            return Ok(expr::TwoOperandsExpr::new_not_eq(lhs, rhs));
        })
    ])
}

// and_expression:
// equality_expression
// | and_expression "&" equality_expression
fn parse_and_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_and_expression");
    parser.parse_any(&[
        Box::new(|iter| parse_equality_expression(iter)),
        Box::new(|iter| {
            let lhs = parse_and_expression(iter)?; 
            iter.match_token(lexer::Token::Ampersand)?; 
            let rhs = parse_equality_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_bw_and(lhs, rhs));
        })
    ])
}

// exclusive_or_expressio:
// and_expression
// | exclusive_or_expression "^" and_expression
fn parse_exclusive_or_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_exclusive_or_expression");
    parser.parse_any(&[
        Box::new(|iter| parse_and_expression(iter)),
        Box::new(|iter| {
            let lhs = parse_exclusive_or_expression(iter)?; 
            iter.match_token(lexer::Token::Caret)?; 
            let rhs = parse_and_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_bw_xor(lhs, rhs));
        })
    ])
}

// inclusive_or_expression:
// exclusive_or_expression 
// | inclusive_or_expression "|" exclusive_or_expression
fn parse_inclusive_or_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_inclusive_or_expression");
    parser.parse_any(&[
        Box::new(|iter| parse_exclusive_or_expression(iter)),
        Box::new(|iter| {
            let lhs = parse_inclusive_or_expression(iter)?; 
            iter.match_token(lexer::Token::Bar)?; 
            let rhs = parse_exclusive_or_expression(iter)?; 
            return Ok(expr::TwoOperandsExpr::new_bw_or(lhs, rhs));
        })
    ])
}

// inclusive_or_expression:
// inclusive_or_expression
// | logical_and_expression "&&" inclusive_or_expression
fn parse_logical_and_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_logical_and_expression");
    parser.parse_any(&[
        Box::new(|iter| parse_inclusive_or_expression(iter)),
        Box::new(|iter| {
            let lhs = parse_logical_and_expression(iter)?; 
            iter.match_token(lexer::Token::WAmpersand)?; 
            let rhs = parse_inclusive_or_expression(iter)?; 
            return Ok(expr::TwoOperandsExpr::new_logical_and(lhs, rhs));
        })
    ])
}

// logical_and_expression:
// logical_and_expression
// | logical_or_expression "||" logical_and_expression
fn parse_logical_or_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_logical_or_expression");
    parser.parse_any(&[
        Box::new(|iter| parse_logical_and_expression(iter)),
        Box::new(|iter| {
            let lhs = parse_logical_or_expression(iter)?; 
            iter.match_token(lexer::Token::WBar)?; 
            let rhs = parse_logical_and_expression(iter)?;
            return Ok(expr::TwoOperandsExpr::new_logical_or(lhs, rhs));
        })
    ])
}

// conditional_expression:
// logical_or_expression 
// | logical_or_expression "?" expression ":" conditional_expression
fn parse_conditional_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_conditional_expression");
    parser.parse_any(&[
        Box::new(|iter| parse_logical_or_expression(iter)),
        Box::new(|iter| {
            let lhs = parse_logical_or_expression(iter); 
            iter.match_token(lexer::Token::QuestionMark)?; 
            let rhs= parse_expression(iter); 
            iter.match_token(lexer::Token::Colon); 
            parse_conditional_expression(iter);
            return Ok(expr::Expression::Ternary(Box::new(expr::Ternary{})));
        })
    ])
}

// assignment_expression:
// conditional_expression
// | unary_expression assignment_operator assignment_expression
fn parse_assignment_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_assignment_expression");
    parser.parse_any(&[
        Box::new(|iter| parse_conditional_expression(iter)),
        Box::new(|iter| {
            let lhs = parse_unary_expression(iter)?; 
            let op = parse_assignment_operator(iter)?; 
            let rhs = parse_assignment_expression(iter)?;
            return Ok(expr::Expression::TwoOperands(Box::new(expr::TwoOperandsExpr{lhs, op, rhs})));
        })
    ])
}

// assignment_operator:
// "=" 
// | "*=" 
// | "/=" 
// | "%=" 
// | "+=" 
// | "-=" 
// | "<<=" 
// | ">>=" 
// | "&=" 
// | "^="
// | "|="
fn parse_assignment_operator(parser: &mut ParseState) -> ParseResult<expr::BiTag> {
    println!("parse_assignment_operator");

    parser.next(); 
    let tok = parser.iter.current_item.unwrap().value;
    use lexer::Token::*;
    return match tok {
        Assign => Ok(expr::BiTag::Assign),
        AsteriskAssign => Ok(expr::BiTag::MulAssign),
        SlashAssign => Ok(expr::BiTag::DivAssign),
        PercentAssign => Ok(expr::BiTag::RemAssign),
        PlusAssign => Ok(expr::BiTag::AddAssign),
        MinusAssign => Ok(expr::BiTag::SubAssign),
        LGuillemetsAssign => Ok(expr::BiTag::BwLShAssign),
        RGuillemetsAssign => Ok(expr::BiTag::BwRShAssign),
        AmpersandAssign => Ok(expr::BiTag::BwAndAssign),
        CaretAssign => Ok(expr::BiTag::BwXorAssign),
        VBarAssign => Ok(expr::BiTag::BwOrAssign),
        _ => Err(ParserError::MatchOrError(
            MatchOrError{
                    expected: vec![
                        Assign, 
                        AsteriskAssign, 
                        SlashAssign,
                        PercentAssign,
                        PlusAssign,
                        MinusAssign,
                        LGuillemetsAssign,
                        RGuillemetsAssign,
                        AmpersandAssign,
                        CaretAssign,
                        VBarAssign,
                    ],
                    got: todo!(),
                }
            )
        ),
    };
}

// expression:
// assignment_expression ("," assignment_expression)*
pub (crate) fn parse_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_expression");

    parse_primary_expression(parser)
    // let first = parse_assignment_expression(parser)?;
    // let result = parser.expect_zero_or_n(|iter: &mut ParseState| {
    //     iter.match_token(lexer::Token::Comma)?;
    //     parse_assignment_expression(iter)
    // });
    // if result.is_empty() {
    //     return Ok(first);
    // }
    // Ok(result.into_iter().fold(first, |acc, el| {
    //     expr::TwoOperandsExpr::new_comma(acc, el)
    // }))
}

// constant_expression:
// conditional_expression
fn parse_constant_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    println!("parse_constant_expression");
    parse_conditional_expression(parser)
}

// Statements Rules

pub(crate) fn parse_statement(parser: &mut ParseState) -> ParseResult<stmt::Statement> {
    parser.parse_any(&[
        Box::new(|iter| parse_labeled_statement(iter)),
        Box::new(|iter| {
            let compound = parse_compound_statement(iter)?;
            Ok(stmt::Statement::CompoundStatement(compound))
        }),
        Box::new(|iter| parse_expression_statement(iter)),
        Box::new(|iter| parse_selection_statement(iter)),
        Box::new(|iter| parse_iteration_statement(iter)),
        Box::new(|iter| parse_jump_statement(iter)),
    ])
}

fn parse_compound_statement(parser: &mut ParseState) -> ParseResult<stmt::CompoundStatement> {
    parser.match_token(lexer::Token::LBrace);
    let result = parser.expect_zero_or_n(parse_block_item);
    parser.match_token(lexer::Token::RBrace);
    return Ok(stmt::CompoundStatement::new(result));
}

fn parse_block_item(parser: &mut ParseState) -> ParseResult<stmt::BlockItem> {
    parser.parse_any(&[
        Box::new(|iter| {
            let result = parse_declaration(iter)?;
            Ok(stmt::BlockItem::Declaration(result))
        }),
        Box::new(|iter| {
            let result = parse_statement(iter)?;
            Ok(stmt::BlockItem::Statement(result))
        })
    ])
}

fn parse_expression_statement(parser: &mut ParseState) -> ParseResult<stmt::Statement> {
    let expression = parse_expression(parser)?;
    parser.match_token(lexer::Token::Colon)?;
    return Ok(stmt::Statement::new_expr_stmt(expression));
}

fn parse_labeled_statement(parser: &mut ParseState) -> ParseResult<stmt::Statement> {
    parser.parse_any(&[
        Box::new(|iter| {
            let identifier = parse_identifier(iter)?;
            iter.match_token(lexer::Token::Colon)?;
            let statement= parse_statement(iter)?;
            return Ok(stmt::Labeled::new(identifier, statement));
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::Case)?;
            let constant = parse_constant_expression(iter)?;
            iter.match_token(lexer::Token::Colon)?;
            let body = parse_statement(iter)?;
            return Ok(stmt::Case::new(constant, body));
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::Default)?;
            iter.match_token(lexer::Token::Colon)?;
            let body = parse_statement(iter)?;
            return Ok(stmt::Default::new(body));
        }),
    ])
}

fn parse_selection_statement(parser: &mut ParseState) -> ParseResult<stmt::Statement> {
    parser.parse_any(&[
        Box::new(|iter| {
            iter.match_token(lexer::Token::If)?;
            iter.match_token(lexer::Token::LParenthesis)?;
            let expression = parse_expression(iter)?;
            iter.match_token(lexer::Token::RParenthesis)?;
            let statement = parse_statement(iter)?;
            return Ok(stmt::IfStmt::new(expression, statement));
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::If)?;
            iter.match_token(lexer::Token::LParenthesis)?;
            let predicate = parse_expression(iter)?;
            iter.match_token(lexer::Token::RParenthesis)?;
            let on_success = parse_statement(iter)?; 
            iter.match_token(lexer::Token::Else)?;
            let on_failure = parse_statement(iter)?;
            return Ok(stmt::IfElseStmt::new(predicate, on_success, on_failure));
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::Switch)?;
            iter.match_token(lexer::Token::LParenthesis)?;
            let controlling = parse_expression(iter)?;
            iter.match_token(lexer::Token::RParenthesis)?;
            let body = parse_statement(iter)?;
            return Ok(stmt::Statement::new_switch(controlling, body));
        })
    ])
}

fn parser_for_initializer_part(parser: &mut ParseState) -> ParseResult<stmt::ForInit> {
    parser.parse_any(&[
        Box::new(|iter| {
            let expression = parse_expression(iter)?;
            iter.match_token(lexer::Token::Semicolon)?;
            return Ok(stmt::ForInit::Expression(expression));
        }),
        Box::new(|iter| {
            let declaration = parse_declaration(iter)?;
            return Ok(stmt::ForInit::Declaration(Box::new(declaration)));
        })
    ])
}

pub(crate) fn parse_iteration_statement(parser: &mut ParseState) -> ParseResult<stmt::Statement> {
    parser.parse_any( &[
        Box::new(|iter| {
            iter.match_token(lexer::Token::While)?;
            iter.match_token(lexer::Token::LParenthesis)?;
            let expression = parse_expression(iter)?;
            iter.match_token(lexer::Token::RParenthesis)?;
            let statement = parse_statement(iter)?;
            return Ok(stmt::Statement::new_while(expression, statement));
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::While)?;
            iter.match_token(lexer::Token::Do)?;
            let statement = parse_statement(iter)?;
            iter.match_token(lexer::Token::While)?;
            iter.match_token(lexer::Token::LParenthesis)?;
            let expression = parse_expression(iter)?;
            iter.match_token(lexer::Token::RParenthesis)?;
            iter.match_token(lexer::Token::Semicolon)?;
            return Ok(stmt::Statement::new_do_while(expression, statement));
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::For)?;
            iter.match_token(lexer::Token::LParenthesis)?;
            let initializer = iter.optional_parse(parser_for_initializer_part); 
            let predicate = iter.optional_parse(parse_expression);
            iter.match_token(lexer::Token::Semicolon)?;
            let step = iter.optional_parse(parse_expression); 
            iter.match_token(lexer::Token::RParenthesis)?;
            let body = parse_statement(iter)?;
            return Ok(stmt::Statement::new_for(initializer, predicate, step, body));
        })
    ])
}


pub(crate) fn parse_jump_statement(parser: &mut ParseState) -> ParseResult<stmt::Statement> {    
    parser.parse_any(&[
        Box::new(|iter| {
            iter.match_token(lexer::Token::Goto)?;
            let identifier = parse_identifier(iter)?;
            iter.match_token(lexer::Token::Semicolon)?;
            return Ok(stmt::Statement::new_goto(identifier));
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::Continue)?;
            iter.match_token(lexer::Token::Semicolon)?;
            return Ok(stmt::Statement::new_continue());
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::Break)?;
            iter.match_token(lexer::Token::Semicolon)?;
            return Ok(stmt::Statement::new_break());
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::Return)?;
            let expression = iter.optional_parse(parse_expression); 
            iter.match_token(lexer::Token::Semicolon)?;
            return Ok(stmt::Statement::new_return(expression));
        })
    ])
}