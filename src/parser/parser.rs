use super::lexer;
use crate::ast::{expr, stmt, decl, self, r#type};
pub use super::public_parser::*;
use std::collections::VecDeque;


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

// impl std::fmt::Display for ParserError {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.write_fmt(format_args!("Unrecognizable token encountered: {}", self.0))
//     }
// }

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

fn parse_translation_unit(parser: &mut ParseState) -> ParseResult<ast::Ast> {
    let external_declarations = parser.expect_zero_or_n(parse_external_declaration);
    Ok(ast::Ast(external_declarations))
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
//     let token = iter.next(); 
//     if token.is_none() {
//         return Err(ParserError);
//     }

//     match token.unwrap().unwrap().1 {
//         lexer::Token::Typedef => Ok(decl::StorageClassSpecifier::Typedef),
//         lexer::Token::Extern => Ok(decl::StorageClassSpecifier::Extern),
//         lexer::Token::Static => Ok(decl::StorageClassSpecifier::Static),
//         lexer::Token::ThreadLocal => Ok(decl::StorageClassSpecifier::ThreadLocal),
//         lexer::Token::Auto => Ok(decl::StorageClassSpecifier::Auto),
//         lexer::Token::Register => Ok(decl::StorageClassSpecifier::Register),
//         _ => Err(ParserError),
//     }
    todo!()
}

fn parse_type_specifier(parser: &mut ParseState) -> ParseResult<decl::TypeSpecifier> {
//     // let token = iter.next(); 
//     // if token.is_none() {
//     //     return None;
//     // }

//     // match token.unwrap().unwrap().1 {
//     //     lexer::Token::Void => decl::TypeSpecifier::Void,
//     //     lexer::Token::Char => decl::TypeSpecifier::Char,
//     //     lexer::Token::Short => decl::TypeSpecifier::Short,
//     //     lexer::Token::Int => decl::TypeSpecifier::Int,
//     //     lexer::Token::Long => decl::TypeSpecifier::Long,
//     //     lexer::Token::Float => decl::TypeSpecifier::Float,
//     //     lexer::Token::Double => decl::TypeSpecifier::Double,
//     //     lexer::Token::Signed => decl::TypeSpecifier::Signed,
//     //     lexer::Token::Unsigned => decl::TypeSpecifier::Unsigned,
//     //     lexer::Token::Bool => decl::TypeSpecifier::Bool,
//     //     lexer::Token::Complex => decl::TypeSpecifier::Complex,
//     //     _ => None,
//     // }

//     // let atomic_type_specifier = AtomicTypeSpecifier(iter);
//     // return decl::TypeSpecifier::AtomicTypeSpecifier(atomic_type_specifier);
//     // let struct_or_union_specifier = StructOrUnionSpecifier(iter);
//     // return decl::TypeSpecifier::StructOrUnionSpecifier(struct_or_union_specifier);
//     // let enum_specifier = EnumSpecifier(iter);
//     // return decl::TypeSpecifier::EnumSpecifier(enum_specifier);
//     // let typedef_name = TypedefName(iter);
//     // return decl::TypeSpecifier::TypedefName(typedef_name);

    todo!()
}

fn parse_struct_or_union_specifier(parser: &mut ParseState) -> ParseResult<decl::StructOrUnionSpecifier> {
//     // StructOrUnion(); 
//     // Identifier(); 
//     // expect_token(iter, lexer::Token::LBrace); 
//     // StructDeclarationList(); 
//     // expect_token(iter, lexer::Token::RBrace);
    
//     // StructOrUnion(); 
//     // Identifier();
    todo!()
}

fn parse_struct_or_union(parser: &mut ParseState) -> ParseResult<decl::ObjKind> {
//     // let token = iter.next(); 
//     // if token.is_none() {
//     //     return None;
//     // }

//     // match token.unwrap() {
//     //     lexer::Token::Struct => decl::ObjKind::Struct,
//     //     lexer::Token::Union => decl::ObjKind::Union,
//     //     _ => None,
//     // }
    todo!()
}

fn parse_struct_declaration_list(parser: &mut ParseState) -> ParseResult<Vec<decl::StructDeclarator>> {
//     // StructDeclaration+
    todo!()
}

fn parse_struct_declaration(parser: &mut ParseState) -> ParseResult<decl::StructDeclaration> {
//     // let qualifier = SpecifierQualifierList(iter); 
//     // let declarator = StructDeclaratorList(iter); 
//     // expect_token(iter, lexer::Token::Semicolon);
//     // return decl::StructDeclaration::Field(decl::Filed{qualifier, declarator});
    
//     // let static_assert_declaration = StaticAssertDeclaration();
//     // return decl::StructDeclaration::StaticAssert(static_assert_declaration);
    todo!()
}

fn parse_specifier_qualifierList(parser: &mut ParseState) -> ParseResult<decl::SpecifierQualifierList> {
//     // TypeSpecifier(); TypeQualifier();
    todo!()
}

fn parse_struct_declarator_list(parser: &mut ParseState) -> ParseResult<Vec<decl::StructDeclarator>> {
//     // StructDeclarator(); <n: ("," StructDeclarator)*> 
//     // => todo!() 
    todo!()
}

fn parse_struct_declarator(parser: &mut ParseState) -> ParseResult<decl::StructDeclarator> {
//     // Declarator => todo!(),
//     // Declarator? ":" ConstantExpression;
    todo!()
}

fn parse_enum_specifier(parser: &mut ParseState) -> ParseResult<decl::EnumSpecifier> {
//     // "enum" Identifier? ("{" EnumeratorList ","? "}")?
//     // => todo!()
    todo!()
}

fn parse_enumerator_list(parser: &mut ParseState) -> ParseResult<decl::Enumerator> {
//     // Enumerator <l: ("," Enumerator)*> 
//     // => todo!()
    todo!()
}

fn parse_enumeration_constant(parser: &mut ParseState) -> ParseResult<String> {
//     // Identifier()
    todo!()
}

fn parse_enumerator(parser: &mut ParseState) -> ParseResult<decl::Enumerator> {
//     // EnumerationConstant <c: ("=" ConstantExpression)?> 
//     // => todo!()
    todo!()
}

fn parse_atomic_type_specifier(parser: &mut ParseState) -> ParseResult<decl::AtomicTypeSpecifier> {
//     // "_Atomic" "(" <typename: TypeName> ")"
//     // //AtomicTypeSpecifier{typename}
//     // => todo!()
    todo!()
}

fn parse_type_qualifier(parser: &mut ParseState) -> ParseResult<decl::TypeQualifier> {
//     // let token = iter.next(); 
//     // if token.is_none() {
//     //     return None;
//     // }

//     // match token.unwrap().unwrap().1 {
//     //     lexer::Token::Const => decl::TypeQualifier::Const,
//     //     lexer::Token::Restrict => decl::TypeQualifier::Restrict,
//     //     lexer::Token::Volatile => decl::TypeQualifier::Volatile,
//     //     lexer::Token::Atomic => decl::TypeQualifier::Atomic,
//     //     _ => None,
//     // }
    todo!()
}

fn parse_function_specifier(parser: &mut ParseState) -> ParseResult<decl::FunctionSpecifier> {
//     // let token = iter.next(); 
//     // if token.is_none() {
//     //     return None;
//     // }

//     // match token.unwrap().unwrap().1 {
//     //     lexer::Token::Inline => decl::FunctionSpecifier::Inline,
//     //     lexer::Token::Noreturn => decl::FunctionSpecifier::Noreturn,
//     //     _ => None,
//     // }
    todo!()
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
//     // Pointer(); DirectDeclarator();
//     // return decl::Declarator{}
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

fn parse_pointer(parser: &mut ParseState) -> ParseResult<r#type::Pointer> {
//     "*"; TypeQualifierList(iter);
    todo!()
}

fn parse_type_qualifier_list(parser: &mut ParseState) -> ParseResult<decl::TypeQualifier> {
//     TypeQualifier(iter);
    todo!()
}

fn parse_parameter_type_list(parser: &mut ParseState) -> ParseResult<decl::ParameterTypeList> {
//     // decl::ParameterTypeList{}
    todo!()
}

fn parse_parameter_list(parser: &mut ParseState) -> ParseResult<Vec<decl::ParameterDeclaration>> {
    todo!()
}

fn parse_parameter_declaration(parser: &mut ParseState) -> ParseResult<decl::ParameterDeclaration> {
//     // decl::ParameterDeclaration{}
//     // decl::ParameterDeclaration{}
    todo!()
}

fn parse_identifier_list(parser: &mut ParseState) -> ParseResult<Vec<String>> {
//     // Identifier ("," Identifier)*
    todo!()
}

fn parse_type_name(parser: &mut ParseState) -> ParseResult<String> {
//     // SpecifierQualifierList AbstractDeclarator?
    todo!()
}

fn parse_abstract_declarator(parser: &mut ParseState) -> ParseResult<decl::AbstractDeclarator> {
//     // Pointer => todo!(),
//     // Pointer? DirectAbstractDeclarator => 
    todo!()
}

fn parse_direct_abstract_declarator(parser: &mut ParseState) -> ParseResult<decl::DirectAbstractDeclarator> {
// //     "(" AbstractDeclarator ")"
// //     => todo!(),
// // DirectAbstractDeclarator? "[" TypeQualifierList? AssignmentExpression? "]"
// //     => todo!(),
// // DirectAbstractDeclarator? "[" "static" TypeQualifierList? AssignmentExpression "]"
// //     => todo!(),
// // DirectAbstractDeclarator? "[" TypeQualifierList "static" AssignmentExpression "]"
// //     => todo!(),
// // DirectAbstractDeclarator? "[" "*" "]"
// //     => todo!(),
// // DirectAbstractDeclarator? "(" ParameterTypeList? ")"
    todo!()
}

fn parse_initializer(parser: &mut ParseState) -> ParseResult<decl::Initializer> {
// //     <ae: AssignmentExpression>
// //     => todo!(),
// // "{" <il: InitializerList> ","? "}" 
// //     => decl::Initializer::InitializerList(il),
    todo!()
}

fn parse_initializer_list(parser: &mut ParseState) -> ParseResult<decl::InitializerList> {
//     // Designation? Initializer ("," Designation? Initializer)* 
    todo!()
}

fn parse_designation(parser: &mut ParseState) -> ParseResult<decl::Designator> {
//     // <DesignatorList> "="
    todo!()
}

fn parse_designator_list(parser: &mut ParseState) -> ParseResult<decl::Designator> {
//     // Vec<decl::Designator> = Designator+
    todo!()
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

fn parse_primary_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    parser.parse_any(&[
        Box::new(|iter| {
            let ident = parse_identifier(iter)?;
            Ok(expr::Expression::Identifier(ident))
        }),
        Box::new(|iter| {
            let constant = parse_constant(iter)?;
            Ok(expr::Expression::Literal(expr::Literal::Str(expr::StringLiteral{
                value: constant,
                prefix: None,
            })))
        }),
        Box::new(|iter| {
            let string = parse_string_literal(iter)?;
            Ok(expr::Expression::Literal(expr::Literal::Str(expr::StringLiteral{
                value: string,
                prefix: None,
            })))
        }),
        Box::new(|iter| {
            iter.match_token(lexer::Token::LParenthesis);
            let expression = parse_expression(iter)?; 
            iter.match_token(lexer::Token::RParenthesis);
            Ok(expression)
        }),
        Box::new(|iter| {
            let generic_selection = parse_generic_selection(iter)?;
            Ok(expr::Expression::GenericSelection(Box::new(generic_selection)))
        })
    ])
}

fn parse_generic_selection(parser: &mut ParseState) -> ParseResult<expr::GenericSelectionExpr> {
    parser.match_token(lexer::Token::Generic);
    parser.match_token(lexer::Token::LParenthesis);
    let expression = parse_assignment_expression(parser)?; 
    parser.match_token(lexer::Token::Comma);
    let association_list = parse_generic_assocList(parser)?; 
    parser.match_token(lexer::Token::RParenthesis);
    Ok(expr::GenericSelectionExpr{expression, association_list})
}

fn parse_generic_assocList(parser: &mut ParseState) -> ParseResult<Vec<expr::GenericAssociation>> {
    // GenericAssociation ("," GenericAssociation)* 
    todo!()
}

fn parse_generic_association(parser: &mut ParseState) -> ParseResult<expr::GenericAssociation> {
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

fn parse_postfix_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    parser.parse_any( &[
        Box::new(|iter| parse_primary_expression(iter)),
        Box::new(|iter| {
            let lhs = parse_postfix_expression(iter)?; 
            iter.match_token(lexer::Token::LBracket)?;
            let rhs = parse_expression(iter)?;
            iter.match_token(lexer::Token::RBracket)?;
            return Ok(expr::TwoOperandsExpr::new_subscript(lhs, rhs));
        }),
        Box::new(|iter| {
            let callie = parse_postfix_expression(iter)?; 
            iter.match_token(lexer::Token::LParenthesis);
            let args = iter.optional_parse(parse_argument_expression_list); 
            iter.match_token(lexer::Token::RParenthesis);
            return Ok(expr::CallExpr::new(callie, args.or(Default::default()).unwrap()));
        }),
        Box::new(|iter| {
            let lhs = parse_postfix_expression(iter)?; 
            iter.match_token(lexer::Token::Dot)?;
            let ident = parse_identifier(iter)?;    
            let rhs = expr::Expression::Identifier(ident);
            return Ok(expr::TwoOperandsExpr::new_access(lhs, rhs));
        }),
        Box::new(|iter| {
            let lhs = parse_postfix_expression(iter)?; 
            iter.match_token(lexer::Token::Arrow);
            let ident = parse_identifier(iter)?;
            let rhs = expr::Expression::Identifier(ident);
            return Ok(expr::TwoOperandsExpr::new_ptr_access(lhs, rhs));
        }),
        Box::new(|iter| {
            let expr = parse_postfix_expression(iter)?; 
            iter.match_token(lexer::Token::TwoPluses)?;
            return Ok(expr::OneOperandExpr::new_postincrement(expr));
        }),
        Box::new(|iter| {
            let expr = parse_postfix_expression(iter)?; 
            iter.match_token(lexer::Token::TwoMinuses)?;
            return Ok(expr::OneOperandExpr::new_postdecrement(expr));
        }),
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
        })
    ])
}

fn parse_argument_expression_list(parser: &mut ParseState) -> ParseResult<Vec<expr::Expression>> {
    // parser.parse_any(&[
    //     Box::new(|iter| parse_assignment_expression(iter)),
    //     Box::new(|iter| {
    //         iter.match_token(lexer::Token::Comma)?;
    //         parse_assignment_expression(iter)
    //     })
    // ])
    todo!()
}

fn parse_unary_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
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

fn parse_unary_operator(parser: &mut ParseState) -> ParseResult<expr::UnaryOp> {
    parser.next();
    use lexer::Token::*;
    match parser.iter.current_item.unwrap().value {
        lexer::Token::Ampersand => Ok(expr::UnaryOp::Address),
        lexer::Token::Asterisk => Ok(expr::UnaryOp::Indirection),
        lexer::Token::Plus => Ok(expr::UnaryOp::Positive),
        lexer::Token::Minus => Ok(expr::UnaryOp::Negative),
        lexer::Token::Tilde => Ok(expr::UnaryOp::BitwiseNot),
        lexer::Token::ExclamationMark => Ok(expr::UnaryOp::LogicalNot),
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

fn parse_cast_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    parser.parse_any(&[
        Box::new(|iter| parse_unary_expression(iter)),
        Box::new(|iter| {
            iter.match_token(lexer::Token::LParenthesis)?;
            parse_type_name(iter);  
            iter.match_token(lexer::Token::RParenthesis);
            let expr = parse_cast_expression(iter)?;
            return Ok(expr::CastExr::new((), expr));
        })
    ])
}

fn parse_multiplicative_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
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

fn parse_additive_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
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

fn parse_shift_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
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

fn parse_relational_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
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

fn parse_equality_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
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

fn parse_and_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
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

fn parse_exclusive_or_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
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

fn parse_inclusive_or_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
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

fn parse_logical_and_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
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

fn parse_logical_or_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
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

fn parse_conditional_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
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

fn parse_assignment_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
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

fn parse_assignment_operator(parser: &mut ParseState) -> ParseResult<expr::BiTag> {
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

fn parse_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
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

    use lexer::Token::*;
    enum Associativity {
        LeftToRight,
        RightToLeft,
    }
    let parse_table = [
        (Comma, 1, Associativity::LeftToRight),
        (Comma, 2, Associativity::LeftToRight),
        (Comma, 3, Associativity::LeftToRight),
        (Comma, 4, Associativity::LeftToRight),
        (Comma, 5, Associativity::LeftToRight),
        (Comma, 6, Associativity::LeftToRight),
        (Comma, 7, Associativity::LeftToRight),
        (Comma, 8, Associativity::LeftToRight),
        (Comma, 9, Associativity::LeftToRight),
        (Comma, 10, Associativity::LeftToRight),
        (Comma, 11, Associativity::LeftToRight),
        (Comma, 12, Associativity::LeftToRight),
        (Comma, 13, Associativity::LeftToRight),

        
        
        
        (AmpersandAssign, 14, Associativity::RightToLeft),
        (CaretAssign, 14, Associativity::RightToLeft),
        (VBarAssign, 14, Associativity::RightToLeft),
        (RGuillemetsAssign, 14, Associativity::RightToLeft),
        (LGuillemetsAssign, 14, Associativity::RightToLeft),
        (PercentAssign, 14, Associativity::RightToLeft),
        (SlashAssign, 14, Associativity::RightToLeft),
        (AsteriskAssign, 14, Associativity::RightToLeft),
        (MinusAssign, 14, Associativity::RightToLeft),
        (PlusAssign, 14, Associativity::RightToLeft),
        (Assign, 14, Associativity::RightToLeft),
        
        (Comma, 15, Associativity::LeftToRight),
    ];
    let mut ops = Vec::<lexer::Token>::new();
    let mut tmp = VecDeque::<lexer::Token>::new();
    
    parser.next();
    let tok = parser.iter.current_item.unwrap().value;
    
    if matches!(tok, Identifier | Constant | StringLiteral) {
        tmp.push_back(tok);
    }
    if matches!(tok, Comma) {
        ops.push(tok);
    }

    todo!()
}

fn parse_constant_expression(parser: &mut ParseState) -> ParseResult<expr::Expression> {
    parse_conditional_expression(parser)
}

// Statements Rules

pub(crate) fn parse_statement(parser: &mut ParseState) -> ParseResult<stmt::Statement> {
    parser.parse_any(&[
        Box::new(|iter| parse_labeled_statement(iter)),
        Box::new(|iter| {
            let compound = parse_compound_statement(iter)?;
            Ok(stmt::Statement::Compound(compound))
        }),
        Box::new(|iter| parse_expression_statement(iter)),
        Box::new(|iter| parse_selection_statement(iter)),
        Box::new(|iter| parse_iteration_statement(iter)),
        Box::new(|iter| parse_jump_statement(iter)),
    ])
}

fn parse_compound_statement(parser: &mut ParseState) -> ParseResult<stmt::CompoundStmt> {
    parser.match_token(lexer::Token::LBrace);
    let result = parser.expect_zero_or_n(parse_block_item);
    parser.match_token(lexer::Token::RBrace);
    return Ok(stmt::CompoundStmt::new(result));
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