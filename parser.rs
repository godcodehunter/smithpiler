use super::lexer;
use crate::ast::{expr, stmt, decl, self, r#type};
use std::{mem::MaybeUninit, marker::PhantomData};

// enum LexerResult {
//     UndefinedToken(usize),
//     EOF,
//     Token(lexer::Token),
// }

// struct MatchError {
//     expected: lexer::Token,
//     got: LexerResult,
// }

// enum ParserError {
//     MatchError(MatchError),
//     MatchAlternativeError(Vec<ParserError>),
// }

// type ParseResult<T> = Result<T, ParserError>;
// type ParseFunction<T> = fn()->ParseResult<T>;

// trait Parser<'input> {
//     type ResultValue;
//     const ROOT_RULE: ParseFunction<Self::ResultValue>;

//     fn new() -> Self {
//         Self {
//             input: unsafe { MaybeUninit::uninit() }, 
//             current_item: None,
//             iter: unsafe { MaybeUninit::uninit() },
//         }
//     }

//     fn parse(&mut self, input: &'input str) -> ParseResult<Self::ResultValue> {
//         self.input = input;
//         self.iter = lexer::token_iter(input, false);
//         Self::ROOT_RULE(self)
//     }
    
//     fn next(&mut self) {
//         self.current_item = self.iter.next();
//     }

//     fn match_token(&self, expected: lexer::Token) -> Result<(), ParserError>{ 
//         let item = self.current_item;
//         if item.is_none() {
//             return Err(ParserError::MatchError(MatchError{expected, got: LexerResult::EOF}));
//         }
//         let lexer_item = item.unwrap(); 
//         if lexer_item.is_err() {
//             let err = lexer_item.unwrap_err();
//             return Err(ParserError::MatchError(MatchError{expected, got: LexerResult::UndefinedToken(err.0)}))
//         }
//         let token = lexer_item.unwrap();
//         if token.1 != expected {
//             return Err(ParserError::MatchError(MatchError{expected, got: LexerResult::Token(token.1)}));
//         }
//         Ok(())
//     }
    
//     fn optional_parse<T>(&self, func: ParseFunction<T>) -> ParseResult<Option<T>> {
//         // let result = func(self.clone());
//         // if result.is_err() {
//         //     let err = result.unwrap_err();
//         //     let ParserError::MatchError(MatchError{got: LexerResult(LexerResult::UndefinedToken(_)), expected: _}) = err {
//         //         return result;
//         //     }
//         //     return Ok(None);
//         // }
//         // result.map(|val| Some(val))
//         todo!()
//     }

    // fn parse_any<T>(&self, alternatives: &[ParseFunction<T>]) -> ParseResult<T> {
    //     let errors: Vec<ParserError>;
    //     for alternative in alternatives {
    //         let result = alternative(self.clone());
    //         if result.is_ok() {
    //             return result;
    //         }
    //         errors.push(result.unwrap_err())
    //     }
    //     Err(ParserError::MatchAlternativeError(errors))
    // }

//     fn expect_zero_or_n<Iter: lexer::TokenStream, T>(iter: &Iter, func: fn(Iter)->ParseResult<T>) -> ParseResult<Vec<T>>{
//         todo!()
//     }
    
//     fn expect_one_or_n<Iter: lexer::TokenStream + Clone, T>(iter: &Iter, func: fn(Iter)->ParseResult<T>) -> ParseResult<Vec<T>> {
//         // let mut result: Vec<T>;
//         // loop {
//         //     let value = func(iter.clone());
//         //     if value.is_ok() { 
//         //         break; 
//         //     }
//         //     result.push(value.unwrap());
//         // }
//         // Err(ParserError)
//         todo!()
//     }
// }

// fn stringify_current_token(self) -> String {
//     let tok = self.current_item.unwrap().unwrap();
//     self.input[tok.0..tok.2].to_string()
// }

// fn parse_identifier(self) -> ParseResult<String> {
//     self.match_token(lexer::Token::Identifier)?;
//     Ok(self.stringify_current_token())
// }

// fn parse_typedef_name(self) -> ParseResult<String> {
//     self.match_token(lexer::Token::Identifier)?;
//     Ok(self.stringify_current_token())
// }

// fn parse_string_literal(self) -> ParseResult<String> {
//     self.match_token(lexer::Token::StringLiteral)?;
//     Ok(self.stringify_current_token())
// }


// fn TranslationUnit(iter: impl lexer::TokenStream + Clone) -> ParseResult<ast::Ast> {
//     // let external_declarations = expect_zero_or_n(iter, ExternalDeclaration);
//     // ast::Ast(external_declarations.unwrap())
//     todo!()
// }

// fn ExternalDeclaration(iter: impl lexer::TokenStream + Clone) -> ParseResult<ast::ExternalDeclaration> {
//     // let result1 = FunctionDefinition(iter);
//     // if result1.is_some() {
//     //     ast::ExternalDeclaration::FunctionDefinition(Box::new(result1))
//     // }
    
//     // let result2 = Declaration(iter);
//     // if result2.is_some() {
//     //     ast::ExternalDeclaration::Declaration(Box::new(result2));
//     // }

//     // None
//     todo!()
// }

// fn DeclarationList(iter: impl lexer::TokenStream + Clone) -> ParseResult<Vec<decl::Declaration>> {
//     // let result: Vec<decl::Declaration>;
//     // loop {
//     //     let value = Declaration(iter);
//     //     if value.is_none() { 
//     //         break; 
//     //     }
//     //     result.0.push(value.unwrap());
//     // }
//     // if !result.is_empty() {
//     //     Some(result)
//     // }
//     // None
//     todo!()
// }

// fn FunctionDefinition(iter: impl lexer::TokenStream + Clone) -> ParseResult<ast::FunctionDefinition> {
//     let specifiers = DeclarationSpecifiers(iter);
//     let declarator = Declarator(iter);
//     let declaration_list = DeclarationList(iter); 
//     let compound_statement = CompoundStatement(iter);

//     return Ok(ast::FunctionDefinition::new(specifiers, declarator, declaration_list, compound_statement));
// }

// fn Declaration(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::Declaration> {
//     parse_any(&iter, &[
//         |iter| {
//             let declaration = DeclarationSpecifiers(iter); 
//             let init = InitDeclaratorList(iter); 
//             expect_token(&iter, lexer::Token::Semicolon)?;
//             return Ok(decl::Declaration::Other(decl::Other{specifiers: declaration, init_declarators: init}));
//         },
//         |iter| StaticAssertDeclaration(iter),
//     ])
// }

// fn DeclarationSpecifier(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::DeclarationSpecifier> {
//     let storage_class_specifier = StorageClassSpecifier(iter);
//     if storage_class_specifier.is_some() {
//         return decl::DeclarationSpecifier::StorageClassSpecifier(storage_class_specifier);
//     }
//     let type_specifier = TypeSpecifier(iter);
//     if type_specifier.is_some() {
//         decl::DeclarationSpecifier::TypeSpecifier(type_specifier);
//     }
//     let type_qualifier = TypeQualifier(iter);
//     if type_qualifier.is_some() {
//         decl::DeclarationSpecifier::TypeQualifier(type_qualifier); 
//     }
//     let function_specifier = FunctionSpecifier(iter);
//     if function_specifier.is_some() {
//         decl::DeclarationSpecifier::FunctionSpecifier(function_specifier); 
//     }
//     let alignment_specifier = AlignmentSpecifier(iter);
//     if alignment_specifier.is_some() {
//         decl::DeclarationSpecifier::AlignmentSpecifier(alignment_specifier); 
//     }

//     None
// }

// fn DeclarationSpecifiers(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::DeclarationSpecifier> { 
//     let result: Vec<decl::DeclarationSpecifier>;
//     loop {
//         let value = DeclarationSpecifier(iter);
//         if value.is_none() { 
//             break; 
//         }
//         result.0.push(value.unwrap());
//     }
//     if !result.is_empty() {
//         Some(result)
//     }
//     None
// }

// fn InitDeclaratorList(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::InitDeclarator> {
//     let mut result;
    
//     let init_declarator = InitDeclarator(iter); 
//     if init_declarator.is_none() {
//         None
//     }
//     loop {
//         let token = iter.next(); 
//         if token.is_none() {
//             return None;
//         }
//         if token.unwrap() != lexer::Token::Comma {
//             return None;
//         }
        
//         InitDeclarator()
//     }

//     if !result.is_empty() {
//         Some(result)
//     }
//     None
// }

// fn InitDeclarator(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::InitDeclarator> {
//     // let declarator = Declarator(iter);
    
//     // let declarator = Declarator(iter);
//     // expect_token(&iter, lexer::Token::Assign);
//     // let initializer = Initializer(iter);
//     todo!()
// }

// fn StorageClassSpecifier(iter: impl lexer::TokenStream) -> ParseResult<decl::StorageClassSpecifier> {
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
// }

// fn TypeSpecifier(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::TypeSpecifier> {
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

//     todo!()
// }

// fn StructOrUnionSpecifier(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::StructOrUnionSpecifier> {
//     // StructOrUnion(); 
//     // Identifier(); 
//     // expect_token(iter, lexer::Token::LBrace); 
//     // StructDeclarationList(); 
//     // expect_token(iter, lexer::Token::RBrace);
    
//     // StructOrUnion(); 
//     // Identifier();
//     todo!()
// }

// fn StructOrUnion(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::ObjKind> {
//     // let token = iter.next(); 
//     // if token.is_none() {
//     //     return None;
//     // }

//     // match token.unwrap() {
//     //     lexer::Token::Struct => decl::ObjKind::Struct,
//     //     lexer::Token::Union => decl::ObjKind::Union,
//     //     _ => None,
//     // }
//     todo!()
// }

// fn StructDeclarationList(iter: impl lexer::TokenStream + Clone) -> ParseResult<Vec<decl::StructDeclarator>> {
//     // StructDeclaration+
//     todo!()
// }

// fn StructDeclaration(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::StructDeclaration> {
//     // let qualifier = SpecifierQualifierList(iter); 
//     // let declarator = StructDeclaratorList(iter); 
//     // expect_token(iter, lexer::Token::Semicolon);
//     // return decl::StructDeclaration::Field(decl::Filed{qualifier, declarator});
    
//     // let static_assert_declaration = StaticAssertDeclaration();
//     // return decl::StructDeclaration::StaticAssert(static_assert_declaration);
//     todo!()
// }

// fn SpecifierQualifierList(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::SpecifierQualifierList> {
//     // TypeSpecifier(); TypeQualifier();
//     todo!()
// }

// fn StructDeclaratorList(iter: impl lexer::TokenStream + Clone) -> ParseResult<Vec<decl::StructDeclarator>> {
//     // StructDeclarator(); <n: ("," StructDeclarator)*> 
//     // => todo!() 
//     todo!()
// }

// fn StructDeclarator(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::StructDeclarator> {
//     // Declarator => todo!(),
//     // Declarator? ":" ConstantExpression;
//     todo!()
// }

// fn EnumSpecifier(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::EnumSpecifier> {
//     // "enum" Identifier? ("{" EnumeratorList ","? "}")?
//     // => todo!()
//     todo!()
// }

// fn EnumeratorList(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::Enumerator> {
//     // Enumerator <l: ("," Enumerator)*> 
//     // => todo!()
//     todo!()
// }

// fn EnumerationConstant(iter: impl lexer::TokenStream + Clone) -> ParseResult<String> {
//     // Identifier()
//     todo!()
// }

// fn Enumerator(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::Enumerator> {
//     // EnumerationConstant <c: ("=" ConstantExpression)?> 
//     // => todo!()
//     todo!()
// }

// fn AtomicTypeSpecifier(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::AtomicTypeSpecifier> {
//     // "_Atomic" "(" <typename: TypeName> ")"
//     // //AtomicTypeSpecifier{typename}
//     // => todo!()
//     todo!()
// }

// fn TypeQualifier(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::TypeQualifier> {
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
//     todo!()
// }

// fn FunctionSpecifier(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::FunctionSpecifier> {
//     // let token = iter.next(); 
//     // if token.is_none() {
//     //     return None;
//     // }

//     // match token.unwrap().unwrap().1 {
//     //     lexer::Token::Inline => decl::FunctionSpecifier::Inline,
//     //     lexer::Token::Noreturn => decl::FunctionSpecifier::Noreturn,
//     //     _ => None,
//     // }
//     todo!()
// }

// fn AlignmentSpecifier(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::AlignmentSpecifier> {
//     // lexer::Token::Alignas; 
//     // lexer::Token::LParenthesis; 
//     // TypeName(); 
//     // lexer::Token::RParenthesis;
    
//     // lexer::Token::Alignas; 
//     // lexer::Token::LParenthesis; 
//     // ConstantExpression(); 
//     // lexer::Token::RParenthesis;
//     todo!()
// }

// fn Declarator(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::Declarator> {
//     // Pointer(); DirectDeclarator();
//     // return decl::Declarator{}
//     todo!()
// }

// fn DirectDeclarator(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::DirectDeclarator> {
//     // parse_any(&iter, &[
//     //     |iter| {
//     //         let identifier = Identifier(iter);
//     //         return decl::DirectDeclarator::FunctionDeclarators;
//     //     },
//     //     |iter| {
//     //         expect_token(iter, lexer::Token::LParenthesis);
//     //         Declarator(); 
//     //         expect_token(iter, lexer::Token::RParenthesis);
//     //         return decl::DirectDeclarator::FunctionDeclarators;
//     //     },
//     //     |iter| {
//     //         DirectDeclarator(iter);
//     //         expect_token(iter, lexer::Token::LBracket); 
//     //         TypeQualifierList(iter); 
//     //         AssignmentExpression(iter); 
//     //         expect_token(iter, lexer::Token::RBracket);
//     //         return decl::DirectDeclarator::FunctionDeclarators;
//     //     },
//     //     |iter| {
//     //         DirectDeclarator(); 
//     //         "["; 
//     //         "static"; 
//     //         TypeQualifierList(); 
//     //         AssignmentExpression(); 
//     //         "]"; 
//     //         return decl::DirectDeclarator::FunctionDeclarators;
//     //     },
//     //     |iter| {
//     //         DirectDeclarator(); 
//     //         "["; 
//     //         TypeQualifierList(); 
//     //         "static"; 
//     //         AssignmentExpression(); 
//     //         "]"; 
//     //         return decl::DirectDeclarator::FunctionDeclarators;
//     //     }, 
//     //     |iter| {
//     //         DirectDeclarator(); 
//     //         "["; 
//     //         TypeQualifierList; 
//     //         "*"; 
//     //         "]"; 
//     //         return decl::DirectDeclarator::FunctionDeclarators;
//     //     },
//     //     |iter| {
//     //         DirectDeclarator(); 
//     //         "("; 
//     //         ParameterTypeList; 
//     //         ")"; 
//     //         return decl::DirectDeclarator::FunctionDeclarators;
//     //     },
//     //     |iter| {
//     //         DirectDeclarator(); 
//     //         "("; 
//     //         IdentifierList(); 
//     //         ")"; 
//     //         return decl::DirectDeclarator::FunctionDeclarators;
//     //     }
//     // ])
//     todo!()
// }

// fn Pointer(iter: impl lexer::TokenStream + Clone) -> ParseResult<r#type::Pointer> {
//     "*"; TypeQualifierList(iter);
//     todo!()
// }

// fn TypeQualifierList(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::TypeQualifier> {
//     TypeQualifier(iter);
//     todo!()
// }

// fn ParameterTypeList(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::ParameterTypeList> {
//     // decl::ParameterTypeList{}
//     todo!()
// }

// fn ParameterList(iter: impl lexer::TokenStream + Clone) -> ParseResult<Vec<decl::ParameterDeclaration>> {
//     todo!()
// }

// fn ParameterDeclaration(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::ParameterDeclaration> {
//     // decl::ParameterDeclaration{}
//     // decl::ParameterDeclaration{}
//     todo!()
// }

// fn IdentifierList(iter: impl lexer::TokenStream + Clone) -> ParseResult<Vec<String>> {
//     // Identifier ("," Identifier)*
//     todo!()
// }

// fn TypeName(iter: impl lexer::TokenStream + Clone) -> ParseResult<String> {
//     // SpecifierQualifierList AbstractDeclarator?
//     todo!()
// }

// fn AbstractDeclarator(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::AbstractDeclarator> {
//     // Pointer => todo!(),
//     // Pointer? DirectAbstractDeclarator => 
//     todo!()
// }

// fn DirectAbstractDeclarator(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::DirectAbstractDeclarator> {
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
//     todo!()
// }

// fn Initializer(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
// //     <ae: AssignmentExpression>
// //     => todo!(),
// // "{" <il: InitializerList> ","? "}" 
// //     => decl::Initializer::InitializerList(il),
//     todo!()
// }

// fn InitializerList(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::InitializerList> {
//     // Designation? Initializer ("," Designation? Initializer)* 
//     todo!()
// }

// fn Designation(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::Designator> {
//     // <DesignatorList> "="
//     todo!()
// }

// fn DesignatorList(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::Designator> {
//     // Vec<decl::Designator> = Designator+
//     todo!()
// }

// fn Designator(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::Designator> {
//     parse_any(&iter, &[
//         |iter| {
//             expect_token(&iter, lexer::Token::LBracket);
//             let expr = ConstantExpression(iter); 
//             expect_token(&iter, lexer::Token::RBracket);
//             return Ok(decl::Designator::new_index(expr));
//         },
//         |iter| {
//             expect_token(&iter, lexer::Token::Dot);
//             let ident = Identifier(iter);
//             return Ok(decl::Designator::new_dot(ident));
//         }
//     ])
// }

// fn StaticAssertDeclaration(iter: impl lexer::TokenStream + Clone) -> ParseResult<decl::StaticAssert> {
//     expect_token(&iter, lexer::Token::StaticAssert);
//     expect_token(&iter, lexer::Token::LParenthesis);
//     let expr = ConstantExpression(iter); 
//     expect_token(&iter, lexer::Token::Comma);
//     let msg = StringLiteral(iter); 
//     expect_token(&iter, lexer::Token::RParenthesis);
//     expect_token(&iter, lexer::Token::Semicolon);
//     return Ok(decl::StaticAssert::new(expr, msg));
// }

// fn PrimaryExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     // expect_token(iter, lexer::Token::Identifier);
//     // return expr::Expression::Identifier();
//     // lexer::Token::Constant;
//     // StringLiteral();
//     // "("; Expression(); ")";
//     // GenericSelection();
//     todo!()
// }

// fn GenericSelection(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::GenericSelectionExpr> {
//     expect_token(&iter, lexer::Token::Generic);
//     expect_token(&iter, lexer::Token::LParenthesis);
//     let expression = AssignmentExpression(iter); 
//     expect_token(&iter, lexer::Token::Comma);
//     let association_list = GenericAssocList(iter); 
//     expect_token(&iter, lexer::Token::RParenthesis);
//     todo!()
// }

// fn GenericAssocList(iter: impl lexer::TokenStream + Clone) -> ParseResult<Vec<expr::GenericAssociation>> {
//     // GenericAssociation ("," GenericAssociation)* 
//     todo!()
// }

// fn GenericAssociation(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::GenericAssociation> {
//     parse_any(&iter, &[
//         |iter| {
//             TypeName(iter); 
//             expect_token(&iter, lexer::Token::Colon)?; 
//             let expression = AssignmentExpression(iter);
//             return Ok(expr::GenericAssociation{expression});
//         },
//         |iter| {
//             expect_token(&iter, lexer::Token::Default)?;
//             expect_token(&iter, lexer::Token::Colon)?;
//             let expression = AssignmentExpression(iter);
//             return Ok(expr::GenericAssociation{expression});
//         }
//     ])
// }

// fn PostfixExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| PrimaryExpression(iter),
//         |iter| {
//             let lhs = PostfixExpression(iter); 
//             expect_token(&iter, lexer::Token::LBracket)?;
//             let rhs = Expression(iter);
//             expect_token(&iter, lexer::Token::RBracket)?;
//             return Ok(expr::TwoOperandsExpr::new_subscript(lhs, rhs));
//         },
//         |iter| {
//             let callie = PostfixExpression(iter); 
//             expect_token(&iter, lexer::Token::LParenthesis);
//             let args = ArgumentExpressionList(iter); 
//             expect_token(&iter, lexer::Token::RParenthesis);
//             return Ok(expr::CallExpr::new(callie, args.or(Default::default()).unwrap()));
//         },
//         |iter| {
//             let lhs = PostfixExpression(iter); 
//             expect_token(&iter, lexer::Token::Dot)?;
//             let ident = Identifier(iter);    
//             let rhs = expr::Expression::Identifier(ident);
//             return Ok(expr::TwoOperandsExpr::new_access(lhs, rhs));
//         },
//         |iter| {
//             let lhs = PostfixExpression(iter); 
//             expect_token(&iter, lexer::Token::Arrow);
//             let ident = Identifier(iter);
//             let rhs = expr::Expression::Identifier(ident);
//             return Ok(expr::TwoOperandsExpr::new_ptr_access(lhs, rhs));
//         },
//         |iter| {
//             let expr = PostfixExpression(iter); 
//             expect_token(&iter, lexer::Token::TwoPluses)?;
//             return Ok(expr::OneOperandExpr::new_postincrement(expr));
//         },
//         |iter| {
//             let expr = PostfixExpression(iter); 
//             expect_token(&iter, lexer::Token::TwoMinuses)?;
//             return Ok(expr::OneOperandExpr::new_postdecrement(expr));
//         },
//         |iter| {
//             expect_token(&iter, lexer::Token::LParenthesis);
//             TypeName(iter); 
//             expect_token(&iter, lexer::Token::RParenthesis);
//             expect_token(&iter, lexer::Token::LBrace);
//             let init_list = InitializerList(iter); 
//             expect_token(&iter, lexer::Token::RParenthesis);
//             return Ok(expr::CompoundLiteral::new(Box::new(init_list)));
//         },
//         |iter| {
//             expect_token(&iter, lexer::Token::LParenthesis);
//             TypeName(iter); 
//             expect_token(&iter, lexer::Token::RParenthesis);
//             expect_token(&iter, lexer::Token::LBrace);
//             let init_list = InitializerList(iter); 
//             expect_token(&iter, lexer::Token::Dot);
//             expect_token(&iter, lexer::Token::RBrace);
//             return Ok(expr::CompoundLiteral::new(Box::new(init_list)));
//         }
//     ])
// }

// fn ArgumentExpressionList(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| AssignmentExpression(iter),
//         |iter| {
//             expect_token(&iter, lexer::Token::Comma)?;
//             AssignmentExpression(iter)
//         }
//     ])
// }

// fn UnaryExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| PostfixExpression(iter),
//         |iter| {
//             expect_token(iter, lexer::Token::TwoPluses)?;
//             let unary_expression= UnaryExpression(iter);
//             return Ok(expr::OneOperandExpr::new_increment(unary_expression));
//         },
//         |iter| {
//             expect_token(iter, lexer::Token::TwoMinuses)?;
//             let unary_expression = UnaryExpression(iter);
//             return Ok(expr::OneOperandExpr::new_decrement(unary_expression));
//         },
//         |iter| {
//             // UnaryOperator(); CastExpression();
//             todo!()
//         },
//         |iter| {
//             expect_token(&iter, lexer::Token::Sizeof);
//             let unary_expression = UnaryExpression(iter); 
//             return Ok(expr::Sizeof::new(unary_expression));
//         },
//         |iter| {
//             expect_token(&iter, lexer::Token::Sizeof);
//             expect_token(&iter, lexer::Token::LParenthesis);
//             let type_name = TypeName(iter);
//             expect_token(&iter, lexer::Token::RParenthesis);
//             return Ok(expr::Sizeof::new(type_name));
//         },
//         |iter| {
//             expect_token(&iter, lexer::Token::Alignof);
//             expect_token(&iter, lexer::Token::LParenthesis);
//             let type_name = TypeName(iter); 
//             expect_token(&iter, lexer::Token::RParenthesis);
//             return Ok(expr::AlignOf::new(type_name));
//         }
//     ])
// }

// fn UnaryOperator(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::UnaryOp> {
//     let token = iter.next(); 
//     if token.is_none() {
//         return None;
//     }

//     return match token.unwrap().unwrap().1 {
//         lexer::Token::Ampersand => Ok(expr::UnaryOp::Address),
//         lexer::Token::Asterisk => Ok(expr::UnaryOp::Indirection),
//         lexer::Token::Plus => Ok(expr::UnaryOp::Positive),
//         lexer::Token::Minus => Ok(expr::UnaryOp::Negative),
//         lexer::Token::Tilde => Ok(expr::UnaryOp::BitwiseNot),
//         lexer::Token::ExclamationMark => Ok(expr::UnaryOp::LogicalNot),
//         _ => None,
//     }
// }

// fn CastExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| UnaryExpression(iter),
//         |iter| {
//             expect_token(&iter, lexer::Token::LParenthesis)?;
//             TypeName(iter);  
//             expect_token(&iter, lexer::Token::RParenthesis);
//             let expr = CastExpression(iter);
//             return Ok(expr::CastExr::new((), expr));
//         }
//     ]);
// }

// fn MultiplicativeExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| CastExpression(iter),
//         |iter| {
//             let lhs = MultiplicativeExpression(iter); 
//             expect_token(&iter, lexer::Token::Asterisk)?;
//             let rhs = CastExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_mul(lhs, rhs));
//         },
//         |iter| {
//             let lhs = MultiplicativeExpression(iter); 
//             expect_token(&iter, lexer::Token::Slash)?;
//             let rhs = CastExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_div(lhs, rhs));
//         },
//         |iter| {
//             let lhs = MultiplicativeExpression(iter); 
//             expect_token(&iter, lexer::Token::Percent)?; 
//             let rhs = CastExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_rem(lhs, rhs));
//         }
//     ])
// }

// fn AdditiveExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//     |iter| MultiplicativeExpression(iter),
//         |iter| {
//             let lhs = AdditiveExpression(iter); 
//             expect_token(&iter, lexer::Token::Plus)?; 
//             let rhs = MultiplicativeExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_add(lhs, rhs));
//         },
//         |iter| {
//             let lhs = AdditiveExpression(iter); 
//             expect_token(&iter, lexer::Token::Minus)?;  
//             let rhs = MultiplicativeExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_sub(lhs, rhs));
//         }
//     ]);
// }

// fn ShiftExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| AdditiveExpression(iter),
//         |iter| {
//             let lhs = ShiftExpression(iter);
//             expect_token(&iter, lexer::Token::TwoSmaller)?; 
//             let rhs = AdditiveExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_left_shift(lhs, rhs));
//         },
//         |iter| {
//             let lhs = ShiftExpression(iter); 
//             expect_token(&iter, lexer::Token::TwoLager)?; 
//             let rhs = AdditiveExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_right_shift(lhs, rhs));
//         }
//     ])
// }

// fn RelationalExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| ShiftExpression(iter),
//         |iter| {
//             let lhs = RelationalExpression(iter); 
//             expect_token(&iter, lexer::Token::Smaller)?; 
//             let rhs = ShiftExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_le(lhs, rhs));
//         },
//         |iter| {
//             let lhs = RelationalExpression(iter); 
//             expect_token(&iter, lexer::Token::Lager)?; 
//             let rhs = ShiftExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_ge(lhs, rhs));
//         },
//         |iter| {
//             let lhs = RelationalExpression(iter); 
//             expect_token(&iter, lexer::Token::LeOrEq)?; 
//             let rhs = ShiftExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_ge_or_eq(lhs, rhs));
//         },
//         |iter| {
//             let lhs = RelationalExpression(iter); 
//             expect_token(&iter, lexer::Token::GeOrEq)?; 
//             let rhs = ShiftExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_ge_or_eq(lhs, rhs));
//         }
//     ])
// }

// fn EqualityExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| RelationalExpression(iter),
//         |iter| {
//             let lhs = EqualityExpression(iter); 
//             expect_token(&iter, lexer::Token::Equal)?; 
//             let rhs = RelationalExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_eq(lhs, rhs));
//         },
//         |iter| {
//             let lhs = EqualityExpression(iter); 
//             expect_token(&iter, lexer::Token::NotEq)?;  
//             let rhs = RelationalExpression(iter); 
//             return Ok(expr::TwoOperandsExpr::new_not_eq(lhs, rhs));
//         }
//     ])
// }

// fn AndExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| EqualityExpression(iter),
//         |iter| {
//             let lhs = AndExpression(iter); 
//             expect_token(&iter, lexer::Token::Ampersand)?; 
//             let rhs = EqualityExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_bw_and(lhs, rhs));
//         }
//     ])
// }

// fn ExclusiveOrExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| AndExpression(iter),
//         |iter| {
//             let lhs = ExclusiveOrExpression(iter); 
//             expect_token(&iter, lexer::Token::Caret)?; 
//             let rhs = AndExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_bw_xor(lhs, rhs));
//         }
//     ])
// }

// fn InclusiveOrExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| ExclusiveOrExpression(iter),
//         |iter| {
//             let lhs = InclusiveOrExpression(iter); 
//             expect_token(&iter, lexer::Token::Bar)?; 
//             let rhs = ExclusiveOrExpression(iter); 
//             return Ok(expr::TwoOperandsExpr::new_bw_or(lhs, rhs));
//         }
//     ])
// }

// fn LogicalAndExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| InclusiveOrExpression(iter),
//         |iter| {
//             let lhs = LogicalAndExpression(iter); 
//             expect_token(&iter, lexer::Token::WAmpersand)?; 
//             let rhs = InclusiveOrExpression(iter); 
//             return Ok(expr::TwoOperandsExpr::new_logical_and(lhs, rhs));
//         }
//     ])
// }

// fn LogicalOrExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| LogicalAndExpression(iter),
//         |iter| {
//             let lhs = LogicalOrExpression(iter); 
//             expect_token(&iter, lexer::Token::WBar)?; 
//             let rhs = LogicalAndExpression(iter);
//             return Ok(expr::TwoOperandsExpr::new_logical_or(lhs, rhs));
//         }
//     ])
// }

// fn ConditionalExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| LogicalOrExpression(iter),
//         |iter| {
//             let lhs = LogicalOrExpression(iter); 
//             expect_token(&iter, lexer::Token::QuestionMark)?; 
//             let rhs= Expression(iter); 
//             lexer::Token::Colon; 
//             ConditionalExpression(iter);
//             return Ok(expr::Expression::Ternary(Box::new(expr::Ternary{})));
//         }
//     ])
// }

// fn AssignmentExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     parse_any(&iter, &[
//         |iter| ConditionalExpression(iter),
//         |iter| {
//             let lhs = UnaryExpression(iter); 
//             let op = AssignmentOperator(iter); 
//             let rhs = AssignmentExpression(iter);
//             return Ok(expr::Expression::TwoOperands(Box::new(expr::TwoOperandsExpr{lhs, op, rhs})));
//         }
//     ])
// }

// fn AssignmentOperator(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::BiTag> {
//     let token = iter.next(); 
//     if token.is_none() {
//         return None;
//     }

//     return match token.unwrap().unwrap().1 {
//         lexer::Token::Assign => Ok(expr::BiTag::Assign),
//         lexer::Token::AsteriskAssign => Ok(expr::BiTag::MulAssign),
//         lexer::Token::SlashAssign => Ok(expr::BiTag::DivAssign),
//         lexer::Token::PercentAssign => Ok(expr::BiTag::RemAssign),
//         lexer::Token::PlusAssign => Ok(expr::BiTag::AddAssign),
//         lexer::Token::MinusAssign => Ok(expr::BiTag::SubAssign),
//         lexer::Token::LGuillemetsAssign => Ok(expr::BiTag::BwLShAssign),
//         lexer::Token::RGuillemetsAssign => Ok(expr::BiTag::BwRShAssign),
//         lexer::Token::AmpersandAssign => Ok(expr::BiTag::BwAndAssign),
//         lexer::Token::CaretAssign => Ok(expr::BiTag::BwXorAssign),
//         lexer::Token::VBarAssign => Ok(expr::BiTag::BwOrAssign),
//         _ => Err(ParserError),
//     };
// }

// fn parse_expression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     return expr::TwoOperandsExpr::new_comma();
    // todo!()
// }

// fn ConstantExpression(iter: impl lexer::TokenStream + Clone) -> ParseResult<expr::Expression> {
//     ConditionalExpression(iter);
// }

// fn Statement(iter: impl lexer::TokenStream + Clone) -> ParseResult<stmt::Statement> {
//     parse_any(&iter, &[
//         |iter| LabeledStatement(iter),
//         |iter| CompoundStatement(iter),
//         |iter| ExpressionStatement(iter),
//         |iter| SelectionStatement(iter),
//         |iter| IterationStatement(iter),
//         |iter| JumpStatement(iter),
//     ])
// }

// fn CompoundStatement(iter: impl lexer::TokenStream + Clone) -> ParseResult<stmt::CompoundStmt> {
//     let compound_statement = CompoundStatement(iter);
//     if compound_statement.is_some() {
//         return stmt::CompoundStmt::new(compound_statement);
//     }
// }

// fn BlockItem(iter: impl lexer::TokenStream + Clone) -> ParseResult<stmt::BlockItem> {
//     parse_any(&iter, &[
//         |iter| {
//             let declaration = Declaration(iter);
//             if declaration.is_some() {
//                 return stmt::BlockItem::Declaration(declaration);
//             }
//             None
//         },
//         |iter| {
//             let statement = Statement(iter);
//             if statement.is_some() {
//                 return stmt::BlockItem::Statement(statement);
//             }
//             None
//         }
//     ])
// }

// fn ExpressionStatement(iter: impl lexer::TokenStream + Clone) -> ParseResult<stmt::Statement> {
//     let expression = Expression(iter);
//     expect_token(&iter, lexer::Token::Colon)?;
//     return Ok(stmt::Statement::new_expr_stmt(expression));
// }

// fn LabeledStatement(iter: impl lexer::TokenStream + Clone) -> ParseResult<stmt::Statement> {
//     parse_any(&iter, &[
//         |iter| {
//             let identifier = Identifier(iter);
//             expect_token(&iter, lexer::Token::Colon)?;
//             let statement= Statement(iter);
//             return Ok(stmt::Labeled::new(identifier, statement));
//         },
//         |iter| {
//             expect_token(&iter, lexer::Token::Case)?;
//             let constant = ConstantExpression(iter);
//             expect_token(&iter, lexer::Token::Colon)?;
//             let body = Statement(iter);
//             return Ok(stmt::Case::new(constant, body));
//         },
//         |iter| {
//             expect_token(&iter, lexer::Token::Default)?;
//             expect_token(&iter, lexer::Token::Colon)?;
//             let body = Statement(iter);
//             return Ok(stmt::Default::new(body));
//         },
//     ])
// }

// fn SelectionStatement(iter: impl lexer::TokenStream + Clone) -> ParseResult<stmt::Statement> {
//     parse_any(&iter, &[
//         |iter| {
//             expect_token(&iter, lexer::Token::Int)?;
//             expect_token(&iter, lexer::Token::LParenthesis)?;
//             let expression = Expression(iter);
//             expect_token(&iter, lexer::Token::RParenthesis)?;
//             let statement = Statement(iter);
//             return Ok(stmt::IfStmt::new(expression, statement));
//         },
//         |iter| {
//             expect_token(&iter, lexer::Token::If)?;
//             expect_token(&iter, lexer::Token::LParenthesis)?;
//             let predicate = Expression(iter);
//             expect_token(&iter, lexer::Token::RParenthesis)?;
//             let on_success = Statement(iter); 
//             expect_token(&iter, lexer::Token::Else)?;
//             let on_failure = Statement(iter);
//             return Ok(stmt::Statement::IfElse(stmt::IfElseStmt{predicate, on_success, on_failure}));
//         },
//         |iter| {
//             expect_token(&iter, lexer::Token::Switch)?;
//             expect_token(&iter, lexer::Token::LParenthesis)?;
//             let controlling = Expression(iter);
//             expect_token(&iter, lexer::Token::RParenthesis)?;
//             let body = Statement(iter);
//             return Ok(stmt::Statement::new_switch(controlling, body));
//         }
//     ])
// }

// fn ForInitializerPart(iter: impl lexer::TokenStream + Clone) -> ParseResult<stmt::Statement> {
//     parse_any(&iter, &[
//         |iter| {
//             let expression = Expression(iter);
//             if expression.is_none() {
//                 return None;
//             }
//             expect_token(&iter, lexer::Token::Semicolon)?;
//             return Some(stmt::ForInit::Expression(expression));
//         },
//         |iter| {
//             let declaration = Declaration(iter);
//             if declaration.is_none() {
//                 return None;
//             }
//             return Some(stmt::ForInit::Declaration(Box::new(declaration.unwrap())));
//         }
//     ])
// }

// fn parse_iteration_statement(self) -> ParseResult<stmt::Statement> {
//     parse_any(&iter, &[
//         |iter| {
//             self.match_token(&iter, lexer::Token::While)?;
//             self.match_token(&iter, lexer::Token::LParenthesis)?;
//             let expression = parse_expression(iter);
//             if expression.is_none() {
//                 return None;
//             }
//             let token = iter.next();
//             self.match_token(&iter,lexer::Token::RParenthesis)?;
//             let statement = parse_statement(iter);
//             if statement.is_none() {
//                 return None;
//             }
//             return Ok(stmt::Statement::new_while(expression.unwrap(), statement.unwrap()));
//         },
//         |iter| {
//             self.match_token(&iter, lexer::Token::While)?;
//             self.match_token(&iter, lexer::Token::Do)?;
//             let statement = parse_statement(iter);
//             if statement.is_none() {
//                 return None;
//             }
//             self.match_token(&iter, lexer::Token::While)?;
//             self.match_token(&iter, lexer::Token::LParenthesis)?;
//             let expression = parse_expression(iter);
//             self.match_token(&iter, lexer::Token::RParenthesis)?;
//             self.match_token(&iter, lexer::Token::Semicolon)?;
//             return Ok(stmt::Statement::new_do_while(statement.unwrap(), expression.unwrap()));
//         },
//         |iter| {
//             self.match_token(&iter, lexer::Token::For)?;
//             self.match_token(&iter, lexer::Token::LParenthesis)?;
//             let initializer = ForInitializerPart(iter); 
//             let predicate = parse_expression(iter);
//             self.match_token(&iter, lexer::Token::Semicolon)?;
//             let step = parse_expression(iter)?; 
//             self.match_token(&iter, lexer::Token::RParenthesis)?;
//             let body = parse_statement(iter);
//             return Ok(stmt::Statement::new_for(initializer, predicate, step, body));
//         }
//     ])
// }

// fn parse_jump_statement(parser: impl Parser<_>) -> ParseResult<stmt::Statement> {
//     parser.parse_any(&[
//         |iter| {
//             parser.next();
//             parser.match_token(lexer::Token::Goto)?;
//             parser.next();
//             let identifier = parser.parse_identifier()?;
//             parser.next();
//             parser.match_token(lexer::Token::Semicolon)?;
//             return Ok(stmt::Statement::new_goto(identifier));
//         },
//         |iter| {
//             parser.next();
//             parser.match_token(lexer::Token::Continue)?;
//             parser.next();
//             parser.match_token(lexer::Token::Semicolon)?;
//             return Ok(stmt::Statement::new_continue());
//         },
//         |iter| {
//             parser.next();
//             parser.match_token(lexer::Token::Break)?;
//             parser.next();
//             parser.match_token(lexer::Token::Semicolon)?;
//             return Ok(stmt::Statement::new_break());
//         },
//         |iter| {
//             parser.next();
//             parser.match_token(lexer::Token::Return)?;
//             parser.next();
//             let expression = parser.optional_parse(Self::parse_expression)?; 
//             if expression.is_some() {
//                 self.next();
//             }
//             parser.match_token(lexer::Token::Semicolon)?;
//             return Ok(stmt::Statement::new_return(expression));
//         }
//     ])
// }

