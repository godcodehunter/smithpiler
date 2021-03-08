pub mod r#type;
pub mod expr;
pub mod stmt;
pub mod decl;
pub mod span;
pub mod position;
mod pretty_print;

pub struct TranslationUnit<'source> {
    pub external_declarations: Vec<ExternalDeclaration>,
    pub source: &'source std::path::Path, 
}

impl std::fmt::Display for TranslationUnit<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Debug)]
pub enum ExternalDeclaration {
    FunctionDefinition(Box<FunctionDefinition>),
    Declaration(Box<decl::Declaration>),
}

#[derive(Debug)]
pub struct FunctionDefinition {
    specifiers: Vec<decl::DeclarationSpecifier>,
    declarator: decl::Declarator,
    declaration: Option<Vec<decl::Declaration>>,
    compound: stmt::CompoundStmt,
}

impl FunctionDefinition {
    pub fn new(specifiers: Vec<decl::DeclarationSpecifier>, declarator: decl::Declarator, declaration: Option<Vec<decl::Declaration>>, compound: stmt::CompoundStmt) -> FunctionDefinition {
        Self{specifiers, declarator, declaration, compound}
    }
}


