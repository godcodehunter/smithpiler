pub mod r#type;
pub mod expr;
pub mod stmt;
pub mod decl;
pub mod span;
pub mod position;
mod pretty_print;

pub struct Ast(pub Vec<ExternalDeclaration>);

pub enum ExternalDeclaration {
    FunctionDefinition(Box<FunctionDefinition>),
    Declaration(Box<decl::Declaration>),
}

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


