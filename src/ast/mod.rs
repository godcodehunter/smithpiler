pub mod r#type;
pub mod expr;
pub mod stmt;
pub mod decl;
pub mod span;
pub mod position;

pub struct Ast(pub Vec<ExternalDeclaration>);

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

pub enum ExternalDeclaration {
    FunctionDefinition(Box<FunctionDefinition>),
    Declaration(Box<decl::Declaration>),
}

pub enum DeclarationSpecifiers {

}

pub struct FunctionDefinition {
    specifiers: DeclarationSpecifiers,
    declarator: decl::Declarator,
    declaration: Option<Vec<decl::Declaration>>,
    compound: stmt::CompoundStmt,
}

impl FunctionDefinition {
    pub fn new(specifiers: DeclarationSpecifiers, declarator: decl::Declarator, declaration: Option<Vec<decl::Declaration>>, compound: stmt::CompoundStmt) -> FunctionDefinition {
        Self{specifiers, declarator, declaration, compound}
    }
}
