pub mod r#type;
pub mod expr;
pub mod stmt;
pub mod decl;
pub mod span;
pub mod position;

pub struct Ast(pub Vec<ExternalDeclaration>);

pub enum ExternalDeclaration {
    FunctionDefinition(Box<FunctionDefinition>),
    Declaration(Box<Declaration>),
}

pub struct FunctionDefinition {
    specifiers: DeclarationSpecifiers,
    declarator: Declarator,
    declaration: Option<Vec<Declaration>>,
    compound: CompoundStatement,
}
