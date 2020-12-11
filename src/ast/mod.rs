pub mod r#type;
pub mod expr;
pub mod stmt;
pub mod decl;
pub mod span;
pub mod position;

use std::path::Path;

pub struct Ast(pub Vec<decl::Decl>);

pub struct TranslationUnit {
    pub decls: Ast,
    //TODO: pub path: String,
}

pub struct CompilationBundle {

}

