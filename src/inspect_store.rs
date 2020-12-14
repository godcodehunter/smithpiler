use std::{collections::HashMap, default::Default};
use crate::ast;

/// It is object of pure abstraction, which the through reference to AST  
/// presents a group of methods and memoization fields an analysis of it.
/// That concept is similar to std::Path, when we provide 
/// functionality by the wrapper over borrowing value of another type. 
pub struct InspectStore<'ast> {
    
    corresponding_type: HashMap<&'ast ast::decl::VarDecl, ast::r#type::Type>,
}

impl<'ast> InspectStore<'ast> {
    pub fn new() -> Self {
        Self{corresponding_type: Default::default()}
    }
    pub fn get_type_size(&self, r#type: &'ast ast::r#type::Type) -> u32 {
        unimplemented!()
    }
    pub fn get_corresponding_type(&self, decl: &ast::decl::VarDecl) -> &ast::r#type::Type {
        // self.corresponding_type[decl]
        unimplemented!()
    }

    pub fn get_fundefpar_type(&self, decl: &ast::decl::FuncDefParam) -> &ast::r#type::Type {
        unimplemented!()
    }
    // Context sensitive expressions (break, continue)
    // pub fn get_cfg_target() -> {
    // 
    // }
}

