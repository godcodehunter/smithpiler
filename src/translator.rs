extern crate llvm_sys;
use llvm_sys::{core::*, prelude::LLVMContextRef};
extern crate libc;
use crate::{expression::*, r#type, type_cast::*};
use crate::statement::*;
use std::{default::Default, collections::hash_map::RandomState};
use std::ffi::CString;
use crate::r#type::*;
use std::os::unix::ffi::OsStrExt;
use std::mem::MaybeUninit;
use lang_c::{ast::*, span::Node, driver};
use crate::utils::*;
use std::convert::TryInto;
use std::collections::HashMap;

use crate::cp_expr_ex::*;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term;
use crate::diagnostics::*;


/// Represent any value gotten after translation 
#[derive(Debug, Clone)]
pub struct TranslatedValue {
    pub value: llvm::prelude::LLVMValueRef,
    pub lang_type: crate::r#type::Type,
}

/// Base trait for all translators.
pub trait BaseTranslator<'ast> {
    fn builder(&self) -> llvm::prelude::LLVMBuilderRef;
    fn context(&self) -> LLVMContextRef;
    fn add_diagnostic(&mut self, diagnostic: Diagnostic);
    /// Returns the translated value at the current translation position if variable not exist return None
    fn resolve_symbol(&self, identifier: &Identifier) -> TranslatedValue;
    /// Update variable and return true otherwise if variable not exist return false
    fn update_symbol(&mut self, identifier: &'ast Identifier, value: TranslatedValue);
    fn resolve_typename(&self, identifier: &Identifier) -> Type;
}

//TODO: temporary placeholder 
pub const NOP_STUB: *const libc::c_char = b"\0".as_ptr() as _;

pub struct Translator<'ast> {
    pub context: llvm::prelude::LLVMContextRef,
    pub module: llvm::prelude::LLVMModuleRef,
    builder: llvm::prelude::LLVMBuilderRef,
    file: MaybeUninit<SimpleFile<String, &'ast String>>,
    pub diagnostics: std::collections::LinkedList<Diagnostic>,
    variables: HashMap<&'ast Identifier, TranslatedValue>,
    types: HashMap<&'ast Identifier, Type>,
    cte: CTEnv,
}

impl<'ast> BaseTranslator<'ast> for Translator<'ast>  {
    fn builder(&self) -> llvm::prelude::LLVMBuilderRef {
        self.builder
    }

    fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push_back(diagnostic)
    }

    fn resolve_symbol(&self, identifier: &Identifier) -> TranslatedValue {
        self.variables.get(identifier).unwrap().clone()
    }

    fn resolve_typename(&self, identifier: &Identifier) -> Type {
        self.types.get(identifier).unwrap().clone()
    }

    fn update_symbol(&mut self, identifier: &'ast Identifier, value: TranslatedValue) {
        self.variables.insert(identifier, value);
    }

    fn context(&self) -> LLVMContextRef {
        self.context
    }
}

impl<'ast> Translator<'ast> {
    pub fn new() -> Self {
        Self {
            context: std::ptr::null_mut(),
            module: std::ptr::null_mut(),
            builder: std::ptr::null_mut(),
            file: MaybeUninit::uninit(),
            diagnostics: Default::default(),
            variables: Default::default(),
            types: Default::default(),
            cte: CTEnv::new().unwrap(),
        }
    }
    
    pub fn translate_declaration(&mut self, declaration: &'ast Node<Declaration>) {
        let mut holder = TypeHolder::new();
        for item in &declaration.node.specifiers {
            if let DeclarationSpecifier::TypeSpecifier(ref spec) = item.node {
                holder.process_specifier(spec);
            }
        }
        let ty: Type = holder.try_into().unwrap();
        for declarator in &declaration.node.declarators {
            let ident: &Node<Identifier>;
            if let DeclaratorKind::Identifier(name ) = &declarator.node.declarator.node.kind.node {
                ident = name;
            } else {
                panic!()
            }

            if declarator.node.declarator.node.derived.len() == 1 {
                match &declarator.node.declarator.node.derived[0].node {
                    DerivedDeclarator::Function(f) => {
                        let mut params = Vec::<Type>::new();

                        let mut variadic = false;
                        let mut map = multimap::MultiMap::<&lang_c::ast::Identifier, &Node<lang_c::ast::Identifier>, RandomState>::new();
                        
                        for param_decl in &f.node.parameters {
                            let translated = self.translate_function_parameter_declaration(param_decl);
                            map.insert(&translated.0.node, translated.0);
                            params.push(translated.1);
                        }
                    
                        let mut duplicates = Vec::new();
                        for pair in &map {
                            if pair.1.len() > 1 {
                                duplicates.push(pair.1);
                            }
                        }
        
                        for duplicate in duplicates {
                            let diagnostic = arguments_with_same_name(duplicate.iter().map(|i|*i));
                            self.diagnostics.push_back(diagnostic);
                        }
        
                        if Ellipsis::Some == f.node.ellipsis {
                            variadic = true;
                        }
                        unsafe {
                            let f_t = Type::new_function(ty, params, variadic);
                            let t_t = f_t.translate(self);
                            let function = LLVMAddFunction(
                                self.module, 
                                CString::new(ident.node.name.clone().into_bytes()).unwrap().as_ptr(), 
                                t_t
                            );
                            self.update_symbol(&ident.node, TranslatedValue{
                                value: function,
                                lang_type: f_t,
                            });
                        }
                        return;
                    }
                    _ => panic!()
                }
            }
    
            if let Some(initializer) = &declarator.node.initializer {
                match &initializer.node {
                    Initializer::Expression(expr) => {
                        let value = Self::translate_expression(self, &*expr);
                        self.update_symbol(&ident.node, value)
                    }
                    Initializer::List(list) => {
                        panic!()
                    }
                }
            }
        }
    }

    // TODO: now translate only in from 'type ident'
    fn translate_function_arg_declaration<'a>(&mut self, declaration: &'a Node<Declaration>) -> (&'a Node<Identifier>, llvm::prelude::LLVMTypeRef) {
        let mut holder = TypeHolder::new();
        for item in &declaration.node.specifiers {
            if let DeclarationSpecifier::TypeSpecifier(ref spec) = item.node {
                holder.process_specifier(spec);
            }
        }
        let ty: Type = holder.try_into().unwrap();
        let declarator = &declaration.node.declarators[0].node.declarator.node;
        match &declarator.kind.node {
            DeclaratorKind::Identifier(ident) => {
                return (&ident, ty.translate(self));
            }
            _ => todo!(),
        }
    }

    fn translate_function_parameter_declaration<'a>(&mut self, declaration: &'a Node<ParameterDeclaration>) -> (&'a Node<Identifier>, Type) {
        let mut holder = TypeHolder::new();
        for item in &declaration.node.specifiers {
            if let DeclarationSpecifier::TypeSpecifier(ref spec) = item.node {
                holder.process_specifier(spec);
            }
        }
        let mut ty: Type = holder.try_into().unwrap();
        let declarator = declaration.node.declarator.as_ref().unwrap();
        match &declarator.node.kind.node {
            DeclaratorKind::Identifier(ident) => {
                for der in &declarator.node.derived {
                    match &der.node {
                        DerivedDeclarator::Pointer(ptr) => {
                            ty = Type::Derived(DerivedType::Pointer(PointerType(ty.into())));
                        }
                        // DerivedDeclarator::Array(_) => {}
                        // DerivedDeclarator::Function(_) => {}
                        // DerivedDeclarator::KRFunction(_) => {}
                        _ => todo!(),
                    }
                }
                return (ident, ty);
            }
            _ => todo!()
        }
    }

    fn translate_function(&mut self, function_definition: &'ast Node<FunctionDefinition>) {
        let mut variadic = false;
        let identifier: &Identifier;
        let mut params = Vec::<(&lang_c::ast::Identifier, Type)>::new();
        
        &function_definition.node.specifiers;
        let ret_type = Type::new_signed_int();

        if let DeclaratorKind::Identifier(ident) = &function_definition.node.declarator.node.kind.node {
            unsafe {
                identifier = &ident.node;
            }
        } else {
            // NOTE: Parser guarantees the presence of an identifier 
            // if function successfully parsed  
            unreachable!()
        }
        
        match &function_definition.node.declarator.node.derived[0].node {
            DerivedDeclarator::Function(declarator) => {
                use multimap::MultiMap;
                use std::collections::hash_map::RandomState;

                let mut map = MultiMap::<&lang_c::ast::Identifier, &Node<lang_c::ast::Identifier>, RandomState>::new();

                for param_decl in &declarator.node.parameters {
                    let translated = self.translate_function_parameter_declaration(param_decl);
                    map.insert(&translated.0.node, translated.0);
                    params.push((&translated.0.node, translated.1));
                }
               
                let mut duplicates = Vec::new();
                for pair in &map {
                    if pair.1.len() > 1 {
                        duplicates.push(pair.1);
                    }
                }

                for duplicate in duplicates {
                    let diagnostic = arguments_with_same_name(duplicate.iter().map(|i|*i));
                    self.diagnostics.push_back(diagnostic);
                }

                if Ellipsis::Some == declarator.node.ellipsis {
                    variadic = true;
                }
            }
            DerivedDeclarator::KRFunction(declarator) => {
                let diagnostic = old_syntax_declaration(function_definition);
                self.diagnostics.push_back(diagnostic);

                use multimap::MultiMap;
                use std::iter::FromIterator;
                use std::collections::hash_map::RandomState;
                
                let iter = declarator.iter()
                    .map(|item| (&item.node, item));
                let mut map = MultiMap::<&lang_c::ast::Identifier, &Node<lang_c::ast::Identifier>, RandomState>::from_iter(iter);
                
                let mut duplicates = Vec::new();
                for pair in &map {
                    if pair.1.len() > 1 {
                        duplicates.push(pair.1);
                    }
                }

                for duplicate in duplicates {
                    let diagnostic = arguments_with_same_name(duplicate.iter().map(|i|*i));
                    self.diagnostics.push_back(diagnostic);
                }
                
                let mut not_listened = Vec::new();
                for declaration in &function_definition.node.declarations {
                    let translated = self.translate_function_arg_declaration(declaration);
                    if map.remove(&translated.0.node).is_none() {
                        not_listened.push(translated.0);
                    }
                }
                let diagnostic = not_listed_parameter_declaration(&function_definition.node.declarator.node.derived[0], not_listened);
                self.diagnostics.push_back(diagnostic);

                if !map.is_empty() {
                    let iter = map.into_iter()
                        .map(|item| item.1[0]);
                    let diagnostic = parameter_without_declaration(iter);
                    self.diagnostics.push_back(diagnostic);
                }
            }
            // NOTE: Parser guarantees that enum DerivedDeclarator for function 
            // have correct variant
            _ => unreachable!()
        }

        let mut p: Vec<Type> = params.iter().map(|i| i.1.clone()).collect();
        unsafe {
            let f_t = Type::new_function(ret_type.clone(), p, variadic);
            let function = LLVMAddFunction(
                self.module, 
                CString::new(identifier.name.clone().into_bytes()).unwrap().as_ptr(), 
                f_t.translate(self)
            );
        
            let block = LLVMAppendBasicBlockInContext(
                self.context,
                function, 
                b"func.body\0".as_ptr() as _,
            );

            self.update_symbol(identifier, TranslatedValue{value: function, lang_type: f_t.clone()});
            for i in params.into_iter().enumerate() {
                let p = LLVMGetParam(function, i.0 as _);
                self.update_symbol(i.1.0, TranslatedValue{value: p, lang_type: i.1.1});
            }

            LLVMPositionBuilderAtEnd(self.builder(), block);
            if let Statement::Compound(compound) = &function_definition.node.statement.node {
                self.translate_compound_statement(compound, ret_type); 
            } else {
                // NOTE: Parser guarantees that statement is compound statement
                unreachable!()
            }
        }
    }

    pub fn check_static_assert(&mut self, node: &'ast Node<StaticAssert>) {
        unsafe {
            let unchecked= self as *mut Translator;
            let v = (*unchecked).cte.execute(unchecked.as_mut().unwrap(), &*node.node.expression);
            if v == 0 {
                let diagnostic = static_assert(node);
                self.diagnostics.push_back(diagnostic);
            }
        }
    }

    /// Translate one translation unit to llvm 
    pub fn translate(&mut self, parse_result: &'ast driver::Parse, path: String) -> llvm::prelude::LLVMModuleRef {
        unsafe {
            self.file.as_mut_ptr().write(SimpleFile::new(path.clone(), &parse_result.source));
            self.context = LLVMContextCreate();
            self.module = LLVMModuleCreateWithNameInContext(path.as_bytes().as_ptr() as _, self.context);
            self.builder = LLVMCreateBuilderInContext(self.context);
        }

        for decl in &parse_result.unit.0 {
            match &decl.node {
                ExternalDeclaration::FunctionDefinition(func) => {
                    self.translate_function(func);
                }
                ExternalDeclaration::Declaration(decl) => {
                    self.translate_declaration(decl);
                }
                ExternalDeclaration::StaticAssert(assert) => { 
                    self.check_static_assert(assert)
                }
            }
        }
        unsafe { LLVMDumpModule(self.module); }
        self.module
    }

    pub fn emit_diagnostics(&self) {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        for diagnostic in &self.diagnostics {
            unsafe {
                term::emit(&mut writer.lock(), &config, self.file.assume_init_ref(), diagnostic).unwrap();
            }
        }
    }

    pub fn write_to_file(&self, path: std::path::PathBuf) {
        unsafe {
            llvm::bit_writer::LLVMWriteBitcodeToFile(self.module, path.as_os_str().as_bytes().as_ptr() as _);
        }
    }
}

impl<'ast> Drop for Translator<'ast> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}
