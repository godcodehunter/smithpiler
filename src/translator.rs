extern crate llvm_sys;
use llvm_sys::core::*;
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
    fn add_diagnostic(&self, diagnostic: Diagnostic) {
        todo!()
    }
    /// Returns the translated value at the current translation position if variable not exist return None
    fn resolve_variable(&self, identifier: &Identifier) -> TranslatedValue;
    /// Update variable and return true otherwise if variable not exist return false
    fn update_variable(&mut self, identifier: &'ast Identifier, value: TranslatedValue);
    fn create_variable(&mut self, identifier: &'ast Identifier, ty: &r#type::Type);
}

//TODO: temporary placeholder 
pub const NOP_STUB: *const libc::c_char = b"nop\0".as_ptr() as _;

pub struct Translator<'ast> {
    context: llvm::prelude::LLVMContextRef,
    module: llvm::prelude::LLVMModuleRef,
    builder: llvm::prelude::LLVMBuilderRef,
    file: MaybeUninit<SimpleFile<String, &'ast String>>,
    diagnostics: std::collections::LinkedList<Diagnostic>,
    variables: HashMap<&'ast Identifier, TranslatedValue>,
}

impl<'ast> BaseTranslator<'ast> for Translator<'ast>  {
    fn builder(&self) -> llvm::prelude::LLVMBuilderRef {
        self.builder
    }

    fn resolve_variable(&self, identifier: &Identifier) -> TranslatedValue {
        self.variables.get(identifier).unwrap().clone()
    }

    fn update_variable(&mut self, identifier: &'ast Identifier, value: TranslatedValue) {
        self.variables.insert(identifier, value);
    }

    fn create_variable(&mut self, identifier: &'ast Identifier, ty: &Type) {
        todo!()
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
                            self.update_variable(&ident.node, TranslatedValue{
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
                        let value = self.translate_expression(&*expr);
                        self.update_variable(&ident.node, value)
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
        let ty: Type = holder.try_into().unwrap();
        let declarator = declaration.node.declarator.as_ref().unwrap();
        match &declarator.node.kind.node {
            DeclaratorKind::Identifier(ident) => {
                return (ident, ty);
            }
            _ => todo!(),
        }
    }

    fn translate_function(&mut self, function_definition: &'ast Node<FunctionDefinition>) {
        let mut variadic = false;
        let identifier: &Identifier;
        let mut params = Vec::<(&lang_c::ast::Identifier, Type)>::new();
        let ret_type: llvm::prelude::LLVMTypeRef;
        
        &function_definition.node.specifiers;
        let ret_type = Type::new_signed_int().translate(self);

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

        let mut p = params.iter().map(|i| i.1.translate(self) ).collect::<Vec<llvm::prelude::LLVMTypeRef>>();
        unsafe {
            let func_type = LLVMFunctionType(
                ret_type,
                p.as_mut_ptr(),
                params.len() as _,
                variadic as _,
            );
            let function = LLVMAddFunction(
                self.module, 
                CString::new(identifier.name.clone().into_bytes()).unwrap().as_ptr(), 
                func_type
            );
        
            let block = LLVMAppendBasicBlockInContext(
                self.context, 
                function, 
                b"body\0".as_ptr() as _,
            );

            self.update_variable(identifier, TranslatedValue{value: function, lang_type: Type::new_signed_int()});
            for i in params.into_iter().enumerate() {
                let p = LLVMGetParam(function, i.0 as _);
                self.update_variable(i.1.0, TranslatedValue{value: p, lang_type: Type::new_signed_int()});
            }

            LLVMPositionBuilderAtEnd(self.builder(), block);
            self.translate_statement(&function_definition.node.statement, Type::new_signed_int()); 
        }
    }

    /// C 6.7.10.3 Expression should be integer constant expression
    fn execute_constant_expression(&mut self, assert: &Node<Expression>) -> u64 {
        unsafe {
            use llvm_sys::execution_engine::*;
            let module = LLVMModuleCreateWithName(NOP_STUB);
            let engine: *mut LLVMExecutionEngineRef = std::ptr::null_mut();
            let error: *mut *mut i8 = std::ptr::null_mut();
            //TODO: check error 
            LLVMCreateExecutionEngineForModule(engine, module, error);
            let func_type = LLVMFunctionType(
                LLVMInt8Type(),
                [].as_mut_ptr(),
                0,
                false as _,
            );
            let function = LLVMAddFunction(
                self.module, 
                b"constant_expression\0".as_ptr() as _, 
                func_type
            );
        
            let block = LLVMAppendBasicBlockInContext(
                self.context, 
                function, 
                NOP_STUB,
            );
            let builder = LLVMCreateBuilderInContext(self.context);
            LLVMPositionBuilderAtEnd(builder, block);
            //TODO: ... 
            let value = LLVMRunFunction(*engine, function, 0, [].as_mut_ptr());
            LLVMGenericValueToInt(value, true as _)
        }
    }

    /// Translate one translation unit to llvm 
    pub fn translate(&mut self, parse_result: &'ast driver::Parse, path: String) -> llvm::prelude::LLVMModuleRef {
        unsafe {
            self.file.as_mut_ptr().write(SimpleFile::new(path.clone(), &parse_result.source));
            self.context = LLVMContextCreate();
            self.module = LLVMModuleCreateWithName(path.as_bytes().as_ptr() as _);
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
                    self.execute_constant_expression(&assert.node.expression);
                    let diagnostic = static_assert(assert);
                    self.diagnostics.push_back(diagnostic);
                }
            }
        }
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
