extern crate llvm_sys as llvm;
extern crate libc;
use crate::{
    // inspect_store::InspectStore, 
    r#type,
    // expression_translator::ExpressionTranslator
};
use std::collections::HashMap;
use std::default::Default;
use std::ffi::CString;
use std::os::unix::ffi::OsStrExt;
use std::mem::MaybeUninit;
use lang_c::{ast::*, span::Node, driver};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term;

/// Base trait for all translators.
pub trait BaseTranslator {
    fn builder(&self) -> llvm::prelude::LLVMBuilderRef;
    fn context(&self) -> llvm::prelude::LLVMContextRef;
    // fn inspect_store(&self) -> &InspectStore;
    fn translator_store(&self) -> &TranslatorStore;
}

/// Force(don't care about cast legality) generate any of possible type conversion in language(union of explicit and implicit casts).  
/// **NOTE**: For equivalent types doing nothing.
///
/// # Arguments
/// * `translator` - translator that perform generation
/// * `value` - convertible value
/// * `associate` - values type
/// * `target` - target type to which the conversion will be performed
pub fn translate_type_cast<T: BaseTranslator>(translator: &T, value: llvm::prelude::LLVMValueRef, associate: &r#type::Type, target: &r#type::Type) -> llvm::prelude::LLVMValueRef {
    // use crate::ast::r#type::Fundamental;

    // if associate == target {
    //     return value;
    // }

    // fn widening_cast(associate: &ast::r#type::Fundamental, target: &ast::r#type::Fundamental) -> InstBuildFunc {
    //     match (associate, target) {
    //         (Fundamental::SignedInteger(_), Fundamental::SignedInteger(_)) => {
    //             return llvm::core::LLVMBuildSExt;
    //         },
    //         (Fundamental::SignedInteger(_), Fundamental::UnsignedInteger(_)) => {
    //             info!(target: "codegen", "discovered unsafe conversion");
    //             return llvm::core::LLVMBuildZExt; 
    //         },
    //         (Fundamental::SignedInteger(_), Fundamental::Floating(_)) => {
    //             info!(target: "codegen", "discovered unsafe conversion");
    //             return llvm::core::LLVMBuildSIToFP;
    //         },
    //         (Fundamental::UnsignedInteger(_), Fundamental::UnsignedInteger(_)) => {
    //             return llvm::core::LLVMBuildZExt;
    //         },
    //         (Fundamental::UnsignedInteger(_), Fundamental::SignedInteger(_)) => {
    //             return llvm::core::LLVMBuildZExt;
    //         },
    //         (Fundamental::UnsignedInteger(_), Fundamental::Floating(_)) => {
    //             info!(target: "codegen", "discovered unsafe conversion");
    //             return llvm::core::LLVMBuildUIToFP;
    //         },
    //         (Fundamental::Floating(_), Fundamental::Floating(_)) => {
    //             return llvm::core::LLVMBuildFPExt;
    //         },
    //         (Fundamental::Floating(_), Fundamental::SignedInteger(_)) => {
    //             info!(target: "codegen", "discovered unsafe conversion");
    //             return llvm::core::LLVMBuildFPToSI;
    //         },
    //         (Fundamental::Floating(_), Fundamental::UnsignedInteger(_)) => {
    //             info!(target: "codegen", "discovered unsafe conversion");
    //             return llvm::core::LLVMBuildFPToUI;
    //         },
    //         (Fundamental::Bool, _) => {
    //             return llvm::core::LLVMBuildZExt;
    //         },
    //         _ => unreachable!()
    //     }
    // }

    // fn narrowing_cast(associate: &ast::r#type::Fundamental, target: &ast::r#type::Fundamental) -> InstBuildFunc {
    //     info!(target: "codegen", "discovered narrowing conversion");
    //     match (associate, target) {
    //         (_, Fundamental::Bool) => {
    //             todo!()
    //         },
    //         (Fundamental::SignedInteger(_), Fundamental::SignedInteger(_)) => {
    //             return llvm::core::LLVMBuildTrunc;
    //         },
    //         (Fundamental::SignedInteger(_), Fundamental::UnsignedInteger(_)) => {
    //             return llvm::core::LLVMBuildTrunc;
    //         },
    //         (Fundamental::SignedInteger(_), Fundamental::Floating(_)) => {
    //             return llvm::core::LLVMBuildSIToFP;
    //         },
    //         (Fundamental::UnsignedInteger(_), Fundamental::UnsignedInteger(_)) => { 
    //             return llvm::core::LLVMBuildTrunc;
    //         },
    //         (Fundamental::UnsignedInteger(_), Fundamental::SignedInteger(_)) => {
    //             return llvm::core::LLVMBuildTrunc;
    //         },
    //         (Fundamental::UnsignedInteger(_), Fundamental::Floating(_)) => {
    //             return llvm::core::LLVMBuildUIToFP;
    //         },
    //         (Fundamental::Floating(_), Fundamental::Floating(_)) => {
    //             return llvm::core::LLVMBuildFPTrunc;
    //         },
    //         (Fundamental::Floating(_), Fundamental::SignedInteger(_)) => {
    //             return llvm::core::LLVMBuildFPToSI;
    //         },
    //         (Fundamental::Floating(_), Fundamental::UnsignedInteger(_)) => {
    //             return llvm::core::LLVMBuildFPToUI;
    //         },
    //         _ => unreachable!()
    //     }
    // }

    // let is_narrowing = translator.inspect_store().get_type_size(associate) > translator.inspect_store().get_type_size(target);

    // let conversion = match (associate, target) {
    //     (Type::Fundamental(lhs), Type::Fundamental(rhs)) if !is_narrowing => widening_cast(lhs, rhs),
    //     (Type::Fundamental(lhs), Type::Fundamental(rhs)) if is_narrowing => narrowing_cast(lhs, rhs),
    //     _ => {
    //         info!(target: "codegen", "discovered unsafe conversion");
    //         llvm::core::LLVMBuildBitCast
    //     }
    // };

    // unsafe {
    //     conversion(translator.builder(), value, translator.translator_store().get_generated(target), NOP_STUB)
    // }
    todo!()
}

//TODO: temporary placeholder 
pub const NOP_STUB: *const libc::c_char = b"nop\0".as_ptr() as _;

pub struct TranslatorStore {
    // generated_types: HashMap<&'ast ast::r#type::Type, llvm::prelude::LLVMTypeRef>,
    // generate_values: HashMap<&'ast ast::decl::VarDecl, llvm::prelude::LLVMValueRef>,
    //TODO: функции могут быть в двух состояниях протранслированные, заготовки 
    // generated_functions: HashMap<&'ast ast::decl::FuncDef, llvm::prelude::LLVMTypeRef>,  
}

impl<'ast> TranslatorStore {
    fn new() -> Self {
        Self {
            // generated_types: Default::default(),
            // generate_values: Default::default(), 
            // generated_functions: Default::default(),
        }
    }

    fn get_generated(&self, ty: &crate::r#type::Type) -> llvm::prelude::LLVMTypeRef {
        // self.generated_types[ty]
        unimplemented!()
    }

    // fn resolve(&self, var: ast::decl::VarDecl) -> llvm::prelude::LLVMTypeRef {
    //     // self.generate_values[var]
    //     unimplemented!()
    // }

    fn get_function() -> llvm::prelude::LLVMValueRef {
        unimplemented!()
    }
}

pub struct Translator {
    context: llvm::prelude::LLVMContextRef,
    module: llvm::prelude::LLVMModuleRef,
    builder: llvm::prelude::LLVMBuilderRef,
    file: MaybeUninit<SimpleFile<String, String>>,
    diagnostics: std::collections::LinkedList<Diagnostic<()>>,
}

type InstBuildFunc = unsafe extern "C" fn(llvm::prelude::LLVMBuilderRef, llvm::prelude::LLVMValueRef, llvm::prelude::LLVMTypeRef, *const libc::c_char) -> llvm::prelude::LLVMValueRef;

impl<'ast> BaseTranslator for Translator  {
    fn builder(&self) -> llvm::prelude::LLVMBuilderRef {
        self.builder
    }

    fn context(&self) -> llvm::prelude::LLVMContextRef {
        self.context
    }

    // fn inspect_store(&self) -> &InspectStore {
    //     &self.inspect_store
    // }

    fn translator_store(&self) -> &TranslatorStore {
        // &self.translator_store
        todo!()
    }
}

impl<'ast> Translator {
    pub fn new() -> Self {
        unsafe {
            Self {
                context: std::ptr::null_mut(),
                module: std::ptr::null_mut(),
                builder: std::ptr::null_mut(),
                file: MaybeUninit::uninit(),
                diagnostics: Default::default(),
                // build_query: Default::default(), 
                // inspect_store,
                // translator_store: TranslatorStore::new(),
            }
        }
    }
 
    fn translate_type(&mut self, r#type: &r#type::Type) -> llvm::prelude::LLVMTypeRef {
        let translate_fundamental = |r#type: &r#type::Fundamental| -> llvm::prelude::LLVMTypeRef {
            use r#type::Fundamental::*;
            match r#type {
                SignedInteger(r#type) => {
                    unsafe {
                        match r#type {
                            r#type::SignedIntegerType::SignedChar => {
                                llvm::core::LLVMInt8TypeInContext(self.context())
                            },
                            r#type::SignedIntegerType::ShortInt => {
                                llvm::core::LLVMInt16TypeInContext(self.context())
                            },
                            r#type::SignedIntegerType::Int => {
                                llvm::core::LLVMInt32TypeInContext(self.context())
                            },
                            r#type::SignedIntegerType::LongInt => {
                                llvm::core::LLVMInt64TypeInContext(self.context())
                            },
                            r#type::SignedIntegerType::LongLongInt => {
                                llvm::core::LLVMInt128TypeInContext(self.context())
                            },
                        }
                    }
                },
                UnsignedInteger(r#type) => {
                    unsafe {
                        match r#type {
                            r#type::UnsignedIntegerType::UnsignedChar => {
                                llvm::core::LLVMInt8TypeInContext(self.context())
                            },
                            r#type::UnsignedIntegerType::UnsignedShort => {
                                llvm::core::LLVMInt16TypeInContext(self.context())
                            },
                            r#type::UnsignedIntegerType::UnsignedInt => {
                                llvm::core::LLVMInt32TypeInContext(self.context())
                            },
                            r#type::UnsignedIntegerType::UnsignedLong => {
                                llvm::core::LLVMInt64TypeInContext(self.context())
                            },
                            r#type::UnsignedIntegerType::UnsignedLongLong => {
                                llvm::core::LLVMInt128TypeInContext(self.context())
                            }   
                        }
                    }
                },
                Floating(r#type) => {
                    unsafe {
                        match r#type {
                            Float => {
                                llvm::core::LLVMFloatTypeInContext(self.context())
                            },
                            Double => {
                                llvm::core::LLVMDoubleTypeInContext(self.context())
                            },
                            LongDouble => {
                                llvm::core::LLVMX86FP80TypeInContext(self.context())
                            }
                        }
                    }
                },
                Void => {
                    unsafe { llvm::core::LLVMVoidTypeInContext(self.context()) }
                },
                Bool => {
                    unsafe { llvm::core::LLVMInt1TypeInContext(self.context()) }
                },
            }
        };

        // let translate_derived = |ty: &ast::r#type::DerivedType| -> llvm::prelude::LLVMTypeRef { 
            // unimplemented!()
            // use ast::r#type::DerivedType::*;
            // match ty {
            //     Array(ty) => {
            //         let elem_type = self.get_generated(ty.elem_type.as_ref());
            //         unsafe { llvm::core::LLVMArrayType(elem_type, ty.size) }
            //     },
            //     Structure(ty) => {
            //         let fields: Vec<_> = ty.fields.into_iter().map(|field| self.get_generated(field) ).collect();
            //         unsafe { llvm::core::LLVMStructType(fields.as_mut_ptr(), fields.len() as u32, false as llvm_sys::prelude::LLVMBool) }
            //     },
            //     Union(ty) => {
            //         let bitsize = self.get_type_size(ty); 
            //         unsafe { llvm::core::LLVMIntTypeInContext(self.context, bitsize) }
            //     },
            //     Function(ty) => {
            //         let ret_type = self.get_generated(ty.returned_type.as_ref());
            //         let args: Vec<_> = ty.params.into_iter().map(|arg| self.get_generated(arg) ).collect();
            //         unsafe { llvm::core::LLVMFunctionType(ret_type, args.as_mut_ptr(), args.len() as u32, false as llvm_sys::prelude::LLVMBool) }
            //     },
            //     Pointer(ty) => {
            //         let elem_type = self.get_generated(ty.referent_type.as_ref());
            //         unsafe { llvm::core::LLVMPointerType(elem_type, 0) }
            //     }
            // }
        // };

        // let translate_enumerated = | ty: &ast::r#type::EnumeratedType | -> llvm::prelude::LLVMTypeRef {
        //     let bitsize = self.inspect_store().get_type_size(&Type::Enumerated(ty.to_owned())); 
        //     unsafe { llvm::core::LLVMIntTypeInContext(self.context, bitsize) }
        // };

        match r#type {
            r#type::Type::Fundamental(fundamental) => translate_fundamental(fundamental),
            _ => todo!()
        //     Type::Enumerated(enumerated) => translate_enumerated(enumerated),
        //     Type::Derived(derived) => translate_derived(derived),
        }
    }

    fn construct_type_build_query(&mut self) {
        // DftPre::new().map(|depth, node|{

        // });

        // TODO: closure that passed into traverser
        

        //Represents queue of composite types that in build process.
        //When last type is builded it ...
        // let build_query: Vec<Vec<llvm::prelude::LLVMTypeRef>>;

        // let last = build_query.last().unwrap();
        // let composite = llvm::core::LLVMStructTypeInContext(
        //     self.context, 
        //     last.as_mut_ptr(), 
        //     last.len(), 
        //     false as _,
        // );
        // build_query.pop();
        // build_query.last().unwrap().push(composite);
    }
    
    // TODO: now translate only in from 'type ident'
    fn translate_function_arg_declaration<'a>(&mut self, declaration: &'a Node<Declaration>) -> (&'a Identifier, llvm::prelude::LLVMTypeRef) {
        let ty = Self::extract_type(&declaration.node.specifiers);
        let declarator = &declaration.node.declarators[0].node.declarator.node;
        match &declarator.kind.node {
            DeclaratorKind::Identifier(ident) => {
                return (&ident.node, self.translate_type(&ty));
            }
            _ => todo!(),
        }
    }

    fn translate_function_parameter_declaration(&mut self, declaration: &Node<ParameterDeclaration>) -> llvm::prelude::LLVMTypeRef {
        let ty = Self::extract_type(&declaration.node.specifiers);
        let declarator = declaration.node.declarator.as_ref().unwrap();
        match &declarator.node.kind.node {
            DeclaratorKind::Identifier(ident) => {
                return self.translate_type(&ty);
            }
            _ => todo!(),
        }
    }

    // As described in 6.7.2.2 extract C type in stream of specifiers 
    fn extract_type(declaration_specifier: &Vec<Node<DeclarationSpecifier>>) -> r#type::Type {
        r#type::Type::new_signed_int()
        // use multiset::HashMultiSet;

        // let mut multiset = HashMultiSet::<ast::decl::DeclarationSpecifier>::new();

        // let convert_map = [
        //     ([[(ast::decl::TypeSpecifier::Void, 1)]], r#type::Fundamental::Void)
        // ];
        // for rule in &convert_map {
        //     for pattern in &rule.0 {
        //         for requirement in pattern {
                    
        //         }
        //     }
        // }

        // let count = multiset.count_of(
            // &ast::decl::DeclarationSpecifier::TypeSpecifier(
                // ast::decl::TypeSpecifier::Void
            // )
        // );
        // if 1 == count {

        // }
        // char

        // signed char
        
        // unsigned char
        
        // short, signed short, short int, or signed short int
        
        // unsigned short, or unsigned short int
        
        // int, signed, or signed int
        
        // unsigned, or unsigned int
        
        // long, signed long, long int, or signed long int
        
        // unsigned long, or unsigned long int
        
        // long long, signed long long, long long int, or signed long long int
        
        // unsigned long long, or unsigned long long int
        
        // float
        
        // double
        
        // long double
        
        // _Bool
        
        // float _Complex
        
        // double _Complex
        
        // long double _Complex
        
        // atomic type specifier
        
        // struct or union specifier
        // enum specifier
        
        // typedef name
    }

    fn translate_function(&mut self, function_definition: &Node<FunctionDefinition>) {
        let mut variadic = false;
        let identifier: &String;
        let mut params = Vec::<llvm::prelude::LLVMTypeRef>::new();
        let ret_type: llvm::prelude::LLVMTypeRef;
        
        &function_definition.node.specifiers;
        let ret_type = self.translate_type(&r#type::Type::new_signed_int());

        if let DeclaratorKind::Identifier(ident) = &function_definition.node.declarator.node.kind.node {
            unsafe {
                identifier = &ident.node.name;
            }
        } else {
            // NOTE: Parser guarantees the presence of an identifier 
            // if function successfully parsed  
            unreachable!()
        }
        
        match &function_definition.node.declarator.node.derived[0].node {
            DerivedDeclarator::Function(declarator) => {
                for param_decl in &declarator.node.parameters {
                    let translated = self.translate_function_parameter_declaration(param_decl);
                    params.push(translated);
                }
                if Ellipsis::Some == declarator.node.ellipsis {
                    variadic = true;
                }
            }
            DerivedDeclarator::KRFunction(declarator) => {
                let span = function_definition.span;
                let diagnostic = Diagnostic::warning()
                    .with_message("using the old syntax is not recommended")
                    .with_labels(vec![Label::primary((),span.start..span.end)]);
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
                    let mut diagnostic = Diagnostic::error()
                        .with_message("found multiple argument with same name");
                        
                    let mut labels = Vec::new();
                    for item in duplicate {
                        let span = item.span;
                        labels.push(Label::primary((),span.start..span.end));
                    }
                    diagnostic = diagnostic.with_labels(labels);

                    self.diagnostics.push_back(diagnostic);
                }
            
                for declaration in &function_definition.node.declarations {
                    let translated = self.translate_function_arg_declaration(declaration);
                    if map.remove(translated.0).is_none() {
                        let declarator_span = declaration.span;
                        let span = function_definition.node.declarator.node.derived[0].span;
                        let diagnostic = Diagnostic::error()
                            .with_message("declaration for not listed parameter")
                            .with_labels(vec![
                                Label::primary((), declarator_span.start..declarator_span.end),
                                Label::secondary((), span.start..span.end)
                            ]);
                        self.diagnostics.push_back(diagnostic);
                    }
                }
                if !map.is_empty() {
                    let mut diagnostic = Diagnostic::error()
                            .with_message("parameter without declaration")
                            .with_labels(vec![Label::primary((), span.start..span.end)]);
                    let mut labels = Vec::new();
                    for pair in map {
                        let span = pair.1[0].span;
                        labels.push(Label::primary((),span.start..span.end));
                    }
                    diagnostic = diagnostic.with_labels(labels);

                    self.diagnostics.push_back(diagnostic);
                }
            }
            // NOTE: Parser guarantees that enum DerivedDeclarator for function 
            // have correct variant
            _ => unreachable!()
        }

        unsafe {
            let func_type = llvm::core::LLVMFunctionType(
                ret_type,
                params.as_mut_ptr(),
                params.len() as _,
                variadic as _,
            );
            let function = llvm::core::LLVMAddFunction(
                self.module, 
                CString::new(identifier.clone().into_bytes()).unwrap().as_ptr(), 
                func_type
            );
        
            let block = llvm::core::LLVMAppendBasicBlockInContext(
                self.context, 
                function, 
                NOP_STUB,
            );
            let builder = llvm::core::LLVMCreateBuilderInContext(self.context);
            llvm::core::LLVMPositionBuilderAtEnd(builder, block);
            // StatementTranslator::new(&self).translate_statement(&ast::stmt::Statement::Compound(func.compound)); 
        }
    }

    /// Translate one translation unit to llvm 
    pub fn translate(&mut self, parse_result: driver::Parse, path: String) -> llvm::prelude::LLVMModuleRef {
        unsafe {
            self.file.as_mut_ptr().write(SimpleFile::new(path.clone(), parse_result.source));
            self.context = llvm::core::LLVMContextCreate();
            self.module = llvm::core::LLVMModuleCreateWithName(path.as_bytes().as_ptr() as _);
            self.builder = llvm::core::LLVMCreateBuilderInContext(self.context);
        }

        for decl in &parse_result.unit.0 {
            match &decl.node {
                ExternalDeclaration::FunctionDefinition(func) => {
                    self.translate_function(func);
                }
                ExternalDeclaration::Declaration(decl) => {
                    self.translate_function_arg_declaration(decl);
                }
                ExternalDeclaration::StaticAssert(assert) => { 
                    todo!()
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

impl<'a> Drop for Translator {
    fn drop(&mut self) {
        unsafe {
            llvm::core::LLVMDisposeBuilder(self.builder);
            llvm::core::LLVMDisposeModule(self.module);
            llvm::core::LLVMContextDispose(self.context);
        }
    }
}

pub struct StatementTranslator<'a> {
    variables: HashMap<String, llvm::prelude::LLVMValueRef>,
    owner: &'a Translator, 
    // expr_trans: MaybeUninit<ExpressionTranslator<'a>>,
}

impl<'a> BaseTranslator for StatementTranslator<'a> {
    fn builder(&self) -> llvm::prelude::LLVMBuilderRef {
        todo!()
    }

    fn context(&self) -> llvm::prelude::LLVMContextRef {
        todo!()
    }

    // fn inspect_store(&self) -> &InspectStore {
    //     todo!()
    // }

    fn translator_store(&self) -> &TranslatorStore {
        todo!()
    }
}

impl<'a> StatementTranslator<'a> {
    pub fn new(owner: &'a Translator) -> Self {
        let mut translator= Self {
            owner,
            variables: Default::default(), 
            // expr_trans: MaybeUninit::uninit()
        };
        // let expr_trans = ExpressionTranslator::new(&translator);
        // translator.expr_trans.write(expr_trans);
        translator
    }

    // fn expr_trans(&mut self) -> &mut ExpressionTranslator<'a> {
    //     unsafe {
    //         // self.expr_trans.assume_init_mut()
    //     }
    // }

    pub fn resolve(&self, identifier: &String) -> llvm::prelude::LLVMValueRef {
        self.variables[identifier]
    }

    pub fn update_variable(&mut self, identifier: &String, value: llvm::prelude::LLVMValueRef) {
        // self.variables[identifier] = value;
        unimplemented!()
    }

    /// Generate 'if' brunch or 'if-else', if 'on_false_stmt' present   
    fn translate_brunch(&mut self, stmt: &'a  Node<WhileStatement>) {
        // unsafe {
        //     let predicate = self.expr_trans().translate(predicate); 
        //     let after_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
        //     let on_true_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
        //     let on_false_block = if on_false_stmt.is_some() {
        //         llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB)
        //     } else {
        //         after_block
        //     };
                
        //     llvm::core::LLVMBuildCondBr(self.builder(), predicate, on_true_block, on_false_block);
        //     llvm::core::LLVMPositionBuilderAtEnd(self.builder(), on_true_block);
        //     self.translate_statement(on_true_stmt);
        //     llvm::core::LLVMBuildBr(self.builder(), after_block);
            
        //     if on_false_stmt.is_some() {
        //         llvm::core::LLVMPositionBuilderAtEnd(self.builder(), on_false_block);
        //         self.translate_statement(on_false_stmt.unwrap());
        //         llvm::core::LLVMBuildBr(self.builder(), after_block);
        //     }
            
        //     llvm::core::LLVMPositionBuilderAtEnd(self.builder(), after_block);
        // }
        todo!()
    }

    fn generate_while(&mut self, stmt: &'a Node<WhileStatement>) {
        // unsafe {
        //     let predicate_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
        //     let body_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
        //     let after_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);

        //     llvm::core::LLVMBuildBr(self.builder(), predicate_block);
        //     llvm::core::LLVMPositionBuilderAtEnd(self.builder(), predicate_block);
        //     let predicate = self.expr_trans.translate(stmt.predicate.as_ref());
        //     llvm::core::LLVMBuildCondBr(self.builder(), predicate, body_block, after_block);
            
        //     llvm::core::LLVMPositionBuilderAtEnd(self.builder(), body_block);
        //     self.translate(stmt.body.as_ref());
        //     llvm::core::LLVMBuildBr(self.builder(), predicate_block);
        // }
        todo!()
    }

    fn generate_dowhile(&mut self, stmt: &'a Node<DoWhileStatement>) {
        // unsafe {
        //     let body_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
        //     let after_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);

        //     llvm::core::LLVMBuildBr(self.builder(), body_block);
        //     llvm::core::LLVMPositionBuilderAtEnd(self.builder(), body_block);
        //     self.translate(stmt.body.as_ref());
        //     let predicate = self.expr_trans.translate(stmt.predicate.as_ref());  
        //     llvm::core::LLVMBuildCondBr(self.builder(), predicate, body_block, after_block);
        // }
        todo!()
    }

    fn generate_for(&mut self, stmt: &'a Node<ForStatement>) {
        // unsafe {
        //     let predicate_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
        //     let body_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
        //     let after_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);

        //     self.expr_trans.translate(stmt.init.as_ref());                
        //     llvm::core::LLVMBuildBr(self.builder(), predicate_block);  
        //     llvm::core::LLVMPositionBuilderAtEnd(self.builder(), predicate_block);
        //     let predicate = self.expr_trans.translate(stmt.predicate.as_ref());
        //     llvm::core::LLVMBuildCondBr(self.builder(), predicate, body_block, after_block);
        //     llvm::core::LLVMPositionBuilderAtEnd(self.builder(), body_block);
        //     self.translate(stmt.body.as_ref());    
        //     self.expr_trans.translate(stmt.step.as_ref());
        // }
        todo!()
    }

    // TODO TODO TODO: неверно работает, так же добавить - default block, проваливание 
    fn generate_switch(&mut self, stmt: &'a Node<SwitchStatement>) {
        todo!()
        // unsafe {
        //     let predicate = self.expr_trans.translate(stmt.value.as_ref());
        //     let after_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
        //     let switch = llvm::core::LLVMBuildSwitch(self.builder(), predicate, after_block, stmt.cases.len().try_into().expect("internal error"));

        //     let mut prev: Option<llvm::prelude::LLVMBasicBlockRef> = None;
        //     for case in stmt.cases {
        //         let case_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
        //         llvm::core::LLVMPositionBuilderAtEnd(self.builder(), case_block);

        //         match case {
        //             ast::stmt::Case::Default(default) => {
        //                 self.translate(default.body.as_ref());
        //             }
        //             ast::stmt::Case::Pattern(pattern) => {
        //                 let pattern_val = Translator::translate_constant(pattern.constant.as_ref());
        //                 self.translate(pattern.body.as_ref());
        //                 llvm::core::LLVMAddCase(switch, pattern_val, case_block);
        //             }
        //         } 

        //         if prev.is_some() {
        //             llvm::core::LLVMPositionBuilderAtEnd(self.builder(), prev.unwrap());
        //             llvm::core::LLVMBuildBr(self.builder(), case_block);
        //         }
                
                
        //         prev = Some(case_block);
        //     }
        //     if prev.is_some() {
        //         llvm::core::LLVMBuildBr(self.builder(), after_block);
        //     }
        // }
    }

    fn translate_statement(&mut self, stmt: &'a Node<Statement>) {
        // unsafe {
        //     use ast::stmt::Statement::*;
        //     match stmt {
        //         Compound(stmt) => {
        //             for item in &stmt.0 {

        //             }
        //         }
        //         IfElse(stmt) => {
        //             // self.generate_brunch(stmt.predicate.as_ref(), stmt.first_stmt.as_ref(), Some(stmt.second_stmt.as_ref()));
        //         }
        //         If(stmt) => {
        //             // self.generate_brunch(stmt.predicate.as_ref(), stmt.first_stmt.as_ref(), None);
        //         }
        //         Return(stmt) => {
        //             // let value = self.expr_trans.translate(stmt.value.as_ref());
        //             // llvm::core::LLVMBuildRet(self.builder(), value);
        //         }
        //         While(stmt) => {
        //             self.generate_while(stmt);
        //         }
        //         DoWhile(stmt) => {
        //             self.generate_dowhile(stmt);
        //         }
        //         For(stmt) => {
        //             self.generate_for(stmt);
        //         }
        //         Switch(stmt) => {
        //             self.generate_switch(stmt);
        //         }
        //         Break => { 
        //             //TODO: for, while or do-while switch 
        //             todo!() 
        //         }
        //         Continue => { 
        //             //TODO: for, while or do-while
        //             todo!() 
        //         }
        //         Expression(expr) => {
        //             // self.expr_trans.translate(expr);
        //             todo!()
        //         }
        //         Goto(_) => {}
        //         Default(_) => {}
        //         Case(_) => {}
        //         Labeled(_) => {}
        //     }   
        // }
        todo!()
    }
}