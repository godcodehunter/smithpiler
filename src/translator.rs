extern crate llvm_sys as llvm;
extern crate libc;
use log::info;
use crate::{inspect_store::InspectStore, ast, ast::TranslationUnit, expression_translator::ExpressionTranslator};
use ast::r#type::Type;
use std::{collections::HashMap, convert::TryInto};
use std::default::Default;

/// Base trait for all translators.
pub trait BaseTranslator {
    fn builder(&self) -> llvm::prelude::LLVMBuilderRef;
    fn context(&self) -> llvm::prelude::LLVMContextRef;
    fn inspect_store(&self) -> &InspectStore;
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
pub fn translate_type_cast<T: BaseTranslator>(translator: &T, value: llvm::prelude::LLVMValueRef, associate: &ast::r#type::Type, target: &ast::r#type::Type) -> llvm::prelude::LLVMValueRef {
    use crate::ast::r#type::Fundamental;

    if associate == target {
        return value;
    }

    fn widening_cast(associate: &ast::r#type::Fundamental, target: &ast::r#type::Fundamental) -> InstBuildFunc {
        match (associate, target) {
            (Fundamental::SignedInteger(_), Fundamental::SignedInteger(_)) => {
                return llvm::core::LLVMBuildSExt;
            },
            (Fundamental::SignedInteger(_), Fundamental::UnsignedInteger(_)) => {
                info!(target: "codegen", "discovered unsafe conversion");
                return llvm::core::LLVMBuildZExt; 
            },
            (Fundamental::SignedInteger(_), Fundamental::Floating(_)) => {
                info!(target: "codegen", "discovered unsafe conversion");
                return llvm::core::LLVMBuildSIToFP;
            },
            (Fundamental::UnsignedInteger(_), Fundamental::UnsignedInteger(_)) => {
                return llvm::core::LLVMBuildZExt;
            },
            (Fundamental::UnsignedInteger(_), Fundamental::SignedInteger(_)) => {
                return llvm::core::LLVMBuildZExt;
            },
            (Fundamental::UnsignedInteger(_), Fundamental::Floating(_)) => {
                info!(target: "codegen", "discovered unsafe conversion");
                return llvm::core::LLVMBuildUIToFP;
            },
            (Fundamental::Floating(_), Fundamental::Floating(_)) => {
                return llvm::core::LLVMBuildFPExt;
            },
            (Fundamental::Floating(_), Fundamental::SignedInteger(_)) => {
                info!(target: "codegen", "discovered unsafe conversion");
                return llvm::core::LLVMBuildFPToSI;
            },
            (Fundamental::Floating(_), Fundamental::UnsignedInteger(_)) => {
                info!(target: "codegen", "discovered unsafe conversion");
                return llvm::core::LLVMBuildFPToUI;
            },
            (Fundamental::Bool, _) => {
                return llvm::core::LLVMBuildZExt;
            },
            _ => unreachable!()
        }
    }

    fn narrowing_cast(associate: &ast::r#type::Fundamental, target: &ast::r#type::Fundamental) -> InstBuildFunc {
        info!(target: "codegen", "discovered narrowing conversion");
        match (associate, target) {
            (_, Fundamental::Bool) => {
                todo!()
            },
            (Fundamental::SignedInteger(_), Fundamental::SignedInteger(_)) => {
                return llvm::core::LLVMBuildTrunc;
            },
            (Fundamental::SignedInteger(_), Fundamental::UnsignedInteger(_)) => {
                return llvm::core::LLVMBuildTrunc;
            },
            (Fundamental::SignedInteger(_), Fundamental::Floating(_)) => {
                return llvm::core::LLVMBuildSIToFP;
            },
            (Fundamental::UnsignedInteger(_), Fundamental::UnsignedInteger(_)) => { 
                return llvm::core::LLVMBuildTrunc;
            },
            (Fundamental::UnsignedInteger(_), Fundamental::SignedInteger(_)) => {
                return llvm::core::LLVMBuildTrunc;
            },
            (Fundamental::UnsignedInteger(_), Fundamental::Floating(_)) => {
                return llvm::core::LLVMBuildUIToFP;
            },
            (Fundamental::Floating(_), Fundamental::Floating(_)) => {
                return llvm::core::LLVMBuildFPTrunc;
            },
            (Fundamental::Floating(_), Fundamental::SignedInteger(_)) => {
                return llvm::core::LLVMBuildFPToSI;
            },
            (Fundamental::Floating(_), Fundamental::UnsignedInteger(_)) => {
                return llvm::core::LLVMBuildFPToUI;
            },
            _ => unreachable!()
        }
    }

    let is_narrowing = translator.inspect_store().get_type_size(associate) > translator.inspect_store().get_type_size(target);

    let conversion = match (associate, target) {
        (Type::Fundamental(lhs), Type::Fundamental(rhs)) if !is_narrowing => widening_cast(lhs, rhs),
        (Type::Fundamental(lhs), Type::Fundamental(rhs)) if is_narrowing => narrowing_cast(lhs, rhs),
        _ => {
            info!(target: "codegen", "discovered unsafe conversion");
            llvm::core::LLVMBuildBitCast
        }
    };

    unsafe {
        conversion(translator.builder(), value, translator.translator_store().get_generated(target), NOP_STUB)
    }
}

//TODO: temporary placeholder 
pub const NOP_STUB: *const libc::c_char = b"nop\0".as_ptr() as _;

struct FuncEntry {
    llvm_entry: llvm::prelude::LLVMValueRef,
}

pub struct TranslatorStore<'ast> {
    generated_types: HashMap<&'ast ast::r#type::Type, llvm::prelude::LLVMTypeRef>,
    generate_values: HashMap<&'ast ast::decl::VarDecl, llvm::prelude::LLVMValueRef>,
    //TODO: функции могут быть в двух состояниях протранслированные, заготовки 
    generated_functions: HashMap<&'ast ast::decl::FuncDef, llvm::prelude::LLVMTypeRef>,  
}

impl<'ast> TranslatorStore<'ast> {
    fn new() -> Self {
        Self {
            generated_types: Default::default(),
            generate_values: Default::default(), 
            generated_functions: Default::default(),
        }
    }

    fn get_generated(&self, ty: &ast::r#type::Type) -> llvm::prelude::LLVMTypeRef {
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

pub struct Translator<'ast> {
    context: llvm::prelude::LLVMContextRef,
    module: llvm::prelude::LLVMModuleRef,
    builder: llvm::prelude::LLVMBuilderRef,
    
    inspect_store: InspectStore<'ast>,
    translator_store: TranslatorStore<'ast>,

    build_query: Vec<llvm::prelude::LLVMTypeRef>,
}

type InstBuildFunc = unsafe extern "C" fn(llvm::prelude::LLVMBuilderRef, llvm::prelude::LLVMValueRef, llvm::prelude::LLVMTypeRef, *const libc::c_char) -> llvm::prelude::LLVMValueRef;

impl<'ast> BaseTranslator for Translator<'ast> {
    fn builder(&self) -> llvm::prelude::LLVMBuilderRef {
        self.builder
    }

    fn context(&self) -> llvm::prelude::LLVMContextRef {
        self.context
    }

    fn inspect_store(&self) -> &InspectStore {
        &self.inspect_store
    }

    fn translator_store(&self) -> &TranslatorStore {
        &self.translator_store
    }
}

impl<'ast> Translator<'ast> {
    pub fn new(inspect_store: InspectStore<'ast>) -> Self {
        unsafe {
            let context = llvm::core::LLVMContextCreate();
            let module = llvm::core::LLVMModuleCreateWithName(NOP_STUB);
            let builder = llvm::core::LLVMCreateBuilderInContext(context);
        
            Self {
                context,
                module,
                builder,
                build_query: Default::default(), 
                inspect_store,
                translator_store: TranslatorStore::new(),
            }
        }
    }
 
    fn translate_type(&mut self, context: llvm::prelude::LLVMContextRef, r#type: &ast::r#type::Type) -> llvm::prelude::LLVMTypeRef {
        let translate_fundamental = |r#type: &ast::r#type::Fundamental| -> llvm::prelude::LLVMTypeRef {
            use ast::r#type::Fundamental::*;
            match r#type {
                SignedInteger(r#type) => {
                    unsafe {
                        match r#type {
                            ast::r#type::SignedIntegerType::SignedChar => {
                                llvm::core::LLVMInt8TypeInContext(context)
                            },
                            ast::r#type::SignedIntegerType::ShortInt => {
                                llvm::core::LLVMInt16TypeInContext(context)
                            },
                            ast::r#type::SignedIntegerType::Int => {
                                llvm::core::LLVMInt32TypeInContext(context)
                            },
                            ast::r#type::SignedIntegerType::LongInt => {
                                llvm::core::LLVMInt64TypeInContext(context)
                            },
                            ast::r#type::SignedIntegerType::LongLongInt => {
                                llvm::core::LLVMInt128TypeInContext(context)
                            },
                        }
                    }
                },
                UnsignedInteger(r#type) => {
                    unsafe {
                        match r#type {
                            ast::r#type::UnsignedIntegerType::UnsignedChar => {
                                llvm::core::LLVMInt8TypeInContext(context)
                            },
                            ast::r#type::UnsignedIntegerType::UnsignedShort => {
                                llvm::core::LLVMInt16TypeInContext(context)
                            },
                            ast::r#type::UnsignedIntegerType::UnsignedInt => {
                                llvm::core::LLVMInt32TypeInContext(context)
                            },
                            ast::r#type::UnsignedIntegerType::UnsignedLong => {
                                llvm::core::LLVMInt64TypeInContext(context)
                            },
                            ast::r#type::UnsignedIntegerType::UnsignedLongLong => {
                                llvm::core::LLVMInt128TypeInContext(context)
                            }   
                        }
                    }
                },
                Floating(r#type) => {
                    unsafe {
                        match r#type {
                            Float => {
                                llvm::core::LLVMFloatTypeInContext(context)
                            },
                            Double => {
                                llvm::core::LLVMDoubleTypeInContext(context)
                            },
                            LongDouble => {
                                llvm::core::LLVMX86FP80TypeInContext(context)
                            }
                        }
                    }
                },
                Void => {
                    unsafe { llvm::core::LLVMVoidTypeInContext(context) }
                },
                Bool => {
                    unsafe { llvm::core::LLVMInt1TypeInContext(context) }
                },
            }
        };

        let translate_derived = |ty: &ast::r#type::DerivedType| -> llvm::prelude::LLVMTypeRef { 
            unimplemented!()
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
        };

        let translate_enumerated = | ty: &ast::r#type::EnumeratedType | -> llvm::prelude::LLVMTypeRef {
            let bitsize = self.inspect_store().get_type_size(&Type::Enumerated(ty.to_owned())); 
            unsafe { llvm::core::LLVMIntTypeInContext(self.context, bitsize) }
        };

        use ast::r#type::Type::*;
        match r#type {
            Type::Fundamental(fundamental) => translate_fundamental(fundamental),
            Type::Enumerated(enumerated) => translate_enumerated(enumerated),
            Type::Derived(derived) => translate_derived(derived),
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
    
    fn translate_constant(stmt: &ast::expr::Expression) -> llvm::prelude::LLVMValueRef {
        todo!()
    }
    
    // Function entry can be referenced, before we actually generate function body  
    // For example, when we generate function call
    fn create_function_entry(&self, decl: &ast::decl::FuncDef) {
        info!(target: "codegen", "create function entry");

        let mut params = decl.params.iter().map(|param| {
            let param_type = self.inspect_store().get_fundefpar_type(param);
            self.translator_store().get_generated(param_type)
        }).collect::<Vec<_>>();
        unsafe {
            let func_type = llvm::core::LLVMFunctionType(
                self.translator_store().get_generated(decl.return_ty.as_ref()),
                params.as_mut_ptr(),
                decl.params.len().try_into().expect("internal error"),
                false as _,
            );
            let function = llvm::core::LLVMAddFunction(
                self.module, 
                decl.func_name.as_ptr() as _, 
                func_type
            );
        }
    }

    fn translate_function(&self, func: llvm::prelude::LLVMValueRef) {
        unsafe {
            let block = llvm::core::LLVMAppendBasicBlockInContext(
                self.context, 
                func, 
                NOP_STUB,
            );
            let builder = llvm::core::LLVMCreateBuilderInContext(self.context);
            llvm::core::LLVMPositionBuilderAtEnd(builder, block);
            // Self::translate_statement(self.context, builder, self.get_functions(func)); 
        }
    }

    pub fn translate(&mut self, unit: TranslationUnit) -> llvm::prelude::LLVMModuleRef {
        for decl in unit.decls.0 {
            // match decl {
            //     ast::decl::Decl::Func(fn_decl) => {
            //         match fn_decl {
            //             ast::decl::FuncDecl::Def(func) => {
            //                 self.create_function_entry(&func);
            //             }
            //             ast::decl::FuncDecl::Prototype(_) => { todo!() }
            //         }
            //     }
            //     ast::decl::Decl::Var(_) => { todo!() }
            // }
        }
        self.module
    }
}

impl<'a> Drop for Translator<'a> {
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
    expr_trans: ExpressionTranslator<'a>,
}

impl<'a> BaseTranslator for StatementTranslator<'a> {
    fn builder(&self) -> llvm::prelude::LLVMBuilderRef {
        todo!()
    }

    fn context(&self) -> llvm::prelude::LLVMContextRef {
        todo!()
    }

    fn inspect_store(&self) -> &InspectStore {
        todo!()
    }

    fn translator_store(&self) -> &TranslatorStore {
        todo!()
    }
}

impl<'a> StatementTranslator<'a> {
    pub fn new() -> Self {
        // Self {variables: Default::default(), expr_trans: ExpressionTranslator::new()}
        unimplemented!()
    }

    pub fn resolve(&self, identifier: &String) -> llvm::prelude::LLVMValueRef {
        self.variables[identifier]
    }

    pub fn update_variable(&mut self, identifier: &String, value: llvm::prelude::LLVMValueRef) {
        // self.variables[identifier] = value;
        unimplemented!()
    }

    // Generate 'if' brunch or 'if-else', if 'on_false_stmt' present   
    fn generate_brunch(&mut self, predicate: &'a ast::expr::Expression, on_true_stmt: &'a ast::stmt::Statement, on_false_stmt: Option<&'a ast::stmt::Statement>) {
        unsafe {
            let predicate = self.expr_trans.translate(predicate); 
            let after_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
            let on_true_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
            let on_false_block = if on_false_stmt.is_some() {
                llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB)
            } else {
                after_block
            };
                
            llvm::core::LLVMBuildCondBr(self.builder(), predicate, on_true_block, on_false_block);
            llvm::core::LLVMPositionBuilderAtEnd(self.builder(), on_true_block);
            self.translate(on_true_stmt);
            llvm::core::LLVMBuildBr(self.builder(), after_block);
            
            if on_false_stmt.is_some() {
                llvm::core::LLVMPositionBuilderAtEnd(self.builder(), on_false_block);
                self.translate(on_false_stmt.unwrap());
                llvm::core::LLVMBuildBr(self.builder(), after_block);
            }
            
            llvm::core::LLVMPositionBuilderAtEnd(self.builder(), after_block);
        }
    }

    fn generate_while(&mut self, stmt: &'a ast::stmt::WhileStmt) {
        unsafe {
            let predicate_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
            let body_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
            let after_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);

            llvm::core::LLVMBuildBr(self.builder(), predicate_block);
            llvm::core::LLVMPositionBuilderAtEnd(self.builder(), predicate_block);
            let predicate = self.expr_trans.translate(stmt.predicate.as_ref());
            llvm::core::LLVMBuildCondBr(self.builder(), predicate, body_block, after_block);
            
            llvm::core::LLVMPositionBuilderAtEnd(self.builder(), body_block);
            self.translate(stmt.body.as_ref());
            llvm::core::LLVMBuildBr(self.builder(), predicate_block);
        }
    }

    fn generate_dowhile(&mut self, stmt: &'a ast::stmt::DoWhileStmt) {
        unsafe {
            let body_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
            let after_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);

            llvm::core::LLVMBuildBr(self.builder(), body_block);
            llvm::core::LLVMPositionBuilderAtEnd(self.builder(), body_block);
            self.translate(stmt.body.as_ref());
            let predicate = self.expr_trans.translate(stmt.predicate.as_ref());  
            llvm::core::LLVMBuildCondBr(self.builder(), predicate, body_block, after_block);
        }
    }

    fn generate_for(&mut self, stmt: &'a ast::stmt::ForStmt) {
        unsafe {
            let predicate_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
            let body_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
            let after_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);

            // self.expr_trans.translate(stmt.init.as_ref());                
            // llvm::core::LLVMBuildBr(self.builder(), predicate_block);  
            // llvm::core::LLVMPositionBuilderAtEnd(self.builder(), predicate_block);
            // let predicate = self.expr_trans.translate(stmt.predicate.as_ref());
            // llvm::core::LLVMBuildCondBr(self.builder(), predicate, body_block, after_block);
            // llvm::core::LLVMPositionBuilderAtEnd(self.builder(), body_block);
            // self.translate(stmt.body.as_ref());    
            // self.expr_trans.translate(stmt.step.as_ref());
        }
    }

    // TODO TODO TODO: неверно работает, так же добавить - default block, проваливание 
    fn generate_switch(&mut self, stmt: &'a ast::stmt::SwitchStmt) {
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

    // TODO: comment - Tail block 
    fn translate(&mut self, stmt: &'a ast::stmt::Statement) {
        unsafe {
            match stmt {
                ast::stmt::Statement::Compound(stmt) => {
                    stmt.0.iter().map(|expr| self.translate(&expr));
                }
                ast::stmt::Statement::IfElse(stmt) => {
                    self.generate_brunch(stmt.predicate.as_ref(), stmt.first_stmt.as_ref(), Some(stmt.second_stmt.as_ref()));
                }
                ast::stmt::Statement::If(stmt) => {
                    self.generate_brunch(stmt.predicate.as_ref(), stmt.first_stmt.as_ref(), None);
                }
                ast::stmt::Statement::Return(stmt) => {
                    // let value = self.expr_trans.translate(stmt.value.as_ref());
                    // llvm::core::LLVMBuildRet(self.builder(), value);
                }
                ast::stmt::Statement::While(stmt) => {
                    self.generate_while(stmt);
                }
                ast::stmt::Statement::DoWhile(stmt) => {
                    self.generate_dowhile(stmt);
                }
                ast::stmt::Statement::For(stmt) => {
                    self.generate_for(stmt);
                }
                ast::stmt::Statement::Switch(stmt) => {
                    self.generate_switch(stmt);
                }
                ast::stmt::Statement::Break => { 
                    //TODO: for, while or do-while switch 
                    todo!() 
                }
                ast::stmt::Statement::Continue => { 
                    //TODO: for, while or do-while
                    todo!() 
                }
                ast::stmt::Statement::Expression(expr) => {
                    // self.expr_trans.translate(expr);
                    todo!()
                }
                ast::stmt::Statement::Goto(_) => {}
            }   
        }
    }
}