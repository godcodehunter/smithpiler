/// An isolated expression executer built on top of the llvm engine used for compile-time evaluation

use std::ffi::CString;
use llvm_sys::{
    core::*, 
    execution_engine::*, 
    prelude::*
};
use lang_c::{ast::*, span::Node};
use crate::{type_cast::translate_type_cast, translator::{Translator, BaseTranslator, TranslatedValue}};
use crate::diagnostics::*;
use crate::r#type::Type;


pub struct CTEnv {
    module: LLVMModuleRef,
    engine: LLVMExecutionEngineRef,
    function: LLVMValueRef,
    block: LLVMBasicBlockRef,
    builder: LLVMBuilderRef,
}

impl CTEnv {
    pub fn new() -> Result<Self, String> {
        unsafe {
            LLVMLinkInInterpreter();
            let module = LLVMModuleCreateWithName(b"ct_env\0".as_ptr() as _);
            let engine: *mut LLVMExecutionEngineRef = &mut std::ptr::null_mut();
            let error: *mut *mut i8 = &mut std::ptr::null_mut();
            if LLVMCreateExecutionEngineForModule(engine, module, error) != 0 {
                return Err(CString::from_raw(*error).into_string().unwrap());
            }
            let func_type = LLVMFunctionType(
                LLVMInt8Type(),
                [].as_mut_ptr(),
                0,
                false as _,
            );
            let function = LLVMAddFunction(
                module, 
                b"ct_expr\0".as_ptr() as _, 
                func_type
            );
            let block = LLVMAppendBasicBlock(
                function, 
                b"body\0".as_ptr() as _,
            );
            let builder = LLVMCreateBuilder();
            LLVMPositionBuilderAtEnd(builder, block);
            
            Ok(Self {
                module,
                engine: *engine,
                function,
                block,
                builder
            })
        }
    }

    fn erase(&mut self) {
        unsafe {
            loop {
                let inst = LLVMGetFirstInstruction(self.block); 
                if inst == std::ptr::null_mut() {
                    break;
                }
                LLVMInstructionEraseFromParent(inst); 
            }          
        }
    }

    pub fn execute<'a>(&'a mut self, translator: &'a mut Translator<'a> , expr: &'a Node<Expression>) -> u64 {
        unsafe {
            struct ProxyTranslator<'a>{
                cte: &'a mut CTEnv,
                rt: &'a mut Translator<'a> ,
            }

            impl<'ast> BaseTranslator<'ast> for ProxyTranslator<'ast> {
                fn builder(&self) -> LLVMBuilderRef {
                    self.cte.builder
                }
                fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
                    self.rt.add_diagnostic(diagnostic)
                }
                fn resolve_symbol(&self, identifier: &Identifier) -> TranslatedValue {
                    self.rt.resolve_symbol(identifier)
                }
                fn update_symbol(&mut self, identifier: &'ast Identifier, value: TranslatedValue) {
                    unimplemented!("not allowed on compile time environment")
                }
                fn resolve_typename(&self, identifier: &Identifier) -> Type {
                    self.rt.resolve_typename(identifier)
                }

                fn context(&self) -> LLVMContextRef {
                    self.rt.context
                }
            }

            let mut proxy = ProxyTranslator{
                cte: self,
                rt: translator,
            };
            let v = Translator::translate_expression(&mut proxy, expr);
            
            LLVMBuildRet(
                proxy.builder(), 
            translate_type_cast(
                &proxy, 
                v.value, 
                &v.lang_type, 
                &Type::new_signed_char(), 
                expr
                )
            );
            let value = LLVMRunFunction(
                proxy.cte.engine, 
                proxy.cte.function, 
                0, 
                [].as_mut_ptr()
            );
            let ret = LLVMGenericValueToInt(value, true as _);
            proxy.cte.erase();
            ret
        }
    }
}

impl Drop for CTEnv {
    fn drop(&mut self) {
        unsafe { LLVMDisposeModule(self.module); }
    }
}