use crate::r#type::*;
use crate::translator::{BaseTranslator, NOP_STUB};

impl Type {
    pub fn translate<'a, T: BaseTranslator<'a>>(&self, translator: &'a T) -> llvm::prelude::LLVMTypeRef {
        use Fundamental::*;
        
        let translate_fundamental = |ty: &Fundamental| -> llvm::prelude::LLVMTypeRef {
            match ty {
                SignedInteger(ty) => {
                    unsafe {
                        match ty {
                            SignedIntegerType::SignedChar => {
                                llvm::core::LLVMInt8Type()
                            },
                            SignedIntegerType::ShortInt => {
                                llvm::core::LLVMInt16Type()
                            },
                            SignedIntegerType::Int => {
                                llvm::core::LLVMInt32Type()
                            },
                            SignedIntegerType::LongInt => {
                                llvm::core::LLVMInt64Type()
                            },
                            SignedIntegerType::LongLongInt => {
                                llvm::core::LLVMInt128Type()
                            },
                        }
                    }
                },
                UnsignedInteger(ty) => {
                    unsafe {
                        match ty {
                            UnsignedIntegerType::UnsignedChar => {
                                llvm::core::LLVMInt8Type()
                            },
                            UnsignedIntegerType::UnsignedShort => {
                                llvm::core::LLVMInt16Type()
                            },
                            UnsignedIntegerType::UnsignedInt => {
                                llvm::core::LLVMInt32Type()
                            },
                            UnsignedIntegerType::UnsignedLong => {
                                llvm::core::LLVMInt64Type()
                            },
                            UnsignedIntegerType::UnsignedLongLong => {
                                llvm::core::LLVMInt128Type()
                            }   
                        }
                    }
                },
                Floating(ty) => {
                    unsafe {
                        match ty {
                            Float => {
                                llvm::core::LLVMFloatType()
                            },
                            Double => {
                                llvm::core::LLVMDoubleType()
                            },
                            LongDouble => {
                                llvm::core::LLVMX86FP80Type()
                            }
                        }
                    }
                },
                Void => {
                    unsafe { llvm::core::LLVMVoidType() }
                },
                Bool => {
                    unsafe { llvm::core::LLVMInt1Type() }
                },
            }
        };

        let translate_derived = |ty: &DerivedType| -> llvm::prelude::LLVMTypeRef { 
            match ty {
                // Array(ty) => {
                //     let elem_type = self.get_generated(ty.elem_type.as_ref());
                //     unsafe { llvm::core::LLVMArrayType(elem_type, ty.size) }
                // },
                // Structure(ty) => {
                //     let fields: Vec<_> = ty.fields.into_iter().map(|field| self.get_generated(field) ).collect();
                //     unsafe { llvm::core::LLVMStructType(fields.as_mut_ptr(), fields.len() as u32, false as llvm_sys::prelude::LLVMBool) }
                // },
                // Union(ty) => {
                //     let bitsize = self.get_type_size(ty); 
                //     unsafe { llvm::core::LLVMIntTypeInContext(self.context, bitsize) }
                // },
                DerivedType::Function(ty) => {
                    let mut params: Vec<llvm::prelude::LLVMTypeRef> = ty.params.iter().map(|i| i.translate(translator)).collect();
                    unsafe {
                        llvm::core::LLVMFunctionType(
                            ty.returned_type.translate(translator),
                            params.as_mut_ptr(),
                            ty.params.len() as _,
                            ty.is_var as _,
                        )
                    }
                },
                _ => todo!()
                // Pointer(ty) => {
                //     let elem_type = self.get_generated(ty.referent_type.as_ref());
                //     unsafe { llvm::core::LLVMPointerType(elem_type, 0) }
                // }
            }
        };

        // let translate_enumerated = | ty: &ast::r#type::EnumeratedType | -> llvm::prelude::LLVMTypeRef {
        //     let bitsize = self.inspect_store().get_type_size(&Type::Enumerated(ty.to_owned())); 
        //     unsafe { llvm::core::LLVMIntTypeInContext(self.context, bitsize) }
        // };

        match self {
            Type::Fundamental(fundamental) => translate_fundamental(fundamental),
        //     Type::Enumerated(enumerated) => translate_enumerated(enumerated),
            Type::Derived(derived) => translate_derived(derived),
            _ => todo!()
        }
    }

}