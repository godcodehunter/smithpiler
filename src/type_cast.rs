use crate::r#type;
use crate::translator::{BaseTranslator, NOP_STUB};
use lang_c::{span::Node, ast::Expression};

/// Force(don't care about cast legality) generate any of possible type conversion in language(union of explicit and implicit casts).  
/// **NOTE**: For equivalent types doing nothing.
///
/// # Arguments
/// * `translator` - translator that perform generation
/// * `value` - convertible value
/// * `associate` - values type
/// * `target` - target type to which the conversion will be performed
pub fn translate_type_cast<'a, T: BaseTranslator<'a>>(
    translator: &T, 
    value: llvm::prelude::LLVMValueRef, 
    associate: &r#type::Type, 
    target: &r#type::Type,
    source: &Node<Expression>
) -> llvm::prelude::LLVMValueRef {
    use crate::r#type::Fundamental;
    type InstBuildFunc = unsafe extern "C" fn(llvm::prelude::LLVMBuilderRef, llvm::prelude::LLVMValueRef, llvm::prelude::LLVMTypeRef, *const libc::c_char) -> llvm::prelude::LLVMValueRef;
    
    if associate == target {
        return value;
    }

    fn widening_cast(associate: &Fundamental, target: &Fundamental) -> InstBuildFunc {
        match (associate, target) {
            (Fundamental::SignedInteger(_), Fundamental::SignedInteger(_)) => {
                return llvm::core::LLVMBuildSExt;
            },
            (Fundamental::SignedInteger(_), Fundamental::UnsignedInteger(_)) => {
                // info!(target: "codegen", "discovered unsafe conversion");
                return llvm::core::LLVMBuildZExt; 
            },
            (Fundamental::SignedInteger(_), Fundamental::Floating(_)) => {
                // info!(target: "codegen", "discovered unsafe conversion");
                return llvm::core::LLVMBuildSIToFP;
            },
            (Fundamental::UnsignedInteger(_), Fundamental::UnsignedInteger(_)) => {
                return llvm::core::LLVMBuildZExt;
            },
            (Fundamental::UnsignedInteger(_), Fundamental::SignedInteger(_)) => {
                return llvm::core::LLVMBuildZExt;
            },
            (Fundamental::UnsignedInteger(_), Fundamental::Floating(_)) => {
                // info!(target: "codegen", "discovered unsafe conversion");
                return llvm::core::LLVMBuildUIToFP;
            },
            (Fundamental::Floating(_), Fundamental::Floating(_)) => {
                return llvm::core::LLVMBuildFPExt;
            },
            (Fundamental::Floating(_), Fundamental::SignedInteger(_)) => {
                // info!(target: "codegen", "discovered unsafe conversion");
                return llvm::core::LLVMBuildFPToSI;
            },
            (Fundamental::Floating(_), Fundamental::UnsignedInteger(_)) => {
                // info!(target: "codegen", "discovered unsafe conversion");
                return llvm::core::LLVMBuildFPToUI;
            },
            (Fundamental::Bool, _) => {
                return llvm::core::LLVMBuildZExt;
            },
            _ => unreachable!()
        }
    }

    fn narrowing_cast(a: &Fundamental, t: &Fundamental) -> InstBuildFunc {
        // info!(target: "codegen", "discovered narrowing conversion");
        match (a, t) {
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

    // Special case of narrowing conversion 
    if matches!((associate, target), (_, r#type::Type::Fundamental(Fundamental::Bool))) {
        unsafe {
            let ty = llvm::core::LLVMTypeOf(value);
            let kind = llvm::core::LLVMGetTypeKind(ty);
            use llvm::LLVMTypeKind::*;
            let c = match kind {
                LLVMIntegerTypeKind => {
                    llvm::core::LLVMConstInt(ty, 0, associate.is_signed() as _)
                },
                LLVMFloatTypeKind | LLVMDoubleTypeKind | LLVMX86_FP80TypeKind => {
                    llvm::core::LLVMConstReal(ty, 0.0)
                },
                _ => todo!(),
            };
 
            let ret: llvm::prelude::LLVMValueRef;
            if associate.is_integer() {
                ret = llvm::core::LLVMBuildICmp(
                    translator.builder(), 
                    llvm::LLVMIntPredicate::LLVMIntNE, 
                    value, 
                    c, 
                    NOP_STUB
                );
            } else if associate.is_real(){
                ret = llvm::core::LLVMBuildFCmp(
                    translator.builder(), 
                    llvm::LLVMRealPredicate::LLVMRealUEQ, 
                    value, 
                    c, 
                    NOP_STUB
                );
            } else {
                panic!()
            }
            return ret;
        }
    }

    let is_narrowing = associate.rank() > target.rank();
    let conversion = match (associate, target) {
        (r#type::Type::Fundamental(lhs), r#type::Type::Fundamental(rhs)) if !is_narrowing => widening_cast(lhs, rhs),
        (r#type::Type::Fundamental(lhs), r#type::Type::Fundamental(rhs)) if is_narrowing => narrowing_cast(lhs, rhs),
        _ => {
            // info!(target: "codegen", "discovered unsafe conversion");
            llvm::core::LLVMBuildBitCast
        }
    };

    unsafe {
        conversion(translator.builder(), value, target.translate(translator), NOP_STUB)
    }
}

pub fn implicit_type_cast<'a, T: BaseTranslator<'a>>(
    translator: &T, 
    value1: llvm::prelude::LLVMValueRef, 
    associate1: &r#type::Type, 
    value2: llvm::prelude::LLVMValueRef, 
    associate2: &r#type::Type,
    source: &Node<Expression>
) -> (r#type::Type, llvm::prelude::LLVMValueRef, llvm::prelude::LLVMValueRef) {
    let v: llvm::prelude::LLVMValueRef;
    let v2: llvm::prelude::LLVMValueRef;
    let t1: &r#type::Type;
    let t2: &r#type::Type;
    if associate1.rank() > associate2.rank() {
        v = value2;
        v2 = value1;
        t1 = associate2;
        t2 = associate1;
    } else {
        v = value1;
        v2 = value2;
        t1 = associate1;
        t2 = associate2;
    }

    (t2.clone(), translate_type_cast(translator, v, t1, t2, source), v2)
}
