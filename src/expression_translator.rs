use crate::{
    inspect_store::InspectStore, 
    translator::{
        translate_type_cast, 
        BaseTranslator, 
        StatementTranslator, 
        NOP_STUB, 
        TranslatorStore
    }
};
use std::{default::Default, convert::TryInto};
extern crate llvm_sys as llvm;
use crate::ast;


pub struct ExpressionTranslator<'a> {
    owner: &'a StatementTranslator<'a>,    
    root: Option<&'a ast::expr::Expression>,
    // Used to delay translation for post increment/decrement operators.
    post_translations: Vec<Box<dyn Fn(&mut Self) -> ()>>,
}

impl<'a> BaseTranslator for ExpressionTranslator<'a> {
    fn builder(&self) -> llvm::prelude::LLVMBuilderRef {
        self.owner.builder()
    }

    fn context(&self) -> llvm::prelude::LLVMContextRef {
        self.owner.context()
    }

    fn inspect_store(&self) -> &InspectStore {
        self.owner.inspect_store()
    }

    fn translator_store(&self) -> &TranslatorStore {
        self.owner.translator_store()
    }
}

impl<'a> ExpressionTranslator<'a> {
    pub fn new(owner: &'a StatementTranslator<'a>) -> Self {
        Self{owner, root: None, post_translations: Default::default()}
    }

    // Resolves identifier in current translation position
    fn resolve(&self, identifier: &String) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        // self.owner.resolve(identifier)
        todo!()
    }

    fn update_variable(&mut self, identifier: &String, value: llvm::prelude::LLVMValueRef) {
        // self.owner.update_variable(identifier, value);
        todo!()
    }

    pub fn translate(&mut self, expr: &'a ast::expr::Expression) -> llvm::prelude::LLVMValueRef {
        self.root = Some(expr);
        let res = self.translate_expression_tree(self.root.unwrap());
        while !self.post_translations.is_empty() {
            let action = self.post_translations.pop().unwrap();
            action(self);
        }
        res.0
    }

    fn translate_greater(&self, lhs: llvm::prelude::LLVMValueRef, rhs: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        unsafe {
            use llvm::LLVMIntPredicate::*;
            let val: llvm::prelude::LLVMValueRef;
            if ty.is_integer() {
                let predicate = if ty.is_signed() {LLVMIntSGT} else {LLVMIntUGT};
                val = llvm::core::LLVMBuildICmp(
                    self.builder(), 
                    predicate, 
                    lhs, 
                    rhs, 
                    NOP_STUB
                );
            } else {
                val = llvm::core::LLVMBuildFCmp(
                    self.builder(), 
                    llvm::LLVMRealPredicate::LLVMRealOGT, 
                    lhs, 
                    rhs, 
                    NOP_STUB
                );
            }
            (val, ty)    
        }
    }

    fn translate_less_or_equal(&self, lhs: llvm::prelude::LLVMValueRef, rhs: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        unsafe {
            use llvm::LLVMIntPredicate::*;
            let val: llvm::prelude::LLVMValueRef;
            if ty.is_integer() {
                let predicate = if ty.is_signed() {LLVMIntSLE} else {LLVMIntULE};
                val = llvm::core::LLVMBuildICmp(
                    self.builder(), 
                    predicate, 
                    lhs, 
                    rhs, 
                    NOP_STUB
                );
            } else {
                val = llvm::core::LLVMBuildFCmp(
                    self.builder(), 
                    llvm::LLVMRealPredicate::LLVMRealOLE, 
                    lhs, 
                    rhs, 
                    NOP_STUB
                );
            }
            (val, ty)
        }
    }

    fn translate_less(&self, lhs: llvm::prelude::LLVMValueRef, rhs: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        unsafe {
            use llvm::LLVMIntPredicate::*;
            let val: llvm::prelude::LLVMValueRef;
            if ty.is_integer() {
                let predicate = if ty.is_signed() {LLVMIntSLT} else {LLVMIntULT};
                val = llvm::core::LLVMBuildICmp(
                    self.builder(), 
                    predicate, 
                    lhs, 
                    rhs, 
                    NOP_STUB
                );
            } else {
                val = llvm::core::LLVMBuildFCmp(
                    self.builder(), 
                    llvm::LLVMRealPredicate::LLVMRealOLT, 
                    lhs, 
                    rhs, 
                    NOP_STUB
                );
            }
            (val, ty)
        }
    }

    fn translate_greater_or_equal(&self, lhs: llvm::prelude::LLVMValueRef, rhs: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        unsafe {
            use llvm::LLVMIntPredicate::*;
            let val: llvm::prelude::LLVMValueRef;
            if ty.is_integer() {
                let predicate = if ty.is_signed() {LLVMIntSGE} else {LLVMIntUGE};
                val = llvm::core::LLVMBuildICmp(
                    self.builder(), 
                    predicate, 
                    lhs, 
                    rhs, 
                    NOP_STUB
                );
            } else {
                val = llvm::core::LLVMBuildFCmp(
                    self.builder(), 
                    llvm::LLVMRealPredicate::LLVMRealOGE, 
                    lhs, 
                    rhs, 
                    NOP_STUB
                );
            }
            (val, ty)
        }
    }

    fn translate_equal(&self, lhs: llvm::prelude::LLVMValueRef, rhs: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        unsafe {
            let val: llvm::prelude::LLVMValueRef;
            if ty.is_integer() {
                val = llvm::core::LLVMBuildICmp(
                    self.builder(), 
                    llvm::LLVMIntPredicate::LLVMIntEQ, 
                    lhs, 
                    rhs, 
                    NOP_STUB
                )
            } else {
                val = llvm::core::LLVMBuildFCmp(
                    self.builder(), 
                    llvm::LLVMRealPredicate::LLVMRealOEQ, 
                    lhs, 
                    rhs, 
                    NOP_STUB
                )
            }
            (val, ty)
        }
    }

    fn translate_not_equal(&self, lhs: llvm::prelude::LLVMValueRef, rhs: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        unsafe {
            let val: llvm::prelude::LLVMValueRef;
            if ty.is_integer() {
                val = llvm::core::LLVMBuildICmp(
                    self.builder(), 
                    llvm::LLVMIntPredicate::LLVMIntNE, 
                    lhs, 
                    rhs, 
                    NOP_STUB
                );
            } else {
                val = llvm::core::LLVMBuildFCmp(
                    self.builder(), 
                    llvm::LLVMRealPredicate::LLVMRealUNE, 
                    lhs, 
                    rhs, 
                    NOP_STUB
                );
            }
            (val, ty)
        }
    }

    fn translate_addition(&self, lhs: llvm::prelude::LLVMValueRef, rhs: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        unsafe {
            let val = llvm::core::LLVMBuildAdd(self.builder(), lhs, rhs, NOP_STUB);
            (val, ty)
        }
    }

    fn translate_subtraction(&self, lhs: llvm::prelude::LLVMValueRef, rhs: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        unsafe {
            let val = llvm::core::LLVMBuildSub(self.builder(), lhs, rhs, NOP_STUB);
            (val, ty)
        }
    }

    fn translate_multiplication(&self, lhs: llvm::prelude::LLVMValueRef, rhs: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        unsafe {
            let val = llvm::core::LLVMBuildMul(self.builder(), lhs, rhs, NOP_STUB);
            (val, ty)
        }
    }
    fn translate_division(&self, lhs: llvm::prelude::LLVMValueRef, rhs: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        todo!()
    }

    // Creates a constant '1' of the same type as passed value 
    fn create_one_of_same_type(ty: &ast::r#type::Type) -> llvm::prelude::LLVMValueRef {
        todo!()
        // llvm::LLVMTypeKind::LLVMIntegerTypeKind => {
        //     llvm::core::LLVMConstInt(ty, 1, ty.is_signed() as _)
        // },
        // LLVMFloatTypeKind | LLVMDoubleTypeKind | LLVMX86_FP80TypeKind => {
        //     llvm::core::LLVMConstReal(ty, 1)
        // },
    }

    // fn translate_increment(&self, value: llvm::prelude::LLVMValueRef, ty: &ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, &ast::r#type::Type) {
    //     let one = Self::create_one_of_same_type(ty);
    //     unsafe {
    //         let val = llvm::core::LLVMBuildAdd(self.builder(), value, one, NOP_STUB);
    //         (val, ty)
    //     }
    // }

    // fn translate_decrement(&self, value: llvm::prelude::LLVMValueRef, ty: &ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, &ast::r#type::Type) {
    //     let one = Self::create_one_of_same_type(ty);
    //     unsafe {
    //         let val = llvm::core::LLVMBuildSub(self.builder(), value, one, NOP_STUB);
    //         (val, ty)
    //     }
    // }

    // fn translate_bitwise_not(&self, value: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, &ast::r#type::Type) {
    //     unsafe {
    //         let val = llvm::core::LLVMBuildNot(self.builder(), value, NOP_STUB);
    //         (val, ty)
    //     }
    // }

    fn translate_negation(&self, value: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        unsafe {
            let val = llvm::core::LLVMBuildNeg(self.builder(), value, NOP_STUB);
            (val, ty)
        }
    }

    fn translate_logical_not(&self, value: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        unsafe {
            let val = llvm::core::LLVMBuildNot(self.builder(), value, NOP_STUB);
            (val, ty)
        }
    }

    fn translate_logical_or(&self, lhs: llvm::prelude::LLVMValueRef, rhs: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        unsafe  {
            let val = llvm::core::LLVMBuildOr(self.builder(), lhs, rhs, NOP_STUB);
            (val, ty)
        }
    }

    fn translate_logical_and(&self, lhs: llvm::prelude::LLVMValueRef, rhs: llvm::prelude::LLVMValueRef, ty: ast::r#type::Type) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        unsafe {
            let val = llvm::core::LLVMBuildAnd(self.builder(), lhs, rhs, NOP_STUB);
            (val, ty)
        }
    }

    fn translate_call(&self, callie: llvm::prelude::LLVMValueRef) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        //TODO: only if callie is identifier 
        // let func = self.get_function(expr.callie);                  
        // // expr.args compile separaitle 
        // llvm::core::LLVMBuildCall2(
        //     self.builder(),
        //     ,
        //     func,
        //     ,
        //     expr.args.len() as u32,
        //     NOP_STUB
        // )
        todo!()
    }

    fn translate_sizeof(&self, ty: llvm::prelude::LLVMTypeRef)  -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        unsafe {
            // llvm::core::LLVMSizeOf(ty);
            todo!()
        }
    }

    fn translate_assign(&self) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        todo!()
    }

    fn translate_literal(&self, literal: &ast::expr::Literal) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        use ast::expr::Literal::*;
        use ast::r#type::*;

        match literal {
            Char(lit) => { 
                unsafe {
                    let ty = Type::Fundamental(Fundamental::UnsignedInteger(UnsignedIntegerType::UnsignedChar)); 
                    let value = llvm::core::LLVMConstInt(
                        llvm::core::LLVMInt8TypeInContext(self.context()),
                        lit.value.into(),
                        true as _,
                    );
                    (value, ty) 
                }
            }
            Integer(lit) => { 
                unsafe { 
                    let ty = Type::Fundamental(Fundamental::SignedInteger(SignedIntegerType::Int)); 
                    let value = llvm::core::LLVMConstInt(
                        llvm::core::LLVMInt8TypeInContext(self.context()),
                        lit.value,
                        true as _,
                    );
                    (value, ty) 
                }
            }
            Str(lit) => {
                unsafe { 
                    let char_ty = Type::Fundamental(Fundamental::UnsignedInteger(UnsignedIntegerType::UnsignedChar));
                    let ty = Type::Derived(DerivedType::Pointer(PointerType(Box::new(char_ty)))); 
                    let value = llvm::core::LLVMConstStringInContext(
                        self.context(),
                        lit.value.as_ptr() as _, 
                        lit.value.len().try_into().expect("internal error"),
                        false as _,
                    );
                    (value, ty) 
                }
            }
            Float(lit) => { 
                unsafe { 
                    let ty = Type::Fundamental(Fundamental::Floating(FloatingType::Float));
                    let value = llvm::core::LLVMConstReal(
                        llvm::core::LLVMFloatTypeInContext(self.context()), 
                        0.0 //TODO: lit.value, 
                    );
                    (value, ty) 
                }
            }
        }
    }

    fn translate_variable() {
        // llvm::core::LLVMBuildAlloca(builder, type_ref, b"test2\0".as_ptr() as _);
    }

    fn translate_expression_tree(&mut self, expr: &'a ast::expr::Expression) -> (llvm::prelude::LLVMValueRef, ast::r#type::Type) {
        todo!()
    //     unsafe { 
    //         match expr {
    //             ast::expr::Expr::Cast(expr)=> {
    //                 let val = self.translate_expression_tree(expr.expr.as_ref()); 
    //                 let val = translate_type_cast(self, val.0, &val.1, expr.ty.as_ref());
    //                 (val, expr.ty.as_ref().to_owned())
    //             }
    //             ast::expr::Expr::OneOperand(expr) => {
    //                 use ast::r#type::*;

    //                 let value = self.translate_expression_tree(expr.value.as_ref());
    //                 match expr.op {
    //                     ast::expr::UnaryOp::Neg => { 
    //                         self.translate_negation(value.0, value.1)
    //                     }
    //                     ast::expr::UnaryOp::BitwiseNot => {
    //                         self.translate_bitwise_not(value.0, value.1)
    //                     }
    //                     ast::expr::UnaryOp::LogicalNot => {
    //                         let target_ty = Type::Fundamental(Fundamental::Bool);
    //                         let converted = translate_type_cast(self, value.0, &value.1, &target_ty);
    //                         self.translate_logical_not(converted, target_ty)
    //                     }
    //                     ast::expr::UnaryOp::Increment => {
    //                         self.translate_increment(value.0, value.1)
    //                     }
    //                     ast::expr::UnaryOp::Postincrement => {
    //                         // self.post_translations.push(Box::new(|s: &mut Self| {
    //                         //     let new_value = self.translate_increment(value.0, value.1);
    //                         //     self.update_variable(expr.value.as_ref(), new_value.0);
    //                         // }));
    //                         //value
    //                         todo!()
    //                     }
    //                     ast::expr::UnaryOp::Decrement => {
    //                         self.translate_decrement(value.0, value.1)
    //                     }
    //                     ast::expr::UnaryOp::Postdecrement => {
    //                         // self.post_translations.push(Box::new(|s: &mut Self| {
    //                         //     let new_value = self.translate_decrement(value.0, value.1);
    //                         //     self.update_variable(expr.value.as_ref(), new_value.0);
    //                         // }));
    //                         // value
    //                         todo!()
    //                     }
    //                     ast::expr::UnaryOp::Sizeof => {
    //                         // self.translate_sizeof()
    //                         todo!()
    //                     }
    //                 }
    //             }
    //             ast::expr::Expr::TwoOperands(stmt) => {
    //                 use ast::r#type::*;

    //                 let lhs = self.translate_expression_tree(stmt.lhs.as_ref());
    //                 let rhs = self.translate_expression_tree(stmt.rhs.as_ref());
    //                 let ty = lhs.1;

    //                 match stmt.op {
    //                     ast::expr::BinOp::Less => { 
    //                         self.translate_less(lhs.0, rhs.0, ty)
    //                     }
    //                     ast::expr::BinOp::LessEqual => { 
    //                         self.translate_less_or_equal(lhs.0, rhs.0, ty)
    //                     }
    //                     ast::expr::BinOp::Greater => { 
    //                         self.translate_greater(lhs.0, rhs.0, ty)
    //                     }
    //                     ast::expr::BinOp::GreaterEqual => { 
    //                         self.translate_greater_or_equal(lhs.0, rhs.0, ty)
    //                     }
    //                     ast::expr::BinOp::Equal => { 
    //                         self.translate_equal(lhs.0, rhs.0, ty)
    //                     }
    //                     ast::expr::BinOp::NotEqual => { 
    //                         self.translate_not_equal(lhs.0, rhs.0, ty)
    //                     }
    //                     ast::expr::BinOp::Plus => { 
    //                         self.translate_addition(lhs.0, rhs.0, ty)
    //                     }
    //                     ast::expr::BinOp::Minus => { 
    //                         self.translate_subtraction(lhs.0, rhs.0, ty)
    //                     }
    //                     ast::expr::BinOp::Multiply => { 
    //                        self.translate_division(lhs.0, rhs.0, ty)
    //                     }
    //                     ast::expr::BinOp::Divide => { 
    //                         todo!()
    //                     }
    //                     ast::expr::BinOp::LogicalOr => { 
    //                         let target_ty = Type::Fundamental(Fundamental::Bool);
    //                         let lhs = translate_type_cast(self, lhs.0, &lhs.1, &target_ty);
    //                         let rhs = translate_type_cast(self, rhs.0, &rhs.1, &target_ty);

    //                         self.translate_logical_or(lhs, rhs, target_ty)
    //                     }
    //                     ast::expr::BinOp::LogicalAnd => { 
    //                         let target_ty = Type::Fundamental(Fundamental::Bool);
    //                         let lhs = translate_type_cast(self, lhs.0, &lhs.1, &target_ty);
    //                         let rhs = translate_type_cast(self, rhs.0, &rhs.1, &target_ty);

    //                         self.translate_logical_and(lhs, rhs, target_ty)
    //                     }
    //                     ast::expr::BinOp::MemberAccess => {
    //                         todo!()
    //                     }
    //                 }
    //             }
    //             ast::expr::Expr::Literal(lit) => {
    //                 self.translate_literal(lit)
    //             }
    //             ast::expr::Expr::Call(expr) => {
    //                 let callie = self.translate_expression_tree(expr.callie.as_ref());
    //                 let args = expr.args.into_iter().map(|expr| self.translate_expression_tree(&expr)).collect::<Vec<_>>();
    //                 self.translate_call(callie.0)
    //             }
    //             ast::expr::Expr::Assign(assign) => {
    //                 // self.update_variable(identifier, value);
    //                 self.translate_assign()
    //             }
    //             ast::expr::Expr::Identifier(ident) => {
    //                 self.resolve(ident)
    //             }
    //         }
    //     }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_name() {
        
    }
}