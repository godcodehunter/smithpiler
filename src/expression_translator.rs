use crate::{
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

    // fn inspect_store(&self) -> &InspectStore {
    //     self.owner.inspect_store()
    // }

    fn translator_store(&self) -> &TranslatorStore {
        self.owner.translator_store()
    }
}

/// Represent any value gotten after translation 
struct TranslatedValue {
    value: llvm::prelude::LLVMValueRef,
    lang_type: crate::r#type::Type,
}

impl<'a> ExpressionTranslator<'a> {
    pub fn new(owner: &'a StatementTranslator<'a>) -> Self {
        Self{owner, root: None, post_translations: Default::default()}
    }

    // Resolves identifier in current translation position
    fn resolve(&self, identifier: &String) -> TranslatedValue {
        // self.owner.resolve(identifier)
        todo!()
    }

    fn update_variable(&mut self, identifier: &String, value: llvm::prelude::LLVMValueRef) {
        // self.owner.update_variable(identifier, value);
        todo!()
    }

    pub fn translate(&mut self, expr: &'a ast::expr::Expression) -> llvm::prelude::LLVMValueRef {
        self.root = Some(expr);
        let result = self.translate_expression_tree(self.root.unwrap());
        while !self.post_translations.is_empty() {
            let action = self.post_translations.pop().unwrap();
            action(self);
        }
        result.value
    }

    fn translate_greater(&self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        unsafe {
            use llvm::LLVMIntPredicate::*;
            let value: llvm::prelude::LLVMValueRef;
            if lhs.lang_type.is_integer() {
                let predicate = if lhs.lang_type.is_signed() {LLVMIntSGT} else {LLVMIntUGT};
                value = llvm::core::LLVMBuildICmp(
                    self.builder(), 
                    predicate, 
                    lhs.value, 
                    rhs.value, 
                    NOP_STUB
                );
            } else {
                value = llvm::core::LLVMBuildFCmp(
                    self.builder(), 
                    llvm::LLVMRealPredicate::LLVMRealOGT, 
                    lhs.value, 
                    rhs.value, 
                    NOP_STUB
                );
            }
            TranslatedValue{value, lang_type: lhs.lang_type} 
        }
    }

    fn translate_less_or_equal(&self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue { 
        unsafe {
            use llvm::LLVMIntPredicate::*;
            let value: llvm::prelude::LLVMValueRef;
            if lhs.lang_type.is_integer() {
                let predicate = if lhs.lang_type.is_signed() {LLVMIntSLE} else {LLVMIntULE};
                value = llvm::core::LLVMBuildICmp(
                    self.builder(), 
                    predicate, 
                    lhs.value, 
                    rhs.value, 
                    NOP_STUB
                );
            } else {
                value = llvm::core::LLVMBuildFCmp(
                    self.builder(), 
                    llvm::LLVMRealPredicate::LLVMRealOLE, 
                    lhs.value, 
                    rhs.value, 
                    NOP_STUB
                );
            }
            TranslatedValue{value, lang_type: lhs.lang_type}
        }
    }

    fn translate_less(&self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        unsafe {
            use llvm::LLVMIntPredicate::*;
            let value: llvm::prelude::LLVMValueRef;
            if lhs.lang_type.is_integer() {
                let predicate = if lhs.lang_type.is_signed() {LLVMIntSLT} else {LLVMIntULT};
                value = llvm::core::LLVMBuildICmp(
                    self.builder(), 
                    predicate, 
                    lhs.value, 
                    rhs.value, 
                    NOP_STUB
                );
            } else {
                value = llvm::core::LLVMBuildFCmp(
                    self.builder(), 
                    llvm::LLVMRealPredicate::LLVMRealOLT, 
                    lhs.value, 
                    rhs.value, 
                    NOP_STUB
                );
            }
            TranslatedValue{value, lang_type: lhs.lang_type}
        }
    }

    fn translate_greater_or_equal(&self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        unsafe {
            use llvm::LLVMIntPredicate::*;
            let value: llvm::prelude::LLVMValueRef;
            if lhs.lang_type.is_integer() {
                let predicate = if lhs.lang_type.is_signed() {LLVMIntSGE} else {LLVMIntUGE};
                value = llvm::core::LLVMBuildICmp(
                    self.builder(), 
                    predicate, 
                    lhs.value, 
                    rhs.value, 
                    NOP_STUB
                );
            } else {
                value = llvm::core::LLVMBuildFCmp(
                    self.builder(), 
                    llvm::LLVMRealPredicate::LLVMRealOGE, 
                    lhs.value, 
                    rhs.value, 
                    NOP_STUB
                );
            }
            TranslatedValue{value, lang_type: lhs.lang_type}
        }
    }

    fn translate_equal(&self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        unsafe {
            let value: llvm::prelude::LLVMValueRef;
            if lhs.lang_type.is_integer() {
                value = llvm::core::LLVMBuildICmp(
                    self.builder(), 
                    llvm::LLVMIntPredicate::LLVMIntEQ, 
                    lhs.value, 
                    rhs.value, 
                    NOP_STUB
                )
            } else {
                value = llvm::core::LLVMBuildFCmp(
                    self.builder(), 
                    llvm::LLVMRealPredicate::LLVMRealOEQ, 
                    lhs.value, 
                    rhs.value, 
                    NOP_STUB
                )
            }
            TranslatedValue{value, lang_type: lhs.lang_type}
        }
    }

    fn translate_not_equal(&self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        unsafe {
            let value: llvm::prelude::LLVMValueRef;
            if lhs.lang_type.is_integer() {
                value = llvm::core::LLVMBuildICmp(
                    self.builder(), 
                    llvm::LLVMIntPredicate::LLVMIntNE, 
                    lhs.value, 
                    rhs.value, 
                    NOP_STUB
                );
            } else {
                value = llvm::core::LLVMBuildFCmp(
                    self.builder(), 
                    llvm::LLVMRealPredicate::LLVMRealUNE, 
                    lhs.value, 
                    rhs.value, 
                    NOP_STUB
                );
            }
            TranslatedValue{value, lang_type: lhs.lang_type}
        }
    }

    fn translate_addition(&self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        unsafe {
            let value = llvm::core::LLVMBuildAdd(
                self.builder(), 
                lhs.value, 
                rhs.value, 
                NOP_STUB
            );
            TranslatedValue{value, lang_type: lhs.lang_type}
        }
    }

    fn translate_subtraction(&self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        unsafe {
            let value = llvm::core::LLVMBuildSub(
                self.builder(), 
                lhs.value, 
                rhs.value, 
                NOP_STUB
            );
            TranslatedValue{value, lang_type: lhs.lang_type}
        }
    }

    fn translate_multiplication(&self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        unsafe {
            let value = llvm::core::LLVMBuildMul(
                self.builder(), 
                lhs.value,
                rhs.value,
                NOP_STUB
            );
            TranslatedValue{value, lang_type: lhs.lang_type}
        }
    }
    fn translate_division(&self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        todo!()
    }

    // Creates a constant '1' of the same type as passed value 
    fn create_one_of_same_type(ty: &crate::r#type::Type) -> llvm::prelude::LLVMValueRef {
        todo!()
        // llvm::LLVMTypeKind::LLVMIntegerTypeKind => {
        //     llvm::core::LLVMConstInt(ty, 1, ty.is_signed() as _)
        // },
        // LLVMFloatTypeKind | LLVMDoubleTypeKind | LLVMX86_FP80TypeKind => {
        //     llvm::core::LLVMConstReal(ty, 1)
        // },
    }

    fn translate_increment(&self, mut value: TranslatedValue) -> TranslatedValue {
        let one = Self::create_one_of_same_type(&value.lang_type);
        unsafe {
            value.value = llvm::core::LLVMBuildAdd(
                self.builder(), 
                value.value, 
                one, 
                NOP_STUB
            );
            value
        }
    }

    fn translate_decrement(&self, mut value: TranslatedValue) -> TranslatedValue {
        let one = Self::create_one_of_same_type(&value.lang_type);
        unsafe {
            value.value = llvm::core::LLVMBuildSub(
                self.builder(), 
                value.value, 
                one, 
                NOP_STUB
            );
            value
        }
    }

    fn translate_bitwise_not(&self, mut value: TranslatedValue) -> TranslatedValue{
        unsafe {
            value.value = llvm::core::LLVMBuildNot(
                self.builder(), 
                value.value, 
                NOP_STUB
            );
            value
        }
    }

    fn translate_negation(&self, mut value: TranslatedValue) -> TranslatedValue {
        unsafe {
            value.value = llvm::core::LLVMBuildNeg(self.builder(), value.value, NOP_STUB);
            value
        }
    }

    fn translate_logical_not(&self, mut value: TranslatedValue) -> TranslatedValue {
        unsafe {
            value.value = llvm::core::LLVMBuildNot(self.builder(), value.value, NOP_STUB);
            value
        }
    }

    fn translate_logical_or(&self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        // let target_ty = Type::Fundamental(Fundamental::Bool);
        // let lhs = translate_type_cast(self, lhs.0, &lhs.1, &target_ty);
        // let rhs = translate_type_cast(self, rhs.0, &rhs.1, &target_ty);
        unsafe  {
            let value = llvm::core::LLVMBuildOr(self.builder(), lhs.value, rhs.value, NOP_STUB);
            TranslatedValue{value, lang_type: lhs.lang_type}
        }
    }

    fn translate_logical_and(&self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        // let target_ty = Type::Fundamental(Fundamental::Bool);
        // let lhs = translate_type_cast(self, lhs.0, &lhs.1, &target_ty);
        // let rhs = translate_type_cast(self, rhs.0, &rhs.1, &target_ty);

        unsafe {
            let value= llvm::core::LLVMBuildAnd(
                self.builder(), 
                lhs.value, 
                rhs.value, 
                NOP_STUB
            );
            TranslatedValue{value, lang_type: lhs.lang_type}
        }
    }

    fn translate_call(&self, callie: llvm::prelude::LLVMValueRef) -> TranslatedValue {
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

    fn translate_sizeof(&self, node: &ast::expr::Sizeof)  -> TranslatedValue {
        todo!()
        // unsafe {
        //     llvm::core::LLVMSizeOf(ty);
        // }
    }

    fn translate_assign(&self) -> TranslatedValue {
        todo!()
    }

    fn translate_literal(&self, literal: &ast::expr::Literal) -> TranslatedValue {
        use ast::expr::Literal::*;
        use crate::r#type::*;

        match literal {
            Char(lit) => { 
                unsafe {
                    let lang_type = Type::new_unsigned_char(); 
                    let value = llvm::core::LLVMConstInt(
                        llvm::core::LLVMInt8TypeInContext(self.context()),
                        lit.value.into(),
                        true as _,
                    );
                    TranslatedValue{value, lang_type} 
                }
            }
            Integer(lit) => { 
                unsafe { 
                    let lang_type = Type::new_signed_int(); 
                    let value = llvm::core::LLVMConstInt(
                        llvm::core::LLVMInt8TypeInContext(self.context()),
                        lit.value,
                        true as _,
                    );
                    TranslatedValue{value, lang_type} 
                }
            }
            Str(lit) => {
                unsafe { 
                    let char_ty = Type::new_unsigned_char();
                    let lang_type = Type::Derived(DerivedType::Pointer(PointerType(Box::new(char_ty)))); 
                    let value = llvm::core::LLVMConstStringInContext(
                        self.context(),
                        lit.value.as_ptr() as _, 
                        lit.value.len().try_into().expect("internal error"),
                        false as _,
                    );
                    TranslatedValue{value, lang_type} 
                }
            }
            Float(lit) => { 
                unsafe { 
                    let lang_type = Type::Fundamental(Fundamental::Floating(FloatingType::Float));
                    let value = llvm::core::LLVMConstReal(
                        llvm::core::LLVMFloatTypeInContext(self.context()), 
                        0.0 //TODO: lit.value, 
                    );
                    TranslatedValue{value, lang_type} 
                }
            }
        }
    }

    fn translate_variable() -> TranslatedValue {
        // llvm::core::LLVMBuildAlloca(builder, type_ref, b"test2\0".as_ptr() as _);
        todo!()
    }

    fn childrens(expr: &'a ast::expr::Expression) -> Box<dyn Iterator<Item = &'a ast::expr::Expression> + 'a> {
        // match expr {
        //     ast::expr::Expression::Identifier(_) => {
        //         Box::new(std::iter::empty())
        //     }
        //     ast::expr::Expression::OneOperand(op) => {
        //         Box::new(std::iter::once(&op.value))
        //     }
        //     ast::expr::Expression::TwoOperands(op) => {
        //         Box::new(std::iter::once(&op.lhs).chain(std::iter::once(&op.rhs)))
        //     }
        //     ast::expr::Expression::Literal(_) => {
        //         Box::new(std::iter::empty())
        //     }
        //     // ast::expr::Expression::Call(_) => {
        //     //     todo!()
        //     // }
        //     ast::expr::Expression::Cast(op) => {
        //         Box::new(std::iter::once(&op.expr))
        //     }
        //     // ast::expr::Expression::GenericSelection(_) => {
        //     //     todo!()
        //     // }
        //     // ast::expr::Expression::CompoundLiteral(_) => {
        //     //     &mut std::iter::empty()
        //     // }
        //     // ast::expr::Expression::Ternary(_) => {
        //     //     todo!()
        //     // }
        //     ast::expr::Expression::Sizeof(_) => {
        //         Box::new(std::iter::empty())
        //     }
        //     ast::expr::Expression::AlignOf(_) => {
        //         Box::new(std::iter::empty())
        //     }
        //     ast::expr::Expression::None => {
        //         panic!()
        //     }
        //     _ => todo!(),
        // }
        todo!()
    }

    // Use DFS to for crawling and convert expression tree to SSA
    // fn translate_expression_tree(&mut self, expr: &'a ast::expr::Expression) -> TranslatedValue {
    // //     let mut stack = vec![expr];
    // //     while !stack.is_empty() {
    // //         let last = stack.pop().unwrap();
    // //         // TODO: map 
    // //         Self::childrens(last).map(|item| stack.push(item));
    // //     }

    // //     let mut flatten = Vec::<&ast::expr::Expression>::new();
    // //     let mut tmp = Vec::<TranslatedValue>::new();
    // //     for item in flatten {
    // //         unsafe { 
    // //             let translated =  match item {
    // //                 ast::expr::Expression::Literal(lit) => self.translate_literal(lit),
    // //                 ast::expr::Expression::Identifier(ident) => self.resolve(ident),
    // //                 ast::expr::Expression::OneOperand(expr) => {
    // //                     use ast::expr::UnaryOp::*;

    // //                     let value = tmp.pop().unwrap();
    // //                     match expr.op {
    // //                         Address => todo!(),
    // //                         Indirection => todo!(),
    // //                         Positive => todo!(),
    // //                         Negative => self.translate_negation(value),
    // //                         LogicalNot => {
    // //                             // let target_ty = Type::Fundamental(Fundamental::Bool);
    // //                             // let converted = translate_type_cast(self, value.0, &value.1, &target_ty);
    // //                             // self.translate_logical_not(converted, target_ty)
    // //                             todo!()
    // //                         },
    // //                         BitwiseNot => self.translate_bitwise_not(value),
    // //                         Increment => self.translate_increment(value),
    // //                         Postincrement => {
    // //                             // self.post_translations.push(Box::new(|s: &mut Self| {
    // //                             //     let new_value = self.translate_increment(value.0, value.1);
    // //                             //     self.update_variable(expr.value.as_ref(), new_value.0);
    // //                             // }));
    // //                             //value
    // //                             todo!()
    // //                         },
    // //                         Decrement => self.translate_decrement(value),
    // //                         Postdecrement => {
    // //                             // self.post_translations.push(Box::new(|s: &mut Self| {
    // //                             //     let new_value = self.translate_decrement(value.0, value.1);
    // //                             //     self.update_variable(expr.value.as_ref(), new_value.0);
    // //                             // }));
    // //                             // value
    // //                             todo!()
    // //                         },
    // //                     }
    // //             },
    // //             ast::expr::Expression::TwoOperands(stmt) => {
    // //                 let lhs = tmp.pop().unwrap();
    // //                 let rhs = tmp.pop().unwrap();
                    
    // //                 use ast::expr::BiTag::*;
    // //                 match stmt.op {
    // //                     Less => self.translate_less(lhs, rhs),
    // //                     LessEqual => self.translate_less_or_equal(lhs, rhs),
    // //                     Greater => self.translate_greater(lhs, rhs),
    // //                     GreaterEqual => self.translate_greater_or_equal(lhs, rhs),
    // //                     Equal => self.translate_equal(lhs, rhs),
    // //                     NotEqual => self.translate_not_equal(lhs, rhs),
    // //                     Add => self.translate_addition(lhs, rhs),
    // //                     Sub => self.translate_subtraction(lhs, rhs),
    // //                     Mul => self.translate_multiplication(lhs, rhs),
    // //                     Div => self.translate_division(lhs, rhs),
    // //                     LogicalOr => self.translate_logical_or(lhs, rhs),
    // //                     LogicalAnd => self.translate_logical_and(lhs, rhs),
    // //                     Subscript => todo!(),
    // //                     _ => todo!()
    // //                 }
    // //             },
    // //             ast::expr::Expression::Sizeof(expr) => self.translate_sizeof(expr),
    // // //             ast::expr::Expr::Cast(expr)=> {
    // // //                 let val = self.translate_expression_tree(expr.expr.as_ref()); 
    // // //                 let val = translate_type_cast(self, val.0, &val.1, expr.ty.as_ref());
    // // //                 (val, expr.ty.as_ref().to_owned())
    // // //             }
    // // //             ast::expr::Expr::Call(expr) => {
    // // //                 let callie = self.translate_expression_tree(expr.callie.as_ref());
    // // //                 let args = expr.args.into_iter().map(|expr| self.translate_expression_tree(&expr)).collect::<Vec<_>>();
    // // //                 self.translate_call(callie.0)
    // // //             }
    // // //             ast::expr::Expr::Assign(assign) => {
    // // //                 // self.update_variable(identifier, value);
    // // //                 self.translate_assign()
    // // //             }
    // // //         }
    // //             _ => todo!()
    // //             };
    // //             tmp.push(translated);
    // //         }
    // //     }

    //     todo!()
    // }
}




