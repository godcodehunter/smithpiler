use lang_c::{ast::*, span::Node};
use crate::{translator::{BaseTranslator, NOP_STUB, TranslatedValue, Translator}, type_cast::{implicit_type_cast, translate_type_cast}};
use crate::r#type::Type;

impl<'ast> Translator<'ast> {
    fn translate_greater(&mut self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
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

    fn translate_less_or_equal(&mut self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue { 
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

    fn translate_less(&mut self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
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

    fn translate_greater_or_equal(&mut self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
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

    fn translate_equal(&mut self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
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

    fn translate_not_equal(&mut self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
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

    fn translate_addition(&mut self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        unsafe {
            let value = if lhs.lang_type.is_integer() {
                    llvm::core::LLVMBuildAdd(
                    self.builder(), 
                    lhs.value, 
                    rhs.value, 
                    NOP_STUB
                ) 
            } else if lhs.lang_type.is_real() {
                llvm::core::LLVMBuildFAdd(
                    self.builder(), 
                    lhs.value, 
                    rhs.value, 
                    NOP_STUB
                )
            } else {
                panic!()
            };
            TranslatedValue{value, lang_type: lhs.lang_type}
        }
    }

    fn translate_subtraction(&mut self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
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

    fn translate_multiplication(&mut self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        unsafe {
            let value = if lhs.lang_type.is_integer() {
                llvm::core::LLVMBuildMul(
                    self.builder(), 
                    lhs.value,
                    rhs.value,
                    NOP_STUB
                )
            } else if lhs.lang_type.is_real() {
                llvm::core::LLVMBuildFMul(self.builder(), lhs.value, rhs.value, NOP_STUB)
            } else {
                panic!()
            };
            TranslatedValue{value, lang_type: lhs.lang_type}
        }
    }
    fn translate_division(&self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        todo!()
    }

    fn translate_module(&self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
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

    fn translate_increment(&mut self, mut value: TranslatedValue) -> TranslatedValue {
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

    fn translate_decrement(&mut self, mut value: TranslatedValue) -> TranslatedValue {
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

    fn translate_bitwise_not(&mut self, mut value: TranslatedValue) -> TranslatedValue {
        unsafe {
            value.value = llvm::core::LLVMBuildNot(
                self.builder(), 
                value.value, 
                NOP_STUB
            );
            value
        }
    }
    
    fn translate_bitwise_and(&mut self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
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

    fn translate_bitwise_xor(&mut self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        todo!()
    }
    

    fn translate_bitwise_or(&mut self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        unsafe {
            let value = llvm::core::LLVMBuildOr(self.builder(), lhs.value, rhs.value, NOP_STUB);
            TranslatedValue{value, lang_type: lhs.lang_type}
        }
    }

    fn translate_negation(&mut self, mut value: TranslatedValue) -> TranslatedValue {
        unsafe {
            value.value = llvm::core::LLVMBuildNeg(self.builder(), value.value, NOP_STUB);
            value
        }
    }

    fn translate_logical_not(&mut self, mut value: TranslatedValue) -> TranslatedValue {
        unsafe {
            value.value = llvm::core::LLVMBuildNot(self.builder(), value.value, NOP_STUB);
            value
        }
    }

    fn translate_logical_or(&mut self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
        unsafe  {
            let value = llvm::core::LLVMBuildOr(self.builder(), lhs.value, rhs.value, NOP_STUB);
            TranslatedValue{value, lang_type: lhs.lang_type}
        }
    }

    fn translate_logical_and(&mut self, lhs: TranslatedValue, rhs: TranslatedValue) -> TranslatedValue {
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

    fn translate_call<I: Iterator<Item = llvm::prelude::LLVMValueRef>>(&mut self, callie: llvm::prelude::LLVMValueRef, iter: I) -> TranslatedValue {                 
        // unsafe {
        //     llvm::core::LLVMBuildCall2(
        //         self.builder(),
        //         ,
        //         func,
        //         ,
        //         iter.len() as _,
        //         NOP_STUB
        //     )
        // }
        todo!()
    }

    fn translate_sizeof(&mut self, node: &Node<TypeName>)  -> TranslatedValue {
        todo!()
        // unsafe {
        //     llvm::core::LLVMSizeOf(ty);
        // }
    }

    fn translate_assign(&mut self) -> TranslatedValue {
        todo!()
    }

    fn translate_constant(&mut self, constant: &Node<Constant>) -> TranslatedValue {
        match &constant.node {
            Constant::Integer(lit) => {
                unsafe { 
                    //TODO: more conversion
                    let lang_type = Type::new_signed_int(); 
                    let value = llvm::core::LLVMConstInt(
                        lang_type.translate(self),
                        lit.number.parse::<i32>().unwrap() as _, 
                        true as _,
                    );
                    TranslatedValue{value, lang_type} 
                }
            }
            Constant::Float(lit) => {
                //TODO: more conversion 
                unsafe { 
                    let lang_type = Type::new_float();
                    let value = llvm::core::LLVMConstReal(
                        lang_type.translate(self), 
                        lit.number.parse::<f32>().unwrap() as _, 
                    );
                    TranslatedValue{value, lang_type} 
                }
            }
            Constant::Character(lit) => {
                //TODO: check out of range 0-255
                unsafe {
                    let lang_type = Type::new_unsigned_char(); 
                    let value = llvm::core::LLVMConstInt(
                        lang_type.translate(self),
                        *lit.as_ptr() as _,
                        true as _,
                    );
                    TranslatedValue{value, lang_type} 
                }
            }
        }
    }

    fn translate_string_literal(&self, lit: &Node<Vec<String>>) -> TranslatedValue {
        // unsafe { 
        //     let char_ty = Type::new_unsigned_char();
        //     let lang_type = Type::Derived(DerivedType::Pointer(PointerType(Box::new(char_ty)))); 
        //     let value = llvm::core::LLVMConstStringInContext(
        //         self.context(),
        //         lit.value.as_ptr() as _, 
        //         lit.value.len(),
        //         false as _,
        //     );
        //     TranslatedValue{value, lang_type} 
        // }
        todo!()
    }

    fn flatten(expr: &Node<Expression>) -> Vec<&Node<Expression>> {
        let mut flatten = Vec::new();

        let mut stack = vec![expr];
        while !stack.is_empty() {
            let last = stack.pop().unwrap();
            flatten.push(last);
            for neighbor in last.neighbors() {
                stack.push(neighbor)
            }
        }

        flatten
    }

    pub fn translate_expression(&mut self, expr: &Node<Expression>) -> TranslatedValue {
        let post_translations: Vec<Box<dyn Fn(&mut Self) -> ()>>;
        let flatten = Self::flatten(expr);
        let mut tmp_stack = Vec::<TranslatedValue>::new();

        for item in flatten.iter().rev() {
            let translated = self.step(item, &mut tmp_stack);
            tmp_stack.push(translated);
        }
        
        // while !post_translations.is_empty() {
        //     let action = post_translations.pop().unwrap();
        //     action(self);
        // }
        tmp_stack.pop().unwrap()
    }

    fn step(&mut self, node: &Node<Expression>, tmp: &mut Vec<TranslatedValue>) -> TranslatedValue {
        unsafe { 
            match &node.node {
                Expression::Identifier(ident) => {
                    self.resolve_variable(&ident.node)
                },
                Expression::Constant(constant) => {
                    self.translate_constant(&*constant)
                },
                Expression::StringLiteral(lit) => {
                    self.translate_string_literal(&*lit)
                },
                Expression::GenericSelection(_) => todo!(),
                Expression::CompoundLiteral(_) => todo!(),
                Expression::Member(_) => {
                    todo!()
                },
                Expression::Call(expr) => {
                    let callie = tmp.pop().unwrap();  
                    // self.translate_call(callie.0, tmp.into_iter())
                    todo!()
                },
                Expression::SizeOf(expr) => {
                    self.translate_sizeof(&*expr)
                },
                Expression::AlignOf(expr) => {
                    todo!()
                },
                Expression::UnaryOperator(expr) => {
                    match expr.node.operator.node {
                        UnaryOperator::PostIncrement => todo!(),
                        UnaryOperator::PostDecrement => todo!(),
                        UnaryOperator::PreIncrement => todo!(),
                        UnaryOperator::PreDecrement => todo!(),
                        UnaryOperator::Address => todo!(),
                        UnaryOperator::Indirection => todo!(),
                        UnaryOperator::Plus => todo!(),
                        UnaryOperator::Minus => todo!(),
                        UnaryOperator::Complement => todo!(),
                        UnaryOperator::Negate => todo!(),
                        UnaryOperator::SizeOf => todo!(),
                    }
                },
                Expression::Cast(expr)=> {
                    // let val = self.translate_expression_tree(expr.expr.as_ref()); 
                    // let val = translate_type_cast(self, val.0, &val.1, expr.ty.as_ref());
                    // (val, expr.ty.as_ref().to_owned())
                    todo!()
                },
                Expression::BinaryOperator(expr) => {
                    let lhs = tmp.pop().unwrap();
                    let rhs = tmp.pop().unwrap();
                    
                    let (lang_type, lhs, rhs) = implicit_type_cast(
                        self, 
                        lhs.value, 
                        &lhs.lang_type, 
                        rhs.value, 
                        &rhs.lang_type,
                        node
                    );
                    let lhs = TranslatedValue{value: lhs, lang_type: lang_type.clone()};
                    let rhs = TranslatedValue{value: rhs, lang_type: lang_type.clone()};

                    match expr.node.operator.node {
                        BinaryOperator::Index => todo!(),
                        BinaryOperator::Multiply => self.translate_multiplication(lhs, rhs),
                        BinaryOperator::Divide => self.translate_division(lhs, rhs),
                        BinaryOperator::Modulo => self.translate_module(lhs, rhs),
                        BinaryOperator::Plus => self.translate_addition(lhs, rhs),
                        BinaryOperator::Minus => self.translate_subtraction(lhs, rhs),
                        BinaryOperator::ShiftLeft => todo!(),
                        BinaryOperator::ShiftRight => todo!(),
                        BinaryOperator::Less =>  self.translate_less(lhs, rhs),
                        BinaryOperator::Greater => self.translate_greater(lhs, rhs),
                        BinaryOperator::LessOrEqual => self.translate_less_or_equal(lhs, rhs),
                        BinaryOperator::GreaterOrEqual => self.translate_greater_or_equal(lhs, rhs),
                        BinaryOperator::Equals => self.translate_equal(lhs, rhs),
                        BinaryOperator::NotEquals => self.translate_not_equal(lhs, rhs),
                        BinaryOperator::BitwiseAnd => self.translate_bitwise_and(lhs, rhs),
                        BinaryOperator::BitwiseXor => self.translate_bitwise_xor(lhs, rhs),
                        BinaryOperator::BitwiseOr => self.translate_bitwise_or(lhs, rhs),
                        BinaryOperator::LogicalOr => {
                            let ty = Type::new_bool();
                            let lhs = translate_type_cast(self, lhs.value, &lhs.lang_type, &ty, node);
                            let rhs = translate_type_cast(self, rhs.value, &rhs.lang_type, &ty, node);
                            let lhs = TranslatedValue{value: lhs, lang_type: ty.clone()};
                            let rhs = TranslatedValue{value: rhs, lang_type: ty.clone()};

                            self.translate_logical_or(lhs, rhs)
                        },
                        BinaryOperator::LogicalAnd => {
                            let ty = Type::new_bool();
                            let lhs = translate_type_cast(self, lhs.value, &lhs.lang_type, &ty, node);
                            let rhs = translate_type_cast(self, rhs.value, &rhs.lang_type, &ty, node);
                            let lhs = TranslatedValue{value: lhs, lang_type: ty.clone()};
                            let rhs = TranslatedValue{value: rhs, lang_type: ty.clone()};

                            self.translate_logical_and(lhs, rhs)
                        },
                        BinaryOperator::Assign => todo!(),
                        BinaryOperator::AssignMultiply => todo!(),
                        BinaryOperator::AssignDivide => todo!(),
                        BinaryOperator::AssignModulo => todo!(),
                        BinaryOperator::AssignPlus => todo!(),
                        BinaryOperator::AssignMinus => todo!(),
                        BinaryOperator::AssignShiftLeft => todo!(),
                        BinaryOperator::AssignShiftRight => todo!(),
                        BinaryOperator::AssignBitwiseAnd => todo!(),
                        BinaryOperator::AssignBitwiseXor => todo!(),
                        BinaryOperator::AssignBitwiseOr => todo!(),
                    }
                },
                Expression::Conditional(expr) => {
                    let condition = tmp.pop().unwrap();
                    let on_success = tmp.pop().unwrap();
                    let on_failure = tmp.pop().unwrap();
                    todo!()
                },
                Expression::Comma(_) => { 
                    todo!()
                },
                Expression::OffsetOf(_) => {
                    todo!()
                },
                _ => todo!()
            }
        }
    }
}

trait Neighbors {
    type Item;
    type Iter: Iterator<Item = Self::Item>;
    fn neighbors(&self) -> Self::Iter;
}

impl<'a> Neighbors for &'a Node<Expression> {
    type Item = Self;

    type Iter = Box<dyn Iterator<Item = Self::Item> +'a>;

    fn neighbors(&self) -> Self::Iter {
        match &self.node {
            Expression::Identifier(_) => {
                Box::new(std::iter::empty())
            }
            Expression::Constant(_) => {
                Box::new(std::iter::empty())
            }
            Expression::StringLiteral(_) => {
                Box::new(std::iter::empty())
            }
            Expression::GenericSelection(_) => {
                Box::new(std::iter::empty())
            }
            Expression::CompoundLiteral(_) => {
                Box::new(std::iter::empty())
            }
            Expression::Member(expr) => {
                Box::new(
                    std::iter::once(&*expr.node.expression)
                    .chain(std::iter::once(&*expr.node.expression))
                )
            }
            Expression::Call(call) => {
                Box::new(
                    std::iter::once(&*call.node.callee)
                    .chain(call.node.arguments.iter())
                )
            }
            Expression::SizeOf(_) => {
                Box::new(std::iter::empty())
            }
            Expression::AlignOf(_) => {
                Box::new(std::iter::empty())
            }
            Expression::UnaryOperator(expr) => {
                Box::new(std::iter::once(&*expr.node.operand))
            }
            Expression::Cast(expr) => {
                Box::new(std::iter::once(&*expr.node.expression))
            }
            Expression::BinaryOperator(expr) => {
                Box::new(
                    std::iter::once(&*expr.node.lhs)
                    .chain(std::iter::once(&*expr.node.rhs))
                )
            }
            Expression::Conditional(expr) => {
                Box::new(
                    std::iter::once(&*expr.node.condition)
                    .chain(std::iter::once(&*expr.node.else_expression))
                    .chain(std::iter::once(&*expr.node.then_expression))
                )
            }
            // Expression::Comma(expr) => {
            //     todo!()
            // }
            // Expression::OffsetOf(_) => {
            //     todo!()
            // }
            // Expression::VaArg(_) => {
            //     todo!()
            // }
            // Expression::Statement(_) => {
            //     todo!()
            // }
            _ => todo!(),
        }
    }
}
