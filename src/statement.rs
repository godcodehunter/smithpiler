use crate::{expression::*, translator::*};
use lang_c::{ast::*, span::Node};
use llvm_sys::prelude::LLVMBasicBlockRef;
use crate::r#type;
use std::collections::HashMap;
use crate::diagnostics::*;
use crate::type_cast::*;
use crate::translator::*;
use llvm::core::*;
use crate::r#type::Type;

struct TranslateStatement {
    lost_control: bool,
    ret_type: Type,
}

impl<'a> Translator<'a> {
    /// Generate 'if' brunch or 'if-else', if 'on_false_stmt' present   
    fn translate_brunch(&mut self, stmt: &'a  Node<IfStatement>) {
        unsafe {
            fn init_after_block(trans: &mut Translator<'_>) -> LLVMBasicBlockRef {
                unsafe {
                    LLVMCreateBasicBlockInContext(trans.context(), b"if.merge\0".as_ptr() as _)
                }
            }

            let mut merge_block: Option<LLVMBasicBlockRef> = None;
            let pred_block = LLVMCreateBasicBlockInContext(self.context, b"if.pred\0".as_ptr() as _);
            let then_block = LLVMCreateBasicBlockInContext(self.context, b"if.then\0".as_ptr() as _);
            let else_block = if stmt.node.else_statement.is_some() {
                let v = LLVMCreateBasicBlockInContext(self.context, b"if.else\0".as_ptr() as _);
                LLVMInsertExistingBasicBlockAfterInsertBlock(self.builder(), v);
                v
            } else {
                merge_block = Some(init_after_block(self));
                merge_block.unwrap()
            };

            LLVMInsertExistingBasicBlockAfterInsertBlock(self.builder(), then_block);
            LLVMInsertExistingBasicBlockAfterInsertBlock(self.builder(), pred_block);
            
            LLVMBuildBr(self.builder(), pred_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), pred_block);
            let t_v = Self::translate_expression(self, stmt.node.condition.as_ref());
            let predicate = translate_type_cast(
                self,
                t_v.value, 
                &t_v.lang_type, 
                &Type::new_bool(), 
                stmt.node.condition.as_ref()
            );
            LLVMBuildCondBr(self.builder(), predicate, then_block, else_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), then_block);
            self.translate_statement(stmt.node.then_statement.as_ref());
            if LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(self.builder())) == std::ptr::null_mut() {
                if merge_block.is_none() {
                    merge_block = Some(init_after_block(self));
                }
                LLVMBuildBr(self.builder(), merge_block.unwrap());
            }
            
            if stmt.node.else_statement.is_some() {
                LLVMPositionBuilderAtEnd(self.builder(), else_block);
                self.translate_statement(stmt.node.else_statement.as_ref().unwrap());
                if LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(self.builder())) == std::ptr::null_mut() {
                    if merge_block.is_none() {
                        merge_block = Some(init_after_block(self));
                    }
                    LLVMBuildBr(self.builder(), merge_block.unwrap());
                }
            }
            
            
            if merge_block.is_some() {
                LLVMInsertExistingBasicBlockAfterInsertBlock(self.builder(), merge_block.unwrap());
                LLVMPositionBuilderAtEnd(self.builder(), merge_block.unwrap());
            } 
        }
    }

    fn generate_while(&mut self, stmt: &'a Node<WhileStatement>) {
        unsafe {
            let pred_block = LLVMCreateBasicBlockInContext(self.context, b"while.pred\0".as_ptr() as _);
            let body_block = LLVMCreateBasicBlockInContext(self.context, b"while.body\0".as_ptr() as _); 
            let merge_block= LLVMCreateBasicBlockInContext(self.context, b"while.merge\0".as_ptr() as _); 
            LLVMInsertExistingBasicBlockAfterInsertBlock(self.builder(), merge_block);
            LLVMInsertExistingBasicBlockAfterInsertBlock(self.builder(), body_block);
            LLVMInsertExistingBasicBlockAfterInsertBlock(self.builder(), pred_block);

            LLVMBuildBr(self.builder(), pred_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), pred_block);
            let t_v = Self::translate_expression(self, stmt.node.expression.as_ref());
            let predicate = translate_type_cast(
                self,
                t_v.value, 
                &t_v.lang_type, 
                &Type::new_bool(), 
                stmt.node.expression.as_ref()
            );
            LLVMBuildCondBr(self.builder(), predicate, body_block, merge_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), body_block);
            self.translate_statement(stmt.node.statement.as_ref());
            if LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(self.builder())) == std::ptr::null_mut() {
                LLVMBuildBr(self.builder(), pred_block);
            }

            LLVMPositionBuilderAtEnd(self.builder(), merge_block);
        }
    }

    fn generate_dowhile(&mut self, stmt: &'a Node<DoWhileStatement>) {
        unsafe {
            let prev = LLVMGetInsertBlock(self.builder());
            let func = LLVMGetBasicBlockParent(prev);

            let body_block = LLVMAppendBasicBlockInContext(self.context, func, b"do_while.body\0".as_ptr() as _); 
            let pred_block = LLVMAppendBasicBlockInContext(self.context, func, b"do_while.pred\0".as_ptr() as _);
            let merge_block = LLVMAppendBasicBlockInContext(self.context, func, b"do_while.merge\0".as_ptr() as _); 
            LLVMInsertExistingBasicBlockAfterInsertBlock(self.builder(), merge_block);
            LLVMInsertExistingBasicBlockAfterInsertBlock(self.builder(), pred_block);
            LLVMInsertExistingBasicBlockAfterInsertBlock(self.builder(), body_block);
            
            LLVMBuildBr(self.builder(), body_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), body_block);
            self.translate_statement(stmt.node.statement.as_ref());
            LLVMBuildBr(self.builder(), pred_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), pred_block);
            let t_v = Self::translate_expression(self, stmt.node.expression.as_ref());
            let predicate = translate_type_cast(
                self,
                t_v.value, 
                &t_v.lang_type, 
                &Type::new_bool(), 
                stmt.node.expression.as_ref()
            );
            LLVMBuildCondBr(self.builder(), predicate, body_block, merge_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), merge_block);
        }
    }

    fn generate_for(&mut self, stmt: &'a Node<ForStatement>) {
        unsafe {
            let prev = LLVMGetInsertBlock(self.builder());
            let func = LLVMGetBasicBlockParent(prev);

            let initializer = LLVMAppendBasicBlockInContext(self.context, func, b"for.init\0".as_ptr() as _);
            let predicate_block = LLVMAppendBasicBlockInContext(self.context, func, b"for.pred\0".as_ptr() as _);
            let step = LLVMAppendBasicBlockInContext(self.context, func, b"for.step\0".as_ptr() as _);
            let body_block = LLVMAppendBasicBlockInContext(self.context, func, b"for.body\0".as_ptr() as _); 
            let after_block = LLVMAppendBasicBlockInContext(self.context, func, b"for.merge\0".as_ptr() as _); 

            LLVMBuildBr(self.builder(), initializer);

            LLVMPositionBuilderAtEnd(self.builder(), initializer);
            match &stmt.node.initializer.node {
                ForInitializer::Empty => {}
                ForInitializer::Expression(expr) => {
                    Self::translate_expression(self, &expr);
                }
                ForInitializer::Declaration(_) => { todo!() }
                ForInitializer::StaticAssert(assert) => {
                    self.check_static_assert(assert);
                }
            }
            LLVMBuildBr(self.builder(), predicate_block);

            LLVMPositionBuilderAtEnd(self.builder(), predicate_block);
            if let Some(predicate) = &stmt.node.condition {
                let t_v = Self::translate_expression(self, predicate); 
                let predicate = translate_type_cast(
                    self,
                    t_v.value, 
                    &t_v.lang_type, 
                    &Type::new_bool(), 
                    stmt.node.condition.as_ref().unwrap()
                );  
                LLVMBuildCondBr(self.builder(), predicate, step, after_block);
            } else {
                LLVMBuildBr(self.builder(), step);
            }

            LLVMPositionBuilderAtEnd(self.builder(), step);
            if let Some(step) = &stmt.node.step {
                Self::translate_expression(self, step); 
            }
            LLVMBuildBr(self.builder(), body_block);


            LLVMPositionBuilderAtEnd(self.builder(), body_block);
            self.translate_statement(&stmt.node.statement);
            LLVMBuildBr(self.builder(), predicate_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), after_block);
        }
    }

    fn generate_switch(&mut self, stmt: &'a Node<SwitchStatement>) {
        unsafe {
            let prev = LLVMGetInsertBlock(self.builder());
            let func = LLVMGetBasicBlockParent(prev);
            
            let predicate = LLVMAppendBasicBlockInContext(self.context, func, b"switch.pred\0".as_ptr() as _);
            let after_block = LLVMAppendBasicBlockInContext(self.context, func, b"switch.merge\0".as_ptr() as _);
            let switch = LLVMAppendBasicBlockInContext(self.context, func, b"switch.body\0".as_ptr() as _);

            LLVMBuildBr(self.builder(), predicate);

            LLVMPositionBuilderAtEnd(self.builder(), predicate);            
            let t_v = Self::translate_expression(self, &stmt.node.expression);
            let predicate = translate_type_cast(
                self,
                t_v.value, 
                &t_v.lang_type, 
                &Type::new_bool(), 
                stmt.node.expression.as_ref()
            );

            todo!()
            // TODO: ближайщий
            // match &stmt.node.statement.node {
            //     Statement::Labeled(labled) => {
            //         match &labled.node.label.node {
            //             Label::Identifier(_) => {}
            //             Label::Case(_) => {}
            //             Label::Default => {}
            //         }
            //     }
            // }

            // let mut prev: Option<llvm::prelude::LLVMBasicBlockRef> = None;
            // for case in stmt.statement.node {
            //     let case_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
            //     llvm::core::LLVMPositionBuilderAtEnd(self.builder(), case_block);

            //     match case {
            //         ast::stmt::Case::Default(default) => {
            //             self.translate(default.body.as_ref());
            //         }
            //         ast::stmt::Case::Pattern(pattern) => {
            //             let pattern_val = Translator::translate_constant(pattern.constant.as_ref());
            //             self.translate(pattern.body.as_ref());
            //             llvm::core::LLVMAddCase(switch, pattern_val, case_block);
            //         }
            //     } 

            //     if prev.is_some() {
            //         llvm::core::LLVMPositionBuilderAtEnd(self.builder(), prev.unwrap());
            //         llvm::core::LLVMBuildBr(self.builder(), case_block);
            //     }
                
                
            //     prev = Some(case_block);
            // }
            // if prev.is_some() {
            //     llvm::core::LLVMBuildBr(self.builder(), after_block);
            // }
        }
    }

    fn translate_return(&mut self, expr: &'a Option<Box<Node<Expression>>>, ret_type: r#type::Type) {
        let mut val: llvm::prelude::LLVMValueRef;
        if expr.is_some() {
            use codespan_reporting::diagnostic::{self, Label};

            if let r#type::Type::Fundamental(r#type::Fundamental::Void) = ret_type {
                self.diagnostics.push_back(Diagnostic::error()
                .with_message("return have expression on function with void return type")
                .with_labels(vec![
                    Label::primary((), expr.as_ref().unwrap().span.start..expr.as_ref().unwrap().span.end)
                ]));
                val = std::ptr::null_mut();
            } else {
                let v = Self::translate_expression(self, expr.as_ref().unwrap());
                let val2 = translate_type_cast(self, v.value, &v.lang_type, &ret_type, expr.as_ref().unwrap());
                val = v.value;
            }

        } else {
            if let r#type::Type::Fundamental(r#type::Fundamental::Void) = ret_type {
                val = std::ptr::null_mut();
            } else {
                unsafe {
                    let r_type = ret_type.translate(self);
                    let v1 = llvm::core::LLVMBuildAlloca(self.builder(), r_type, b"test\0".as_ptr() as _);
                    let v2 = llvm::core::LLVMBuildLoad2(self.builder(), r_type, v1, b"some\0".as_ptr() as _);
                    val = v2;
                }
            }
        }
        
        unsafe {
            LLVMBuildRet(self.builder(), val);
        }
    }

    fn translate_statement(&mut self, stmt: &'a Node<Statement>) {
        match &stmt.node {
            // Statement::Labeled(_) => {}
            Statement::If(stmt) => {
                self.translate_brunch(stmt);
            }
            Statement::Switch(stmt) => {
                self.generate_switch(stmt);
            }
            Statement::While(stmt) => {
                self.generate_while(stmt);
            }
            Statement::DoWhile(stmt) => {
                self.generate_dowhile(stmt);
            }
            Statement::Goto(stmt) => {
                todo!()
            }
            Statement::Continue => {
                //TODO: for, while or do-while
                return;
            }
            Statement::Break => {
                //TODO: for, while or do-while switch 
                return;
            }
            Statement::Return(expr) => {
                self.translate_return(expr, Type::new_signed_int());
            }
            Statement::Asm(_) => unimplemented!(),
            Statement::Labeled(_) => unimplemented!(),
            Statement::Compound(stmt) => {
                self.translate_compound_statement(stmt, Type::new_signed_int());
            },
            Statement::Expression(ref expr) => {
                if expr.is_some() {
                    Self::translate_expression(self, expr.as_ref().unwrap());
                }
            }
            Statement::For(_) => {}
        }
    }

    pub fn translate_compound_statement(&mut self, stmt: &'a Vec<Node<BlockItem>>, ret_type: r#type::Type) {
        let mut iter = stmt.into_iter();
        while let Some(item) = iter.next() {
            match &item.node {
                BlockItem::Declaration(decl) => {
                    self.translate_declaration(decl);
                }
                BlockItem::StaticAssert(node) => {
                    self.check_static_assert(node);
                }
                BlockItem::Statement(node) => {
                    match &node.node {
                        Statement::Continue
                        | Statement::Break 
                        | Statement::Return(_) => {
                            let last = stmt.last().unwrap();
                            if item != last {
                                let next = iter.next().unwrap();
                                let diagnostic = unreachable_after_return(
                                    node, 
                                    next.span.start..last.span.end,
                                );
                                self.add_diagnostic(diagnostic);
                            }

                            self.translate_statement(node);
                            return;
                        },
                        _ => self.translate_statement(node),
                    }
                }
            }
            
        }    
        // self.translate_return(&None, ret_type); 
    }
}

// struct Walker<'ast> {
//     path: Vec<(&'ast usize, usize)>
// }

// impl<'ast> Walker<'ast> {
//     type Item = usize;

//     pub fn path() -> impl Iterator<Item = Self::Item> {

//     }
// }