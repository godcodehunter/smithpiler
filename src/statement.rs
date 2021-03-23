use crate::{expression::*, translator::*};
use lang_c::{ast::*, span::Node};
use crate::r#type;
use std::collections::HashMap;
use crate::diagnostics::*;
use crate::type_cast::*;
use crate::translator::*;
use llvm::core::*;
use crate::r#type::Type;

struct StatementTranslator<'ast> {
    closes_loop: &'ast Node<Statement>,
}

impl<'a> Translator<'a> {
    /// Generate 'if' brunch or 'if-else', if 'on_false_stmt' present   
    fn translate_brunch(&mut self, stmt: &'a  Node<IfStatement>) {
        unsafe {
            let t_v = Self::translate_expression(self, stmt.node.condition.as_ref());
            let predicate = translate_type_cast(
                self,
                t_v.value, 
                &t_v.lang_type, 
                &Type::new_bool(), 
                stmt.node.condition.as_ref()
            );
            let after_block = LLVMCreateBasicBlockInContext(self.context, NOP_STUB);
            LLVMInsertExistingBasicBlockAfterInsertBlock(self.builder(), after_block);
            let on_true_block = LLVMCreateBasicBlockInContext(self.context, b"on_true\0".as_ptr() as _);
            LLVMInsertExistingBasicBlockAfterInsertBlock(self.builder(), on_true_block);
            let on_false_block = if stmt.node.else_statement.is_some() {
               let v = LLVMCreateBasicBlockInContext(self.context, b"on_false\0".as_ptr() as _);
               LLVMInsertExistingBasicBlockAfterInsertBlock(self.builder(), v);
               v
            } else {
                after_block
            };
                
            LLVMBuildCondBr(self.builder(), predicate, on_true_block, on_false_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), on_true_block);
            self.translate_statement(stmt.node.then_statement.as_ref());
            LLVMBuildBr(self.builder(), after_block);
            
            if stmt.node.else_statement.is_some() {
                LLVMPositionBuilderAtEnd(self.builder(), on_false_block);
                self.translate_statement(stmt.node.else_statement.as_ref().unwrap());
                LLVMBuildBr(self.builder(), after_block);
            }
            
            LLVMPositionBuilderAtEnd(self.builder(), after_block);
        }
    }

    fn generate_while(&mut self, stmt: &'a Node<WhileStatement>) {
        unsafe {
            let prev = LLVMGetInsertBlock(self.builder());
            let func = LLVMGetBasicBlockParent(prev);

            let predicate_block = LLVMAppendBasicBlockInContext(self.context, func, b"loop_predicate\0".as_ptr() as _);
            let body_block = LLVMAppendBasicBlockInContext(self.context, func, b"loop_body\0".as_ptr() as _); 
            let after_block = LLVMAppendBasicBlockInContext(self.context, func, b"after_loop\0".as_ptr() as _); 

            LLVMBuildBr(self.builder(), predicate_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), predicate_block);
            let t_v = Self::translate_expression(self, stmt.node.expression.as_ref());
            let predicate = translate_type_cast(
                self,
                t_v.value, 
                &t_v.lang_type, 
                &Type::new_bool(), 
                stmt.node.expression.as_ref()
            );
            LLVMBuildCondBr(self.builder(), predicate, body_block, after_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), body_block);
            self.translate_statement(stmt.node.statement.as_ref());
            LLVMBuildBr(self.builder(), after_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), after_block);
        }
    }

    fn generate_dowhile(&mut self, stmt: &'a Node<DoWhileStatement>) {
        unsafe {
            let prev = LLVMGetInsertBlock(self.builder());
            let func = LLVMGetBasicBlockParent(prev);

            let body_block = LLVMAppendBasicBlockInContext(self.context, func, b"loop_body\0".as_ptr() as _); 
            let predicate_block = LLVMAppendBasicBlockInContext(self.context, func, b"loop_predicate\0".as_ptr() as _);
            let after_block = LLVMAppendBasicBlockInContext(self.context, func, b"after_loop\0".as_ptr() as _); 
            
            LLVMBuildBr(self.builder(), body_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), body_block);
            self.translate_statement(stmt.node.statement.as_ref());
            LLVMBuildBr(self.builder(), predicate_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), predicate_block);
            let t_v = Self::translate_expression(self, stmt.node.expression.as_ref());
            let predicate = translate_type_cast(
                self,
                t_v.value, 
                &t_v.lang_type, 
                &Type::new_bool(), 
                stmt.node.expression.as_ref()
            );
            LLVMBuildCondBr(self.builder(), predicate, body_block, after_block);
            
            LLVMPositionBuilderAtEnd(self.builder(), after_block);
        }
    }

    fn generate_for(&mut self, stmt: &'a Node<ForStatement>) {
        unsafe {
            let prev = LLVMGetInsertBlock(self.builder());
            let func = LLVMGetBasicBlockParent(prev);

            let initializer = LLVMAppendBasicBlockInContext(self.context, func, b"initializer_loop\0".as_ptr() as _);
            let predicate_block = LLVMAppendBasicBlockInContext(self.context, func, b"loop_predicate\0".as_ptr() as _);
            let step = LLVMAppendBasicBlockInContext(self.context, func, b"loop_step\0".as_ptr() as _);
            let body_block = LLVMAppendBasicBlockInContext(self.context, func, b"loop_body\0".as_ptr() as _); 
            let after_block = LLVMAppendBasicBlockInContext(self.context, func, b"after_loop\0".as_ptr() as _); 

            LLVMBuildBr(self.builder(), initializer);

            LLVMPositionBuilderAtEnd(self.builder(), initializer);
            match &stmt.node.initializer.node {
                ForInitializer::Empty => {}
                ForInitializer::Expression(expr) => {
                    let t_v = Self::translate_expression(self, &expr);
                    let predicate = translate_type_cast(
                    self,
                    t_v.value, 
                    &t_v.lang_type, 
                    &Type::new_bool(), 
                    stmt.node.condition.as_ref().unwrap()
                    ); 
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
            
            let predicate = LLVMAppendBasicBlockInContext(self.context, func, b"predicate\0".as_ptr() as _);
            let after_block = LLVMAppendBasicBlockInContext(self.context, func, b"after_block\0".as_ptr() as _);
            let switch = LLVMAppendBasicBlockInContext(self.context, func, b"switch\0".as_ptr() as _);

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
            llvm::core::LLVMBuildRet(self.builder(), val);
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
                todo!() 
            }
            Statement::Break => {
                //TODO: for, while or do-while switch 
                todo!() 
            }
            Statement::Asm(_) => {}
            Statement::Labeled(_) => {}
            Statement::Compound(_) => {}
            Statement::Expression(ref expr) => {
                if expr.is_some() {
                    Self::translate_expression(self, expr.as_ref().unwrap());
                }
            }
            Statement::For(_) => {}
            Statement::Return(_) => {}
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
                        Statement::Return(expr) => {
                            let last = stmt.last().unwrap();
                            if item != last {
                                let next = iter.next().unwrap();
                                let diagnostic = unreachable_after_return(
                                    node, 
                                    next.span.start..last.span.end,
                                );
                                self.add_diagnostic(diagnostic);
                            }

                            self.translate_return(expr, ret_type);
                            return;
                        },
                        Statement::Expression(Some(ref expr)) => {
                            Self::translate_expression(self, expr);
                        },
                        Statement::For(stmt) => {
                            self.generate_for(stmt);
                        },
                        _ => self.translate_statement(node),
                    }
                }
            }
        }    
        self.translate_return(&None, ret_type); 
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