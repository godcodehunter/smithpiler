use crate::{expression::*, translator::*};
use lang_c::{ast::*, span::Node};
use crate::r#type;
use std::collections::HashMap;
use crate::diagnostics::*;
use crate::type_cast::*;
use crate::translator::*;

impl<'a> Translator<'a> {
    /// Generate 'if' brunch or 'if-else', if 'on_false_stmt' present   
    fn translate_brunch(&mut self, stmt: &'a  Node<IfStatement>) {
        // unsafe {
        //     let predicate = self.expr_trans().translate(stmt.node.condition.as_ref()); 
        //     let after_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
        //     let on_true_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
        //     let on_false_block = if stmt.node.else_statement.is_some() {
        //         llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB)
        //     } else {
        //         after_block
        //     };
                
        //     llvm::core::LLVMBuildCondBr(self.builder(), predicate, on_true_block, on_false_block);
        //     llvm::core::LLVMPositionBuilderAtEnd(self.builder(), on_true_block);
        //     self.translate_statement(stmt.node.then_statement.as_ref());
        //     llvm::core::LLVMBuildBr(self.builder(), after_block);
            
        //     if stmt.node.else_statement.is_some() {
        //         llvm::core::LLVMPositionBuilderAtEnd(self.builder(), on_false_block);
        //         self.translate_statement(stmt.node.else_statement.as_ref().unwrap());
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
        //     let predicate = self.expr_trans().translate(stmt.node.expression.as_ref());
        //     llvm::core::LLVMBuildCondBr(self.builder(), predicate, body_block, after_block);
            
        //     llvm::core::LLVMPositionBuilderAtEnd(self.builder(), body_block);
        //     self.translate_statement(stmt.node.statement.as_ref());
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
        //     self.translate_statement(stmt.node.statement.as_ref());
        //     let predicate = self.expr_trans().translate(stmt.node.expression.as_ref());  
        //     llvm::core::LLVMBuildCondBr(self.builder(), predicate, body_block, after_block);
        // }
        todo!()
    }

    fn generate_for(&mut self, stmt: &'a Node<ForStatement>) {
        // unsafe {
        //     let predicate_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
        //     let body_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);
        //     let after_block = llvm::core::LLVMCreateBasicBlockInContext(self.context(), NOP_STUB);

        //     self.expr_trans().translate(stmt.node.initializer);                
        //     llvm::core::LLVMBuildBr(self.builder(), predicate_block);  
        //     llvm::core::LLVMPositionBuilderAtEnd(self.builder(), predicate_block);
        //     let predicate = self.expr_trans().translate(stmt.node.condition.as_ref());
        //     llvm::core::LLVMBuildCondBr(self.builder(), predicate, body_block, after_block);
        //     llvm::core::LLVMPositionBuilderAtEnd(self.builder(), body_block);
        //     self.translate_statement(stmt.node.statement.as_ref());    
        //     self.expr_trans().translate(stmt.node.step.as_ref());
        // }
        todo!()
    }

    fn generate_switch(&mut self, stmt: &'a Node<SwitchStatement>) {
        // unsafe {
        //     let predicate = self.expr_trans().translate(stmt.node.expression.as_ref());
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
        todo!()
    }

    pub fn translate_statement(&mut self, stmt: &'a Node<Statement>, ret_type: r#type::Type) {
        match &stmt.node {
            Statement::Compound(block) => {
                let mut is_has_return = false;
                
                let mut iter = block.into_iter();
                while let Some(item) = iter.next() {
                    match &item.node {
                        BlockItem::Declaration(decl) => {
                            self.translate_declaration(decl);
                        }
                        BlockItem::StaticAssert(_) => {
                            todo!()
                        }
                        BlockItem::Statement(stmt) => {
                            let last = block.last().unwrap();
                            match &stmt.node {
                                Statement::Return(_) if item != last => {
                                    is_has_return = true;

                                    let diagnostic = unreachable_after_return(
                                        stmt, 
                                        iter.next().unwrap().span.start..last.span.end);
                                    self.add_diagnostic(diagnostic);
                                    break;
                                },
                                Statement::Return(expr) => {
                                    is_has_return = true;
                                    unsafe {
                                        let mut val: TranslatedValue;
                                        if expr.is_some() {
                                            val = self.translate_expression(expr.as_ref().unwrap());
                                        } else {
                                            todo!();
                                        }
                                        let val2 = translate_type_cast(self, val.value, &val.lang_type, &ret_type, expr.as_ref().unwrap());
                                        llvm::core::LLVMBuildRet(self.builder(), val2);
                                    }
                                },
                                Statement::Expression(Some(ref expr)) => {
                                    self.translate_expression(expr);
                                },
                                _ => todo!()
                            }
                        }
                    } 
                }
                if !is_has_return {
                    unsafe {
                        let r_type = ret_type.translate(self);
                        let value = llvm::core::LLVMBuildAlloca(self.builder(), r_type, b"test\0".as_ptr() as _);
                        let value2 = llvm::core::LLVMBuildLoad2(self.builder(), r_type, value, b"some\0".as_ptr() as _);
                        llvm::core::LLVMBuildRet(self.builder(), value2);
                    }
                }
            }
            // Statement::Labeled(_) => {}
            // Statement::If(stmt) => {
            //     self.translate_brunch(stmt);
            // }
            // Statement::Switch(stmt) => {
            //     self.generate_switch(stmt);
            // }
            // Statement::While(stmt) => {
            //     self.generate_while(stmt)
            // }
            // Statement::DoWhile(stmt) => {
            //     self.generate_dowhile(stmt);
            // }
            // Statement::For(stmt) => {
            //     self.generate_for(stmt);
            // }
            // Statement::Goto(_) => {
            //     //TODO: for, while or do-while
            //     todo!() 
            // }
            // Statement::Continue => {
            //     //TODO: for, while or do-while
            //     todo!() 
            // }
            // Statement::Break => {
            //     //TODO: for, while or do-while switch 
            //     todo!() 
            // }
            // Statement::Asm(_) => {}
            _ => todo!()
        }
    }
}