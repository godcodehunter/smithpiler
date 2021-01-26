use super::*;

// fn pprint_tree(node: &Node) {
//     fn pprint_tree(node: &Node, prefix: String, last: bool) {
//         let prefix_current = if last { "`- " } else { "|- " };

//         println!("{}{}{}", prefix, prefix_current, node);

//         let prefix_child = if last { "   " } else { "|  " };
//         let prefix = prefix + prefix_child;

//         if !node.children.is_empty() {
//             let last_child = node.children.len() - 1;

//             for (i, child) in node.children.iter().enumerate() {
//                 pprint_tree(&child, prefix.to_string(), i == last_child);
//             }
//         }
//     }

//     pprint_tree(node, "".to_string(), true);
// }

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for item in &self.0 {
            item.fmt(f)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for ExternalDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExternalDeclaration::FunctionDefinition(fd) => { fd.fmt(f) }
            ExternalDeclaration::Declaration(decl) => { decl.fmt(f) }
        }    
    }
}

impl std::fmt::Display for FunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.compound.fmt(f)
    }
}

impl std::fmt::Display for decl::Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl std::fmt::Display for stmt::CompoundStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for item in &self.0 {
            f.write_fmt(format_args!("{}", item))?;
        }
        Ok(())
    }
}

impl std::fmt::Display for stmt::BlockItem { 
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            stmt::BlockItem::Statement(stmt) => { stmt.fmt(f) }
            stmt::BlockItem::Declaration(decl) => { decl.fmt(f) }
        }
    }
}

impl std::fmt::Display for stmt::Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            stmt::Statement::Compound(_) => {}
            stmt::Statement::IfElse(_) => {}
            stmt::Statement::If(_) => {}
            stmt::Statement::Goto(gt) => { f.write_str(gt)?; }
            stmt::Statement::Return(ret) => { 
                let val = &ret.as_ref().value;
                if val.is_some() {
                    //TODO: val.unwrap().fmt(f)?;
                }
            }
            stmt::Statement::Break => { f.write_str("break")?; }
            stmt::Statement::Continue => { f.write_str("continue")?; }
            stmt::Statement::While(_) => { f.write_str("while")?; }
            stmt::Statement::DoWhile(_) => { f.write_str("do_while")?; }
            stmt::Statement::For(fl) => { 
                f.write_str("for")?;
                let expr = fl.as_ref();
                // expr.init;
                // expr.predicate;
                // expr.step;
                // expr.body;
            }
            stmt::Statement::Switch(sw) => {}
            stmt::Statement::Expression(expr) => {}
            stmt::Statement::Default(_) => {}
            stmt::Statement::Case(_) => {}
            stmt::Statement::Labeled(_) => {}
        }
        Ok(())
    }
}