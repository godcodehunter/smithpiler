use super::*;

// Provide iterator over neighbors 
trait Traversable<'a> {
    type Item;
    type Iter: Iterator<Item = &'a Self::Item>;
    fn traverse(self) -> Self::Iter;
}

trait NodeDataDisplay {
    fn fmt(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

fn pprint_tree<'a, T>(node: &'a T) 
where T: Traversable<'a> + std::fmt::Display {
    fn pprint_tree<'b, T>(node: &'b T, prefix: String, last: bool) 
    where T: Traversable<'b> + std::fmt::Display {
        let prefix_current = if last { "`- " } else { "|- " };

        println!("{}{}{}", prefix, prefix_current, node);

        let prefix_child = if last { "   " } else { "|  " };
        let prefix = prefix + prefix_child;

        if !node.traverse().next().is_some() {
        let last_child = node.traverse().count() - 1;

            for (i, child) in node.traverse().enumerate() {
                pprint_tree(&child, prefix.to_string(), i == last_child);
            }
        }
    }

    pprint_tree(node, "".to_string(), true);
}

impl<'a> Traversable<'a> for &'a Ast {
    type Item = ExternalDeclaration;
    type Iter = std::slice::Iter<'a, Self::Item>;

    fn traverse(self) -> Self::Iter {
        self.0.iter()
    }
}

impl NodeDataDisplay {
    fn fmt(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Ast\n")?;
    }
}

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        pprint_tree(&self);
        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////
impl std::fmt::Display for ExternalDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExternalDeclaration::FunctionDefinition(fd) 
                => { fd.fmt(f) }
            ExternalDeclaration::Declaration(decl) 
                => { decl.fmt(f) }
        }    
    }
}

impl std::fmt::Display for FunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("FunctionDefinition\n")?;
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
        f.write_str("CompoundStmt\n")?;
        pprint_tree(self.0);
        Ok(())
    }
}

impl std::fmt::Display for stmt::BlockItem { 
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            stmt::BlockItem::Statement(stmt) 
                => { stmt.fmt(f) }
            stmt::BlockItem::Declaration(decl) 
                => { decl.fmt(f) }
        }
    }
}

impl std::fmt::Display for stmt::Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            stmt::Statement::Compound(com) 
                => { com.fmt(f)?; }
            stmt::Statement::IfElse(ife) => { 
                f.write_str("IfElse")?;
                pprint_tree(ife);
            }
            stmt::Statement::If(i) 
                => { i.fmt(f)?; }
            stmt::Statement::Goto(gt) 
                => { f.write_str("Goto")?; }
            stmt::Statement::Return(ret) => { 
                let val = &ret.as_ref().value;
                if val.is_some() {
                    //TODO: val.unwrap().fmt(f)?;
                }
            }
            stmt::Statement::Break 
                => { f.write_str("Break")?; }
            stmt::Statement::Continue 
                => { f.write_str("Continue")?; }
            stmt::Statement::While(wh) => { 
                f.write_str("While")?; 
                pprint_tree(wh);
            }
            stmt::Statement::DoWhile(wh) => { 
                f.write_str("DoWhile")?; 
                pprint_tree(wh);
            }
            stmt::Statement::For(fl) => { 
                f.write_str("For")?;
                let expr = fl.as_ref();
                // expr.init;
                // expr.predicate;
                // expr.step;
                // expr.body;
            }
            stmt::Statement::Switch(sw) => {
                f.write_str("Switch")?;
                pprint_tree(sw);
            }
            stmt::Statement::Expression(expr) => {

            }
            stmt::Statement::Default(_) => {

            }
            stmt::Statement::Case(cs) => {

            }
            stmt::Statement::Labeled(labeled) => {

            }
        }
        Ok(())
    }
}