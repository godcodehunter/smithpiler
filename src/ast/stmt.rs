use super::{expr::Expression, decl};

// A statement, like int x = 2;
#[derive(Debug)]
pub enum Statement {
    Compound(CompoundStmt),
    IfElse(Box<IfElseStmt>),
    If(Box<IfStmt>),
    Goto(String),
    Return(Box<ReturnStmt>),
    Break,
    Continue,
    While(Box<WhileStmt>),
    DoWhile(Box<DoWhileStmt>),
    For(Box<ForStmt>),
    Switch(Box<SwitchStmt>),
    Expression(Option<Box<Expression>>),
    Default(Box<Default>),
    Case(Box<Case>),
    Labeled(Box<Labeled>),
}

impl Statement {
    pub fn new_continue() -> Statement {
        Statement::Continue
    }

    pub fn new_return(expr: Option<Expression>) -> Statement {
        Statement::Return(Box::new(ReturnStmt{value: expr}))
    }

    pub fn new_break() -> Statement {
        Statement::Break
    }

    pub fn new_goto(label: String) -> Statement {
        Statement::Goto(label)
    }

    pub fn new_while(predicate: Expression, body: Statement) -> Statement {
        Statement::While(Box::new(WhileStmt{predicate, body}))
    }

    pub fn new_do_while(predicate: Expression, body: Statement) -> Statement {
        Statement::DoWhile(Box::new(DoWhileStmt{predicate, body}))
    }

    pub fn new_for(init: Option<ForInit>, predicate: Option<Expression>, step: Option<Expression>, body: Statement) -> Statement {
        Statement::For(Box::new(ForStmt{init, predicate, step, body}))
    }

    pub fn new_switch(controlling: Expression, body: Statement) -> Statement {
        Statement::Switch(Box::new(SwitchStmt{controlling, body}))
    }

    pub fn new_expr_stmt(expression: Expression) -> Statement {
        todo!()
    }
}

#[derive(Debug)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(decl::Declaration),
} 
// A compound statement consist of a sequence of other statements. 
// Example:
// {
// int x = 2;
// f(x);
// }
#[derive(Debug)]
pub struct CompoundStmt(pub Vec<BlockItem>);

impl CompoundStmt {
    pub fn new(items: Vec<BlockItem>) -> Self {
        Self(items)
    }
}

// if (...) {...} else {...}.
#[derive(Debug)]
pub struct IfElseStmt {
    pub predicate: Expression,
    pub on_success: Statement,
    pub on_failure: Statement,
}

impl IfElseStmt {
    pub fn new(predicate: Expression, on_success: Statement, on_failure: Statement) -> Statement {
        Statement::IfElse(Box::new(IfElseStmt{predicate, on_success, on_failure}))
    }
}

// if(...) {...}.
#[derive(Debug)]
pub struct IfStmt {
    pub predicate: Expression,
    pub on_success: Statement,
}

impl IfStmt {
    pub fn new(predicate: Expression, on_success: Statement) -> Statement {
        Statement::If(Box::new(IfStmt{predicate, on_success}))
    }
}

// return 123;.
#[derive(Debug)]
pub struct ReturnStmt {
    pub value: Option<Expression>,
}
// while(...) {...}.
#[derive(Debug)]
pub struct WhileStmt {
    pub predicate: Expression,
    pub body: Statement,
}

#[derive(Debug)]
pub struct DoWhileStmt {
    pub predicate: Expression,
    pub body: Statement,
}

#[derive(Debug)]
pub enum ForInit {
    Expression(Expression),
    Declaration(Box<decl::Declaration>),
}

#[derive(Debug)]
pub struct ForStmt {
    pub init: Option<ForInit>,
    pub predicate: Option<Expression>,
    pub step: Option<Expression>,
    pub body: Statement,
}

#[derive(Debug)]
pub struct Case {
    pub constant: Expression,
    pub body: Statement,
}

impl Case {
    pub fn new(constant: Expression, body: Statement) -> Statement {
        Statement::Case(Box::new(Case{constant, body}))
    }
}

#[derive(Debug)]
pub struct Default {
    pub body: Statement,
}

impl Default {
    pub fn new(body: Statement) -> Statement {
        Statement::Default(Box::new(Default{body}))
    }
}

#[derive(Debug)]
pub struct SwitchStmt {
    pub controlling: Expression,
    pub body: Statement, 
}

#[derive(Debug)]
pub struct Labeled {
    pub label: String,
    pub stmt: Statement,
}

impl Labeled {
    pub fn new(label: String, stmt: Statement) -> Statement {
        Statement::Labeled(Box::new(Labeled{label, stmt}))
    }
}