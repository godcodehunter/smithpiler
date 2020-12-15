use super::{expr::Expression, decl};

// A statement, like int x = 2;
pub enum Statement {
    Compound(CompoundStmt),
    IfElse(IfElseStmt),
    If(IfStmt),
    Goto(String),
    Return(ReturnStmt),
    Break,
    Continue,
    While(Box<WhileStmt>),
    DoWhile(DoWhileStmt),
    For(ForStmt),
    Switch(SwitchStmt),
    Expression(Option<Box<Expression>>),
    Default(Default),
    Case(Case),
    Labeled(Labeled),
}

impl Statement {
    pub fn new_continue() -> Statement {
        Statement::Continue
    }

    pub fn new_return(expr: Expression) -> Statement {
        Statement::Return(expr)
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
        Statement::DoWhile(Box::new(WhileStmt{predicate, body}))
    }

    pub fn new_for(init: Option<ForInit>, predicate: Option<Expression>, step: Option<Expression>, body: Statement) {
        Statement::For(Box::new(ForStmt{init, predicate, step, body}))
    }

    pub fn new_switch(controlling: Expression, body: Statement) {
        Statement::Switch(Box::new(SwitchStmt{controlling, body}))
    }

    pub fn new_expression() -> {
        
    }
}

pub enum BlockItem {
    Statement(Box<Statement>),
    Declaration(Box<decl::Decl>),
} 
// A compound statement consist of a sequence of other statements. 
// Example:
// {
// int x = 2;
// f(x);
// }
pub struct CompoundStmt(pub Vec<BlockItem>);

// if (...) {...} else {...}.
pub struct IfElseStmt {
    pub predicate: Box<Expression>,
    pub first_stmt: Box<Statement>,
    pub second_stmt: Box<Statement>,
}

// if(...) {...}.
pub struct IfStmt {
    pub predicate: Box<Expression>,
    pub first_stmt: Box<Statement>,
}

// return 123;.
pub struct ReturnStmt {
    pub value: Option<Box<Expression>>,
}
// while(...) {...}.
pub struct WhileStmt {
    pub predicate: Expression,
    pub body: Statement,
}

pub struct DoWhileStmt {
    pub predicate: Box<Expression>,
    pub body: Box<Statement>,
}

pub enum ForInit {
    Expression(Expression),
    Declaration(Box<decl::Decl>),
}

pub struct ForStmt {
    pub init: Option<ForInit>,
    pub predicate: Option<Expression>,
    pub step: Option<Expression>,
    pub body: Statement,
}

pub struct Case {
    pub constant: Box<Expression>,
    pub body: Box<Statement>,
}

pub struct Default {
    pub body: Box<Statement>,
}

pub struct SwitchStmt {
    pub controlling: Expression,
    pub body: Statement, 
}

pub struct Labeled {
    pub label: String,
    pub marked: Box<Statement>,
}