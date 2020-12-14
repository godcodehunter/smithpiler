use super::{expr::Expr, decl};

// A statement, like int x = 2;
pub enum Statement {
    Compound(CompoundStmt),
    IfElse(IfElseStmt),
    If(IfStmt),
    Goto(String),
    Return(ReturnStmt),
    Break,
    Continue,
    While(WhileStmt),
    DoWhile(DoWhileStmt),
    For(ForStmt),
    Switch(SwitchStmt),
    Expression(Option<Box<Expr>>),
    Default(Default),
    Case(Case),
    Labeled(Labeled),
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
    pub predicate: Box<Expr>,
    pub first_stmt: Box<Statement>,
    pub second_stmt: Box<Statement>,
}

// if(...) {...}.
pub struct IfStmt {
    pub predicate: Box<Expr>,
    pub first_stmt: Box<Statement>,
}

// return 123;.
pub struct ReturnStmt {
    pub value: Option<Box<Expr>>,
}
// while(...) {...}.
pub struct WhileStmt {
    pub predicate: Box<Expr>,
    pub body: Box<Statement>,
}

pub struct DoWhileStmt {
    pub predicate: Box<Expr>,
    pub body: Box<Statement>,
}

pub enum ForInit {
    Expression(Box<Expr>),
    Declaration(Box<decl::Decl>),
}

pub struct ForStmt {
    pub init: Option<ForInit>,
    pub predicate: Option<Box<Expr>>,
    pub step: Option<Box<Expr>>,
    pub body: Box<Statement>,
}

pub struct Case {
    pub constant: Box<Expr>,
    pub body: Box<Statement>,
}

pub struct Default {
    pub body: Box<Statement>,
}

pub struct SwitchStmt {
    pub controlling: Box<Expr>,
    pub body: Box<Statement>, 
}

pub struct Labeled {
    pub label: String,
    pub marked: Box<Statement>,
}