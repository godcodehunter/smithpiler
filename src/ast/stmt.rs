use super::expr::Expr;

// A statement, like int x = 2;
pub enum Statement {
    Compound(CompoundStmt),
    IfElse(IfElseStmt),
    If(IfStmt),
    Return(ReturnStmt),
    Break,
    Continue,
    While(WhileStmt),
    DoWhile(DoWhileStmt),
    For(ForStmt),
    Switch(SwitchStmt),
    Expression(Expr),
}

// A compound statement consist of a sequence of other statements. 
// Example:
// {
// int x = 2;
// f(x);
// }
pub struct CompoundStmt(pub Vec<Statement>);

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
    pub value: Box<Expr>,
}
// while(...) {...}.
pub struct WhileStmt {
    pub predicate: Box<Expr>,
    pub stmt: Box<Statement>,
}

pub struct DoWhileStmt {
    pub predicate: Box<Expr>,
    pub body: Box<Statement>,
}

pub struct ForStmt {
    pub init: Box<Expr>,
    pub predicate: Box<Expr>,
    pub step: Box<Expr>,
    pub body: Box<Statement>,
}

pub struct Pattern {
    pub constant: Box<Expr>,
    pub body: Box<Statement>,
}

pub struct Default {
    pub body: Box<Statement>,
}

pub enum Case {
    Default(Default),
    Pattern(Pattern),
}

pub struct SwitchStmt {
    pub value: Box<Expr>,
    pub cases: Vec<Case>, 
}