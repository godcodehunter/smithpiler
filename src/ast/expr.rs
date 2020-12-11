use super::{r#type::Type, decl::VarDecl, decl::FuncDecl};

#[derive(Clone, Eq, PartialEq)]
// Any expression in the C programming language.
pub enum Expr {
    Identifier(String),
    OneOperand(OneOperandExpr),
    TwoOperands(TwoOperandsExpr),
    Literal(Literal),
    Call(CallExpr),
    Assign(AssignExpr),
    Cast(CastExr),
}

#[derive(Clone, Eq, PartialEq)]
pub struct CastExr {
    pub ty: Box<Type>,
    pub expr: Box<Expr>, 
}

#[derive(Clone, Eq, PartialEq)]
// A literal expression. For example, "hello world", 141, 666.
pub enum Literal {
    Char(CharLiteral),
    Integer(IntegerLiteral),
    Str(StringLiteral),
    Float(FloatLiteral),
}

#[derive(Clone, Eq, PartialEq)]
pub enum CharLiteralPrefix {
    L,
    LCU,
    UCU,
}

#[derive(Clone, Eq, PartialEq)]
pub struct CharLiteral {
    pub prefix: CharLiteralPrefix,
    pub value: u8,
}

#[derive(Clone, Eq, PartialEq)]
pub enum StringLiteralPrefix {
    U8,
    LCU,
    UCU,
    L,
}

#[derive(Clone, Eq, PartialEq)]
pub struct StringLiteral {
    pub prefix: StringLiteralPrefix,
    pub value: String,
}

#[derive(Clone, Eq, PartialEq)]
enum IntegerSuffix {
    U,
    L,
    UL,
    LL,
    ULL,
}

#[derive(Clone, Eq, PartialEq)]
pub enum FloatLiteralPrefix {
    F,
    L
}

#[derive(Clone, Eq, PartialEq)]
pub struct FloatLiteral {
    pub prefix: FloatLiteralPrefix,
    // pub value: f64, TODO: !!!! 
}

#[derive(Clone, Eq, PartialEq)]
pub enum IntegerLiteralRepresentationKind {
    DecimalConstant,
    OctalConstant,
    HexadecimalConstant,
}

#[derive(Clone, Eq, PartialEq)]
pub struct IntegerLiteral {
    pub suffix: Option<IntegerSuffix>, 
    pub representation_kind: IntegerLiteralRepresentationKind,
    pub value: u64, 
}

#[derive(Clone, Eq, PartialEq)]
// An expression that has an unary operator with an operand. Example: +1, !(true && false).
pub struct OneOperandExpr {
    pub op: UnaryOp,
    pub value: Box<Expr>,
}

#[derive(Clone, Eq, PartialEq)]
pub enum UnaryOp {
    Neg,
    Sizeof,
    LogicalNot,
    BitwiseNot,
    Increment,
    Postincrement,
    Decrement,
    Postdecrement,
}

#[derive(Clone, Eq, PartialEq)]
// An expression that has a binary operator with two operands. Example: 1 < 12, 24 != (14 + 14).
pub struct TwoOperandsExpr {
    pub lhs: Box<Expr>,
    pub op: BinOp,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Eq, PartialEq)]
// A tag that determines a binary operator type.
pub enum BinOp {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    Plus,
    Minus,
    Multiply,
    Divide,
    LogicalOr,
    LogicalAnd,
    MemberAccess,
}

#[derive(Clone, Eq, PartialEq)]
// Example: f(123, 321).
pub struct CallExpr {
    pub args: Vec<Expr>,
    pub callie: Box<Expr>,
}

#[derive(Clone, Eq, PartialEq)]
// y = 2.
pub struct AssignExpr {
    pub declaration: Box<VarDecl>,
    pub rhs: Box<Expr>,
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[derive(Debug, Eq, PartialEq)]
//     struct Node<'a>(&'a Expr, &'a [Node<'a>]);

//     #[test]
//     fn bfs_works() {
//         let abc = Expr::Var(VarExpr {
//             var_name: "abc".to_owned(),
//         });
//         let def = Expr::Var(VarExpr {
//             var_name: "def".to_owned(),
//         });
//         let ghi = Expr::Var(VarExpr {
//             var_name: "ghi".to_owned(),
//         });
        
//         let tree = [Node(&def, &[]), Node(&ghi, &[])];

//         let iter = Bft::new(&tree, |node| node.1.iter());
//         let mut iter = iter.map(|(depth, node)| (depth, node.0));

//         assert_eq!(iter.next(), Some((0, &abc)));
//         assert_eq!(iter.next(), Some((1, &def)));
//         assert_eq!(iter.next(), Some((1, &ghi)));
//         assert_eq!(iter.next(), None);
//     }
// }
