use super::{r#type::Type};

#[derive(Clone, Eq, PartialEq)]
// Any expression in the C programming language.
pub enum Expr {
    Identifier(String),
    OneOperand(OneOperandExpr),
    TwoOperands(TwoOperandsExpr),
    Literal(Literal),
    Call(CallExpr),
    Cast(CastExr),
    GenericSelection(GenericSelectionExpr),
}

#[derive(Clone, Eq, PartialEq)]
pub struct GenericSelectionExpr {
    expression: Box<Expr>,
    association_list: Vec<GenericAssociation>,
}

//TODO: !!!! 
pub struct GenericAssociation {
    expression: Box<Expr>,
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CharLiteralPrefix {
    L,
    LCU,
    UCU,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CharLiteral {
    pub prefix: Option<CharLiteralPrefix>,
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
    pub prefix: Option<StringLiteralPrefix>,
    pub value: String,
}

#[derive(Clone, Eq, PartialEq)]
pub enum IntegerSuffix {
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
    //&x
    Address,
    //*x
    Indirection,
    //+x
    Positive,
    //-x
    Negative,
    //sizeof
    Sizeof,
    //!
    LogicalNot,
    //~
    BitwiseNot,
    //++x
    Increment,
    //x++
    Postincrement,
    //--x
    Decrement,
    //x--
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
    // = basic assignment
    Assign,
    // += addition assignment
    AddAssign,
    // -= subtraction assignment
    SubAssign,
    // *= multiplication assignment	
    MulAssign,
    // /= division assignment
    DivAssign,
    // %= modulo assignment
    ModAssign,
    // &= bitwise AND assignment
    BwAndAssign,
    // |= bitwise OR assignment	
    BwOrAssign,
    // ^= bitwise XOR assignment	
    BwXorAssign,
    // <<= bitwise left shift assignment
    BwLShAssign,
    // >>= bitwise right shift assignment
    BwRShAssign,
    // <
    Less,
    // <=
    LessEqual,
    // >
    Greater,
    // >=
    GreaterEqual,
    // ==
    Equal,
    // !=
    NotEqual,
    // +
    Plus,
    // - 
    Minus,
    // *
    Multiply,
    // /
    Divide,
    // 
    LogicalOr,
    //
    LogicalAnd,
    //
    MemberAccess,
    //
    Comma,
}

#[derive(Clone, Eq, PartialEq)]
// Example: f(123, 321).
pub struct CallExpr {
    pub args: Vec<Expr>,
    pub callie: Box<Expr>,
}

