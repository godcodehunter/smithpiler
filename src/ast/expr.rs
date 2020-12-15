use super::{r#type::Type};

#[derive(Clone, Eq, PartialEq)]
// Any expression in the C programming language.
pub enum Expression {
    Identifier(String),
    OneOperand(Box<OneOperandExpr>),
    TwoOperands(Box<TwoOperandsExpr>),
    Literal(Literal),
    Call(Box<CallExpr>),
    Cast(Box<CastExr>),
    GenericSelection(Box<GenericSelectionExpr>),
    CompoundLiteral(Box<CompoundLiteral>),
    Ternary(Box<Ternary>),
    Sizeof(Box<Sizeof>),
    AlignOf(AlignOf)
}

#[derive(Clone, Eq, PartialEq)]
pub struct AlignOf {

}

impl AlignOf {
    pub fn new() {

    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct Sizeof {

}

impl Sizeof {
    pub fn new() -> Expression {

    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct Ternary {

}

#[derive(Clone, Eq, PartialEq)]
pub struct CompoundLiteral {
    initializer: Box<decl::InitializerList>,
}

impl CompoundLiteral {
    pub fn new(initializer: Box<decl::InitializerList>) -> Expression {
        Expression::CompoundLiteral(Box::new(Self{initializer}))
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct GenericSelectionExpr {
    expression: Expression,
    association_list: Vec<GenericAssociation>,
}

//TODO: !!!! 
pub struct GenericAssociation {
    expression: Expression,
}

#[derive(Clone, Eq, PartialEq)]
pub struct CastExr {
    pub ty: Box<Type>,
    pub expr: Expression, 
}

impl CastExr {
    pub fn new(ty: Box<Type>, expr: Expression) -> Expression {
        Expression::Cast(Box::new(Self{ty, expr}))
    }
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
    pub value: Expression,
}

impl OneOperandExpr {
    pub fn new_increment(value: Expression) -> Self {
        Self{op: UnaryOp::Increment, value}
    }

    pub fn decrement(value: Expression) -> Self {
        Self{op: UnaryOp::Decrement, value}
    }

    pub fn new_postincrement(value: Expression) -> Self {
        Self{op: UnaryOp::Postincrement, value}
    }

    pub fn new_postdecrement(value: Expression) -> Self {
        Self{op: UnaryOp::Postdecrement, value}
    }
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
    pub lhs: Expression,
    pub op: BiTag,
    pub rhs: Expression,
}

impl TwoOperandsExpr {
    fn as_expr(self) -> Expression {
        Expression::TwoOperands(Box::new(self))
    }
    pub fn new_assign(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::Assign, rhs}.as_expr()
    }

    pub fn new_add_assign(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::AddAssign, rhs}.as_expr()
    }

    pub fn new_sub_assign(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::SubAssign, rhs}.as_expr()
    }

    pub fn new_div_assign(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::DivAssign, rhs}.as_expr()
    }

    pub fn new_rem_assign(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::RemAssign, rhs}.as_expr()
    }

    pub fn new_bw_and_assign(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::BwAndAssign, rhs}.as_expr()
    }

    pub fn new_bw_or_assign(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::BwOrAssign, rhs}.as_expr()
    }

    pub fn new_bw_xor_assign(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::BwXorAssign, rhs}.as_expr()
    }

    pub fn new_bw_left_shift(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::BwXorAssign, rhs}.as_expr()
    }

    pub fn new_bw_right_shift(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::BwXorAssign, rhs}.as_expr()
    }

    pub fn new_left_shift(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::LeftShift, rhs}.as_expr()
    }

    pub fn new_right_shift(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::RightShift, rhs}.as_expr()
    }

    pub fn new_le(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::Less, rhs}.as_expr()
    }

    pub fn new_le_or_eq(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::LessEqual, rhs}.as_expr()
    }

    pub fn new_ge(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::Greater, rhs}.as_expr()
    }

    pub fn new_ge_or_eq(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::GreaterEqual, rhs}.as_expr()
    }

    pub fn new_eq(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::Equal, rhs}.as_expr()
    }

    pub fn new_not_eq(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::NotEqual, rhs}.as_expr()
    }

    pub fn new_add(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::LogicalOr, rhs}.as_expr()
    }

    pub fn new_sub(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::LogicalOr, rhs}.as_expr()
    }

    pub fn new_mul(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::LogicalOr, rhs}.as_expr()
    }

    pub fn new_div(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::LogicalOr, rhs}.as_expr()
    }

    pub fn new_rem(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::LogicalOr, rhs}.as_expr()
    }

    pub fn new_bw_or(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::LogicalOr, rhs}.as_expr()
    }

    pub fn new_bw_and(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::LogicalOr, rhs}.as_expr()
    }

    pub fn new_bw_xor(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::LogicalOr, rhs}.as_expr()
    }

    pub fn new_logical_or(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::LogicalOr, rhs}.as_expr()
    }

    pub fn new_logical_and(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::LogicalAnd, rhs}.as_expr()
    }

    pub fn new_comma(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::Comma, rhs}.as_expr()
    }

    pub fn new_subscript(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::Subscript, rhs}.as_expr()
    }
    
    pub fn new_access(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::Access, rhs}.as_expr()
    }

    pub fn new_ptr_access(lhs: Expression, rhs: Expression) -> Expression {
        Self{lhs, op: BiTag::PtrAccess, rhs}.as_expr()
    }
}

#[derive(Clone, Eq, PartialEq)]
// A tag that determines a binary operator type.
pub enum BiTag {
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
    // %= remaindion assignment
    RemAssign,
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
    // <<
    LeftShift,
    // >>
    RightShift,
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
    Add,
    // - 
    Sub,
    // *
    Mul,
    // /
    Div,
    // %
    Rem,
    // |
    BwOr,
    // &
    BwAnd,
    // ^
    BwXor,
    // ||
    LogicalOr,
    // &&
    LogicalAnd,
    // ,
    Comma,
    // a[12]
    Subscript,
    // a.b
    Access,
    // a->b
    PtrAccess,
}

#[derive(Clone, Eq, PartialEq)]
// Example: f(123, 321).
pub struct CallExpr {
    pub callie: Expression,
    pub args: Vec<Expression>,
}

impl CallExpr {
    pub fn new(callie: Expression, args: Vec<Expression>) -> Expression {
        Expression::Call(Self{callie, args})
    }
}

