use super::expr::Expr;

// Any type in the C programming language.
#[derive(Clone, Eq, PartialEq)]
pub enum Type {
    Fundamental(Fundamental),
    Enumerated(EnumeratedType),
    Derived(DerivedType),
}

impl Type {
    pub fn is_integer(&self) -> bool {
        match self {
            Type::Fundamental(fund) => {
                match fund {
                    Fundamental::SignedInteger(_) => true,
                    Fundamental::UnsignedInteger(_) => true,
                    _ => false,
                }
            }
            _ => false
        }
    }

    pub fn is_real(&self) -> bool {
        todo!()
    }

    pub fn is_signed(&self) -> bool {
        todo!()
    }
}

#[derive(Clone, Eq, PartialEq)]
pub enum Fundamental {
    Bool,
    Void,
    SignedInteger(SignedIntegerType),
    UnsignedInteger(UnsignedIntegerType),
    Floating(FloatingType),
}

#[derive(Clone, Eq, PartialEq)]
// All signed integers.
pub enum SignedIntegerType {
    SignedChar,
    ShortInt,
    Int,
    LongInt,
    LongLongInt,
}

#[derive(Clone, Eq, PartialEq)]
// All unsigned integers.
pub enum UnsignedIntegerType {
    UnsignedChar,
    UnsignedShort,
    UnsignedInt,
    UnsignedLong,
    UnsignedLongLong,
}

#[derive(Clone, Eq, PartialEq)]
// All floating point types.
pub enum FloatingType {
    Float,
    Double,
    LongDouble,
}

#[derive(Clone, Eq, PartialEq)]
// enum { ... } Abc;.
pub struct EnumeratedType {
    variants: Vec<EnumeratedTypeVariant>,
}

#[derive(Clone, Eq, PartialEq)]
// A variant name with a constant integer expression.
pub struct EnumeratedTypeVariant {
    id: String,
    constant: Expr,
}

#[derive(Clone, Eq, PartialEq)]
// A user-defined data type.
pub enum DerivedType {
    Array(ArrayType),
    Structure(StructureType),
    Union(UnionType),
    Function(FunctionType),
    Pointer(PointerType),
}

#[derive(Clone, Eq, PartialEq)]
pub struct ArrayType {
    pub elem_type: Box<Type>,
    pub size: u32,
}

#[derive(Clone, Eq, PartialEq)]
pub struct StructureType {
    pub struct_name: String,
    pub fields: Vec<EnumeratedTypeVariant>,
}

#[derive(Clone, Eq, PartialEq)]
pub struct UnionType {
    pub union_name: String,
    pub fields: Vec<EnumeratedTypeVariant>,
}

#[derive(Clone, Eq, PartialEq)]
pub struct FunctionType {
    pub identifier: String,
    pub returned_type: Box<Type>,
    pub params: Vec<Type>,
}

#[derive(Clone, Eq, PartialEq)]
pub struct PointerType(pub Box<Type>);

