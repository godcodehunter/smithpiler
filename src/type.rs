// Any type in the C programming language.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Fundamental(Fundamental),
    // Enumerated(EnumeratedType),
    Derived(DerivedType),
}

impl Type {
    pub fn new_bool() -> Self {
        Type::Fundamental(Fundamental::Bool)
    }

    pub fn new_void() -> Self {
        Type::Fundamental(Fundamental::Void)
    }

    pub fn new_signed_char() -> Self {
        todo!()
    }

    pub fn new_unsigned_char() -> Self {
        Type::Fundamental(Fundamental::UnsignedInteger(UnsignedIntegerType::UnsignedChar))
    }

    pub fn new_signed_short_int() -> Self {
        todo!()
    }

    pub fn new_unsigned_short_int() -> Self {
        Type::Fundamental(Fundamental::UnsignedInteger(UnsignedIntegerType::UnsignedShort))
    }

    pub fn new_signed_int() -> Self {
        Type::Fundamental(Fundamental::SignedInteger(SignedIntegerType::ShortInt))
    }

    pub fn new_signed_long_int() -> Self {
        todo!()
    }

    pub fn new_signed_long_long_int() -> Self {
        todo!()
    }

    pub fn new_float() -> Self {
        Type::Fundamental(Fundamental::Floating(FloatingType::Float))
    }

    pub fn stringify(&self) -> &'static str {
        todo!()
    }

    pub fn rank(&self) -> u8 {
        match self {
            Type::Fundamental(v) => v.rank(),
            _ => panic!()
            // Type::Enumerated(v) => v.rank(),
            // Type::Derived(v) => v.rank(),
        }
    }
    
    pub fn new_function<I: IntoIterator<Item = Type>>(ret: Type, params: I, is_variadic: bool) -> Self {
        Type::Derived(DerivedType::Function(FunctionType{
            returned_type: ret.into(),
            params: params.into_iter().collect(),
            is_var: is_variadic,
        }))
    }

    pub fn is_fundamental(&self) -> bool {
        true
    }
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
        match self {
            Type::Fundamental(v) => {
                match v {
                    Fundamental::Floating(_) => true,
                    _ => false,
                }
            },
            _ => panic!()
        }
    }

    pub fn is_signed(&self) -> bool {
        todo!()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Fundamental {
    Bool,
    Void,
    SignedInteger(SignedIntegerType),
    UnsignedInteger(UnsignedIntegerType),
    Floating(FloatingType),
}

impl Into<&str> for Fundamental {
    fn into(self) -> &'static str {
        match self {
            Fundamental::Bool => "bool",
            Fundamental::Void => "void",
            Fundamental::SignedInteger(v) => v.into(),
            Fundamental::UnsignedInteger(v) => v.into(),
            Fundamental::Floating(v) => v.into(),
        }
    }
}

impl Fundamental {
    pub fn rank(&self) -> u8 {
        match self {
            Fundamental::Bool => 1,
            Fundamental::Void => unimplemented!("void has no rank"),
            Fundamental::SignedInteger(v) => v.rank(), 
            Fundamental::UnsignedInteger(v) => v.rank(),
            Fundamental::Floating(v) => v.rank(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
// All signed integers.
pub enum SignedIntegerType {
    SignedChar,
    ShortInt,
    Int,
    LongInt,
    LongLongInt,
}

impl SignedIntegerType {
    pub fn rank(&self) -> u8 {
        match self {
            SignedIntegerType::SignedChar => 2,
            SignedIntegerType::ShortInt => 3,
            SignedIntegerType::Int => 4,
            SignedIntegerType::LongInt => 5,
            SignedIntegerType::LongLongInt => 6,
        }
    }
}

impl Into<&str> for SignedIntegerType {
    fn into(self) -> &'static str {
        match self {
            SignedIntegerType::SignedChar => "signed char",
            SignedIntegerType::ShortInt => "short int",
            SignedIntegerType::Int => "int",
            SignedIntegerType::LongInt => "long int",
            SignedIntegerType::LongLongInt => "long long int",
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
// All unsigned integers.
pub enum UnsignedIntegerType {
    UnsignedChar,
    UnsignedShort,
    UnsignedInt,
    UnsignedLong,
    UnsignedLongLong,
}

impl Into<&str> for UnsignedIntegerType {
    fn into(self) -> &'static str {
        match self {
            UnsignedIntegerType::UnsignedChar => "unsigned char",
            UnsignedIntegerType::UnsignedShort => "unsigned short",
            UnsignedIntegerType::UnsignedInt => "unsigned int",
            UnsignedIntegerType::UnsignedLong => "unsigned long",
            UnsignedIntegerType::UnsignedLongLong => "unsigned long long",
        }
    }
}

impl UnsignedIntegerType {
    pub fn rank(&self) -> u8 {
        match self {
            UnsignedIntegerType::UnsignedChar => 2,
            UnsignedIntegerType::UnsignedShort => 3,
            UnsignedIntegerType::UnsignedInt => 4,
            UnsignedIntegerType::UnsignedLong => 5,
            UnsignedIntegerType::UnsignedLongLong => 6,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
// All floating point types.
pub enum FloatingType {
    Float,
    Double,
    LongDouble,
}

impl FloatingType {
    pub fn rank(&self) -> u8 {
        match self {
            FloatingType::Float => 7,
            FloatingType::Double => 8,
            FloatingType::LongDouble => 9,
        }
    }
}

impl Into<&str> for FloatingType {
    fn into(self) -> &'static str {
        match self {
            FloatingType::Float => "float",
            FloatingType::Double => "double",
            FloatingType::LongDouble => "long double",
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
// enum { ... } Abc;.
pub struct EnumeratedType {
    variants: Vec<EnumeratedTypeVariant>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
// A variant name with a constant integer expression.
pub struct EnumeratedTypeVariant {
    id: String,
    // constant: Expression,
}

#[derive(Debug, Clone, Eq, PartialEq)]
// A user-defined data type.
pub enum DerivedType {
    Array(ArrayType),
    Structure(StructureType),
    Union(UnionType),
    Function(FunctionType),
    Pointer(PointerType),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ArrayType {
    pub elem_type: Box<Type>,
    pub size: u32,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructureType {
    pub struct_name: String,
    pub fields: Vec<EnumeratedTypeVariant>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UnionType {
    pub union_name: String,
    pub fields: Vec<EnumeratedTypeVariant>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionType {
    pub returned_type: Box<Type>,
    pub params: Vec<Type>,
    pub is_var: bool,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PointerType(pub Box<Type>);

