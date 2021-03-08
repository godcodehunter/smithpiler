
use super::r#type::Type;
use super::expr;

#[derive(Debug)]
pub enum Declaration {
    Other(Other),
    StaticAssert(StaticAssert),
}

#[derive(Debug)]
pub struct Other {
    pub specifiers: Vec<DeclarationSpecifier>,
    pub init_declarators: Option<Vec<InitDeclarator>>, 
}

#[derive(Debug)]
pub struct InitDeclarator {
    pub declarator: Declarator,
    pub initializer: Option<Initializer>,
}

impl InitDeclarator {
    pub fn new(declarator: Declarator, initializer: Option<Initializer>) -> Self {
        todo!()
    }
}

#[derive(Debug)]
pub enum DeclarationSpecifier {
    StorageClassSpecifier(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
    FunctionSpecifier(FunctionSpecifier),
    AlignmentSpecifier(AlignmentSpecifier),
}

#[derive(Debug)]
pub enum StorageClassSpecifier {
    Typedef,
    Extern,
    Static,
    ThreadLocal,
    Auto,
    Register,
}

#[derive(Debug)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Bool,
    Complex,
    AtomicTypeSpecifier(AtomicTypeSpecifier),
    StructOrUnionSpecifier(StructOrUnionSpecifier),
    EnumSpecifier(EnumSpecifier),
    TypedefName(String),
}

#[derive(Debug)]
pub struct AtomicTypeSpecifier {
    typename: (),
}

#[derive(Debug)]
pub enum ObjKind  {
    Struct,
    Union,
}

#[derive(Debug)]
pub struct AlignmentSpecifier {

}

#[derive(Debug)]
pub struct StructOrUnionSpecifier {
    kind: ObjKind,
    indetifier: Option<String>,
    decl_list: Vec<StructDeclaration>,
}

#[derive(Debug)]
pub enum SpecifierQualifierList {
    
}

#[derive(Debug)]
pub enum StructDeclaration {
    Field(Filed),
    StaticAssert(StaticAssert),
}

#[derive(Debug)]
pub struct Filed {
    pub qualifier: SpecifierQualifierList,
    pub declarator: Vec<StructDeclarator>,
} 

#[derive(Debug)]
pub enum StructDeclarator {
    Declarator(Declarator),
    Declarator2(Declarator2),
}

#[derive(Debug)]
pub struct Declarator2 {
    declarator: Option<Declarator>,
    constant: Box<expr::Expression>,
}

#[derive(Debug)]
pub struct Declarator {
    
}

#[derive(Debug)]
pub struct EnumSpecifier {

}

pub struct Enumerator {
    pub identifier: String,
    pub expression: expr::Expression,
}

#[derive(Debug)]
pub enum TypeQualifier {
    Const,
    Restrict,
    Volatile,
    Atomic,
}

#[derive(Debug)]
pub enum FunctionSpecifier {
    Inline,
    Noreturn,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Initializer {
    InitializerList(InitializerList),
    AssignmentExpression,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct InitializerList {
    initializer: Vec<(Vec<Designator>, Initializer)>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Designator {
    Dot(String),
    Index(expr::Expression),
}

impl Designator {
    pub fn new_dot(value: String) -> Self {
        Designator::Dot(value)
    }

    pub fn new_index(value: expr::Expression) -> Self {
        Designator::Index(value)
    }
}

#[derive(Debug)]
pub struct StaticAssert {
    pub predicate: expr::Expression,
    pub message: String,
}

impl StaticAssert {
    pub fn new(predicate: expr::Expression, message: String) -> Declaration  {
        Declaration::StaticAssert(Self{predicate, message})
    }
}

#[derive(Debug)]
pub enum DirectDeclarator {
    FunctionDeclarators,
}

#[derive(Debug)]
pub enum AbstractDeclarator {
    
}

#[derive(Debug)]
pub enum DirectAbstractDeclarator {

}

#[derive(Debug)]
pub struct ParameterTypeList {
    
}

#[derive(Debug)]
pub struct ParameterDeclaration {
    
}