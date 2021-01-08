use super::r#type::Type;
use super::expr;

pub enum Declaration {
    Other(Other),
    StaticAssert(StaticAssert),
}

pub struct Other {
    specifiers: Vec<DeclarationSpecifier>,
    init_declarators: Vec<InitDeclarator>, 
}

pub struct InitDeclarator {
    declarator: Declarator,
    initializer: Option<Initializer>,
}

impl InitDeclarator {
    pub fn new(declarator: Declarator, initializer: Option<Initializer>) -> Self {
        todo!()
    }
}

pub enum DeclarationSpecifier {
    StorageClassSpecifier(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
    FunctionSpecifier(FunctionSpecifier),
    AlignmentSpecifier(AtomicTypeSpecifier),
}

pub enum StorageClassSpecifier {
    Typedef,
    Extern,
    Static,
    ThreadLocal,
    Auto,
    Register,
}

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
    TypedefName,
}

pub struct AtomicTypeSpecifier {
    typename: (),
}

pub enum ObjKind  {
    Struct,
    Union,
}

pub struct StructOrUnionSpecifier {
    kind: ObjKind,
    indetifier: Option<String>,
    decl_list: Vec<StructDeclaration>,
}

pub enum SpecifierQualifierList {
    
}

pub enum StructDeclaration {
    Field(Filed),
    StaticAssert(StaticAssert),
}

pub struct Filed {
    qualifier: SpecifierQualifierList,
    declarator: Vec<StructDeclarator>,
} 

pub enum StructDeclarator {
    Declarator(Declarator),
    Declarator2(Declarator2),
}

pub struct Declarator2 {
    declarator: Option<Declarator>,
    constant: Box<expr::Expression>,
}

pub struct Declarator {
    
}

pub struct EnumSpecifier {

}

pub struct Enumerator {
    identifier: String,
    expression: expr::Expression,
}

pub enum TypeQualifier {
    Const,
    Restrict,
    Volatile,
    Atomic,
}

pub enum FunctionSpecifier {
    Inline,
    Noreturn,
}

#[derive(Clone, Eq, PartialEq)]
pub enum Initializer {
    InitializerList(InitializerList),
    AssignmentExpression,
}

#[derive(Clone, Eq, PartialEq)]
pub struct InitializerList {
    initializer: Vec<(Vec<Designator>, Initializer)>,
}

#[derive(Clone, Eq, PartialEq)]
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

pub struct StaticAssert {
    predicate: expr::Expression,
    message: String,
}

impl StaticAssert {
    pub fn new(predicate: expr::Expression, message: String) -> Declaration  {
        Declaration::StaticAssert(Self{predicate, message})
    }
}

pub enum AbstractDeclarator {
    
}

pub enum DirectAbstractDeclarator {

}

pub struct ParameterTypeList {
    
}

pub struct ParameterDeclaration {
    
}