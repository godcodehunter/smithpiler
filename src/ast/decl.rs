use super::r#type::Type;
use super::expr;

pub enum Decl {
    Other(Other),
    StaticAssert(StaticAssert),
}

struct Other {
    specifiers: Vec<DeclarationSpecifier>,
    init_declarators: Vec<InitDeclarator>, 
}

struct InitDeclarator {
    declarator: Declarator,
    initializer: Option<Initializer>,
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
    EnumSpecifier(),
    TypedefName(),
}

pub struct AtomicTypeSpecifier {
    typename: TypeName,
}

pub enum ObjKind  {
    Struct,
    Union,
}

struct StructOrUnionSpecifier {
    kind: ObjKind,
    indetifier: Option<String>,
    decl_list: Vec<StructDeclaration>,
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

struct Declarator2 {
    declarator: Option<Declarator>,
    constant: Box<expr::Expression>,
}

struct Declarator {
    
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

pub enum Initializer {
    InitializerList(InitializerList),
    AssignmentExpression,
}

pub struct InitializerList{
    initializer: Vec<(Vec<Designator>, Initializer)>,
}

pub enum Designator {
    Dot(String),
    Index(Box<expr::Expression>),
}

impl Designator {
    pub fn new_dot(value: String) -> Self {
        Dot{value}
    }

    pub fn new_index(value: Box<expr::Expression>) -> Self {
        Index{value}
    }
}

pub struct StaticAssert {
    predicate: Box<expr::Expression>,
    message: String,
}

impl StaticAssert {
    pub fn new(predicate: Box<expr::Expression>, message: String) -> Self  {
        Self{predicate, message}
    }
}

pub enum AbstractDeclarator {
    
}

pub enum DirectAbstractDeclarator {

}

