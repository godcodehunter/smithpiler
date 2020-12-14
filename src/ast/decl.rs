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
    constant: Box<expr::Expr>,
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
    Index(Box<expr::Expr>),
}

pub struct StaticAssert {
    predicate: Box<expr::Expr>,
    message: String,
}

pub enum AbstractDeclarator {

}

pub enum DirectAbstractDeclarator {

}

