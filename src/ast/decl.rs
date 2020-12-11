use super::r#type::Type;
use super::stmt::CompoundStmt;

// A declaration construct. Examples: void func(int, int);, int x; void add(int x, int y) {return x + y; }.
pub enum Decl {
    Func(FuncDecl),
    Var(VarDecl),
}

// A function declaration is either a function definition or a prototype.
pub enum FuncDecl {
    Def(FuncDef),
    Prototype(FuncPrototype),
}

// A function definition like void add(int x, int y) {return x + y; }.
pub struct FuncDef {
    pub func_name: String,
    pub return_ty: Box<Type>,
    pub params: FuncDefParams,
    pub body: CompoundStmt,
}

type FuncDefParams = Vec<FuncDefParam>;

pub struct FuncDefParam {
    param_name: String,
    ty: Type,
}

// A function prototype like void func(int x, int y);.
pub struct FuncPrototype {
    func_name: String,
    return_ty: Type,
    params: FuncDefParams,
}

#[derive(Clone, Eq, PartialEq)]
// A variable declaration like int x;
pub struct VarDecl {
    var_name: String,
    ty: Type,
}
