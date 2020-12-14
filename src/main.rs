#![feature(proc_macro_hygiene)]
mod translator;
mod expression_translator;
mod ast;
mod inspect_store;
mod parser;

use inspect_store::InspectStore;
use ast::Ast;
use translator::Translator;
use std::path::Path;
use std::fs::File;


fn parse_file(path: &Path) -> TranslationUnit {
	let file = File::open(&path).expect("couldn't open target file");
	
	//TODO: crate parser
	TranslationUnit{decls: Ast(Default::default())}
}

extern crate clap;
use clap::{Arg, App, SubCommand};

fn main() {
	let matches = App::new("smithpiler")
        .version("0.1.0")
        .author("Dmitry Opokin <mamhigtt@gmail.com>")
        .about("C11 LLVM fronted compiler")
        .arg(Arg::with_name("INPUT")
            .help("Sets the input file to use")
            .required(true)
            .index(1))
        .subcommand(SubCommand::with_name("dump ast")
			.about("Printing ast"))
		.subcommand(SubCommand::with_name("dump llvmir")
			.about("Printing llvmir"))
		.get_matches();
	
	let path = matches.value_of("INPUT").unwrap();
	let unit = parse_file(Path::new(path));
	let inspect_store = InspectStore::new();
	let module = Translator::new(inspect_store).translate(unit);
	unsafe {
		llvm_sys::core::LLVMDumpModule(module);
	}
}

// /////// 

// // A function declaration is either a function definition or a prototype.
// pub enum FuncDecl {
//     Def(FuncDef),
//     Prototype(FuncPrototype),
// }

// // A function definition like void add(int x, int y) {return x + y; }.
// pub struct FuncDef {
//     pub func_name: String,
//     pub return_ty: Box<Type>,
//     pub params: FuncDefParams,
//     pub body: CompoundStmt,
// }

// type FuncDefParams = Vec<FuncDefParam>;

// pub struct FuncDefParam {
//     param_name: String,
//     ty: Type,
// }

// // A function prototype like void func(int x, int y);.
// pub struct FuncPrototype {
//     func_name: String,
//     return_ty: Type,
//     params: FuncDefParams,
// }

// #[derive(Clone, Eq, PartialEq)]
// // A variable declaration like int x;
// pub struct VarDecl {
//     var_name: String,
//     ty: Type,
// }


//////////////////////////////////////////////////


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

