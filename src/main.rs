#![feature(proc_macro_hygiene)]
mod translator;
mod expression_translator;
mod ast;
mod inspect_store;

use ast::{TranslationUnit, Ast};
use translator::Translator;
use std::path::Path;
use std::fs::File;

fn parse_file(path: &Path) -> TranslationUnit {
	let file = File::open(&path).expect("couldn't open target file");
	//TODO: crate parser
	TranslationUnit{decls: Ast(Default::default())}
}

//TODO: dump ast, dump llvmir, into obj file
fn main() {
	let path = Path::new("hello.txt");
	let unit = parse_file(path);
	let module = Translator::new().translate(unit);
	unsafe {
		llvm_sys::core::LLVMDumpModule(module);
	}
}
