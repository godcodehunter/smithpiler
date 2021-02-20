// #![feature(proc_macro_hygiene)]
#![feature(generic_associated_types)]
// mod translator;
mod ast;
// mod inspect_store;
mod parser;

// use inspect_store::InspectStore;
// use translator::Translator;

extern crate clap;
use clap::{Arg, App, SubCommand};
use parser::{Options, Parser};

fn main() -> Result<(), Box<dyn std::error::Error>>  {
	let matches = App::new("smithpiler")
        .version("0.1.0")
        .author("Dmitry Opokin <mamhigtt@gmail.com>")
        .about("C11 LLVM fronted compiler")
        .arg(Arg::with_name("INPUT")
            .help("Sets the input file to use")
            .required(true)
			.index(1))
		.subcommand(SubCommand::with_name("dump_lexer")
			.about("Printing lexer token stream"))
        .subcommand(SubCommand::with_name("dump_ast")
			.about("Printing ast"))
		.subcommand(SubCommand::with_name("dump_llvmir")
			.about("Printing llvmir"))
		.get_matches();
	
	let path = matches.value_of("INPUT").unwrap();
	let options = Options {
		dump_ast: matches.is_present("dump_ast"), 
		dump_lexer: matches.is_present("dump_lexer"),
	};

	// let inspect_store = InspectStore::new();
	let unit = Parser::new(Some(options)).parse(path)?;
	println!("{}", unit);
	// let module = Translator::new(inspect_store).translate(unit);
	// if matches.is_present("dump_llvmir") {
	// 	unsafe {
	// 		llvm_sys::core::LLVMDumpModule(module);
	// 	}
	// }
	Ok(())
}


