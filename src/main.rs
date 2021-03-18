#![feature(new_uninit)]
#![feature(concat_idents)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(maybe_uninit_extra)]
#![feature(maybe_uninit_ref)]
mod translator;
mod r#type;
mod expression;
use translator::Translator;
mod utils;
mod diagnostics;
mod translate_type;
mod type_cast;
mod statement;

extern crate llvm_sys as llvm;
extern crate clap;
use clap::{Arg, App, SubCommand};
use std::process::Command;
use std::path::{PathBuf, Path};
use std::os::unix::ffi::OsStrExt;
use lang_c::driver::{Config, parse};
use lang_c::visit::Visit;
use lang_c::print::Printer;

/// Accept path to llvm bitcode file and generate object file with same name   
fn generate_object(ll_file: &std::path::Path) {
	let status = Command::new("llc")
	.arg("-filetype=obj")
	.arg(ll_file)
	.status()
	.expect("failed to execute llc");
	
	if !status.success() {
		panic!("generating object file error: {}", status);
	} 
}

/// Accepts array of path to objects files and link they into executable 
/// NOTE: https://stackoverflow.com/questions/3577922/how-to-link-a-gas-assembly-program-that-uses-the-c-standard-library-with-ld-with
fn create_executable(obj_files: &[&std::path::Path]) {
	let status = Command::new("ld")
	.args(&[
		"-dynamic-linker",
		"/lib64/ld-linux-x86-64.so.2",
		"/usr/lib/x86_64-linux-gnu/crt1.o",
		"/usr/lib/x86_64-linux-gnu/crti.o",
		"-lc",
	])
	.args(obj_files)
	.arg("/usr/lib/x86_64-linux-gnu/crtn.o")
	.status()
	.expect("failed to execute ld");

	if !status.success() {
		panic!("linking executable: {}", status);
	}
}

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
	
	let input = Path::new(matches.value_of_os("INPUT").unwrap());
	
	let config = Config::default();
	let parse = parse(&config, input)?;

	// if matches.is_present("dump_lexer") {
	// 	parser.dump_lexer(input);
	// 	return Ok(());
	// }

	if matches.is_present("dump_ast") {
		let s = &mut String::new();
		Printer::new(s).visit_translation_unit(&parse.unit);
		println!("{}", s);
		return Ok(());
	}

	let mut translator = Translator::new();
	translator.translate(&parse, input.to_str().unwrap().into());
	translator.emit_diagnostics();

	let mut file_name: PathBuf = input.file_name()
		.unwrap()
		.to_os_string()
		.into();
	file_name.set_extension("bc");
	
	translator.write_to_file(file_name.clone());

	if matches.is_present("dump_llvmir") {
		let status = Command::new("llvm-dis")
			.arg(file_name.clone())
			.args(&["-o", "-"])
			.status()
			.expect("failed to execute llvm-dis");
			if !status.success() {
				panic!("print llvm assembler: {}", status);
			}
		return Ok(());
	}

	generate_object(file_name.as_path());
	file_name.set_extension("o");
	create_executable(&[file_name.as_path()]);
	
	Ok(())
}


