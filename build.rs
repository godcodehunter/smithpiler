extern crate lalrpop;

fn main() {
    lalrpop::Configuration::new()
        .emit_report(true)
        .process_file("src/parser/parser.lalrpop").unwrap();
}