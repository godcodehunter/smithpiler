extern crate lalrpop;

fn main() {
    lalrpop::Configuration::new()
        .emit_report(true)
        .log_debug()
        .process_file("src/parser/parser.lalrpop").unwrap();
}