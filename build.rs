extern crate lalrpop;

fn main() {
    lalrpop::Configuration::new().process_file("src/parser/parser.lalrpop").unwrap();
}