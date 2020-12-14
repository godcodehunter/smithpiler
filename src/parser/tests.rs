use super::*;
use crate::ast::{expr, stmt};

mod character_constant_tests {
    use super::*;

    #[test]
    fn parse_constant() {
        let parser = parser::CharacterConstantParser::new();
        let res = parser.parse("'a'").unwrap();
        assert_eq!(res.value, b'a');
        assert!(res.prefix.is_none());
    }

    #[test]
    fn parse_constant_with_prefix() {
        let parser = parser::CharacterConstantParser::new();
        let res = parser.parse("L'c'").unwrap();
        assert_eq!(res.value, b'c');
        assert!(res.prefix.is_some());
        assert_eq!(res.prefix.unwrap(), expr::CharLiteralPrefix::L);
    }
}

mod expression_test {
    use super::*;

    #[test]
    fn t() {
    }
}

mod primary_expression_test {
    use super::*;

    #[test]
    fn parse_identifier() {
    }

    #[test]
    fn parse_constant() {
    }

    #[test]
    fn parse_string() {
    }

    #[test]
    fn parse_parenthesized_expr() {
    }

    #[test]
    fn parse_generic_selection() {
    }
}

mod jump_statement_tests {
    use super::*;

    #[test]
    fn parse_continue() {
        let parser = parser::JumpStatementParser::new();
        let res = parser.parse("continue;").unwrap();
        assert!(matches!(res.as_ref(), stmt::Statement::Continue));
    }

    #[test]
    fn parse_goto() {
        const label: &str = "label";
        let parser = parser::JumpStatementParser::new();
        let res = parser.parse(format!("goto {};", label).as_str()).unwrap();
        match res.as_ref() {
            stmt::Statement::Goto(ident) => {
                assert_eq!(ident, label);
            },
            _ => panic!()
        }   
    }

    #[test]
    fn parse_return() {
        let parser = parser::JumpStatementParser::new();
        let res = parser.parse("return;").unwrap();
        match res.as_ref() {
            stmt::Statement::Return(ret) => {
                assert!(ret.value.is_none());
            }
            ,
            _ => panic!()
        }
    }

    #[test]
    fn parse_return_with_expr() {
        let parser = parser::JumpStatementParser::new();
        let res = parser.parse("return ident*7+4;").unwrap();
        match res.as_ref() {
            stmt::Statement::Return(ret) => {
                assert!(ret.value.is_some());
            }
            ,
            _ => panic!()
        }
    }
}

mod iteration_statement_tests {
    use super::*;

    #[test]
    fn parse_while() {
        const input: &str = r#"
            while(true) {

            } 
        "#;

        let parser = parser::IterationStatementParser::new();
        let res = parser.parse(input);
    }

    #[test]
    fn parse_do_while() {
        const input: &str = r#"
            do {
                
            } while();
        "#;

        let parser = parser::IterationStatementParser::new();
        let res = parser.parse(input);
    }

    #[test]
    fn parse_for() {
        const input: &str = r#"
            for(;;) {

            }
        "#;

        let parser = parser::IterationStatementParser::new();
        let res = parser.parse(input);
    }
}

mod selection_statement_tests {
    use super::*;

    #[test]
    fn parse_if() {
        const input: &str = r#"
            if(var1 == 42) {
                var2 = 33;
            }
        "#;

        let parser = parser::SelectionStatementParser::new();
        let res = parser.parse(input);
    }

    #[test]
    fn parse_if_else() {
        const input: &str = r#"
            if(var1 == 42) {
                var2 = 33;
            } else {
                var2 = 1917;
            }
        "#;

        let parser = parser::SelectionStatementParser::new();
        let res = parser.parse(input);
    }

    #[test]
    fn parse_switch() {
        const input: &str = r#"
            switch(var1) {
                case 1: {
                    var2+=4;
                    break;
                }
                case 2: {
                    var2+=24;
                    break;
                }   
                case 3: {
                    var2+=74;
                    break;
                }
                default: {
                    var2+=290;
                    break;
                }
            }
        "#;

        let parser = parser::SelectionStatementParser::new();
        let res = parser.parse(input);
    }
}


mod statement_tests {
    use super::*;

    #[test]
    fn parse_labeled() {
        
    }

    #[test]
    fn parse_compound() {

    }

    #[test]
    fn parse_expression() {

    }

    #[test]
    fn parse_selection() {

    }

    #[test]
    fn parse_iteration() {

    }

    #[test]
    fn parse_jump() {

    }
}

mod declarations_tests {
    use super::*;


}