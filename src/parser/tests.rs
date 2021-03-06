use super::*;
use crate::ast::{expr, stmt};

// mod character_constant_tests {
//     use super::*;

//     #[test]
//     fn parse_constant() {
//         let parser = parser::CharacterConstantParser::new();
//         let res = parser.parse("'a'").unwrap();
//         assert_eq!(res.value, b'a');
//         assert!(res.prefix.is_none());
//     }

//     #[test]
//     fn parse_constant_with_prefix() {
//         let parser = parser::CharacterConstantParser::new();
//         let res = parser.parse("L'c'").unwrap();
//         assert_eq!(res.value, b'c');
//         assert!(res.prefix.is_some());
//         assert_eq!(res.prefix.unwrap(), expr::CharLiteralPrefix::L);
//     }
// }

mod expression_test {
    use super::*;

    #[test]
    fn parse_expression() {

    }
}

use crate::parser::public_parser::Parser;

mod jump_statement_tests {
    use super::*;

    #[test]
    fn parse_continue() {
        let mut parser = parser::JumpStatementParser::new();
        let res = parser.parse("continue;").unwrap();
        assert!(matches!(res, stmt::Statement::Continue));
    }

    #[test]
    fn parse_goto() {
        const label: &str = "label";
        let input = format!("goto {};", label);
        let mut parser = parser::JumpStatementParser::new();
        let res = parser.parse(input.as_ref()).unwrap();
        match res {
            stmt::Statement::Goto(ident) => {
                assert_eq!(ident, label);
            },
            _ => panic!()
        }   
    }

    #[test]
    fn parse_return() {
        let mut parser = parser::JumpStatementParser::new();
        let res = parser.parse("return;").unwrap();
        match res {
            stmt::Statement::Return(ret) => {
                assert!(ret.value.is_none());
            }
            ,
            _ => panic!()
        }
    }

    #[test]
    fn parse_return_with_expr() {
        let mut parser = parser::JumpStatementParser::new();
        let res = parser.parse("return ident*7+4;").unwrap();
        match res {
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

        let mut parser = parser::IterationStatementParser::new();
        let res = parser.parse(input);
    }

    #[test]
    fn parse_do_while() {
        const input: &str = r#"
            do {
                
            } while();
        "#;

        let mut parser = parser::IterationStatementParser::new();
        let res = parser.parse(input);
    }

    #[test]
    fn parse_for() {
        const input: &str = r#"
            for(;;) {

            }
        "#;

        let mut parser = parser::IterationStatementParser::new();
        let res = parser.parse(input);
    }
}

// mod selection_statement_tests {
//     use super::*;

//     #[test]
//     fn parse_if() {
//         const input: &str = r#"
//             if(var1 == 42) {
//                 var2 = 33;
//             }
//         "#;

//         let parser = parser::SelectionStatementParser::new();
//         let res = parser.parse(input);
//     }

//     #[test]
//     fn parse_if_else() {
//         const input: &str = r#"
//             if(var1 == 42) {
//                 var2 = 33;
//             } else {
//                 var2 = 1917;
//             }
//         "#;

//         let parser = parser::SelectionStatementParser::new();
//         let res = parser.parse(input);
//     }

//     #[test]
//     fn parse_switch() {
//         const input: &str = r#"
//             switch(var1) {
//                 case 1: {
//                     var2+=4;
//                     break;
//                 }
//                 case 2: {
//                     var2+=24;
//                     break;
//                 }   
//                 case 3: {
//                     var2+=74;
//                     break;
//                 }
//                 default: {
//                     var2+=290;
//                     break;
//                 }
//             }
//         "#;

//         let parser = parser::SelectionStatementParser::new();
//         let res = parser.parse(input);
//     }
// }


mod statement_tests {
    use super::*;

    #[test]
    fn parse_labeled() {
        const input: &str = "case 12: a = 42;";

        let mut parser = parser::StatementParser::new();
        parser.parse(input).unwrap();
    }

    #[test]
    fn parse_compound() {
        const input: &str = r#"
        {
            int a;
            a = (42*a)/200;
        }
        "#;

        let mut parser = parser::StatementParser::new();
        parser.parse(input).unwrap();
    }

    #[test]
    fn parse_expression() {
        const input: &str = "int a = 42;";

        let mut parser = parser::StatementParser::new();
        parser.parse(input).unwrap();
    }

    #[test]
    fn parse_selection() {
        const input: &str = r#"
            if(true) {

            }
        "#;

        let mut parser = parser::StatementParser::new();
        parser.parse(input).unwrap();
    }

    #[test]
    fn parse_iteration() {
        const input: &str = r#"
            for(;;) {

            }
        "#;

        let mut parser = parser::StatementParser::new();
        parser.parse(input).unwrap();
    }

    #[test]
    fn parse_jump() {
        const input: &str  = "continue;";

        let mut parser = parser::StatementParser::new();
        let result = parser.parse(input).unwrap();
        assert!(matches!(result, stmt::Statement::Continue));
    }
}

// mod declarations_tests {
//     use super::*;


// }