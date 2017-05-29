#![feature(box_syntax)]

mod lexer;
mod parser;
mod evaluator;
mod sheet;

use sheet::Sheet;

//TODO: Do a system audit and assess possibility of updating in single loop at the time when the cell is updated
//TODO: would then need to assess what to do with calling the observer functions
//TODO: Do system audit and ensure lexer-parser-evaluator is a single loop
//TODO: Ensure that the cell update propagation is also one loop
//TODO: After a proof of concept of the idea, will need to work on efficiency, make the inter sheet messaging efficient

fn interprete(s: &str) -> String {
        let sheet = Sheet::new();
        let mut l = lexer::Lexer::new(s);
        let tokens = l.lex();
        let mut p = parser::Parser::new(&tokens);
        let (expr,_) = p.parse().unwrap();
        expr.eval(&sheet).to_string()
}

#[test]
fn test_arithmetic() {
    assert_eq!("7", interprete("4+3"));
    assert_eq!("4",interprete("8/2"));
    assert_eq!("12",interprete("8 + 5 * 2 - 3 * 4 / 2"));
    assert_eq!("-2",interprete("8 + 5 * (2 - 3) * 4 / 2"));
}
