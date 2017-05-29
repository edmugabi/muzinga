use std::slice::Iter;
use std::iter::Peekable;

use lexer::Token;
use lexer::OpToken;
use evaluator::Expr;
use evaluator::BinOp;
use evaluator::UnOp;
use sheet::NodeRef;

pub struct Parser<'a> {
    input: Peekable<Iter<'a, Token>>
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &Vec<Token>) -> Parser {
            Parser { input: tokens.iter().peekable() }
        }

    //pass in the environment to attach to node references
    pub fn parse(&mut self) -> Result<(Expr,Vec<NodeRef>),String> {
        self.parse_expr(Prec::LOWEST, Vec::new())
    }
    fn parse_expr(&mut self, precedence: Prec, v: Vec<NodeRef>) -> Result<(Expr,Vec<NodeRef>),String> {

        let (mut expr,mut v) = self.parse_prefix(v).unwrap();

        while let Some(&next_token) = self.input.peek() {

            let next_precedence = match next_token {
                &Token::Op(ref symbol) => Parser::get_precedence(symbol),
                _ => return Ok((expr,v))
            };

            if precedence >= next_precedence {
                break;
            }
            let expr_v = self.parse_infix(expr,next_precedence,v).unwrap();
            expr = expr_v.0;
            v = expr_v.1;
        }
        Ok((expr,v))
    }

    fn parse_prefix(&mut self, mut v: Vec<NodeRef>) -> Result<(Expr, Vec<NodeRef>),String> {
        match self.input.next() {
            Some(t) => match t {
                &Token::Int(ref s) => {
                    Ok((Expr::Val(s.parse::<i32>().unwrap()),v))
                },

                &Token::Op(ref op_tok) => match op_tok {

                    &OpToken::Minus => {
                        let (op_expr,v_nodes) = self.parse_expr(Prec::PREFIX,v).unwrap();
                                        Ok((Expr::UnExpr(
                                                UnOp::Neg,
                                                box op_expr),
                                         v_nodes))},
                    _ => Err(format!("{:?} not a prefix token",op_tok))
                },
                &Token::LParen => {

                    let expr_v = self.parse_expr(Prec::LOWEST,v);
                    if self.expect_peek(&Token::RParen) {
                        expr_v
                    } else {
                        Err(format!("Expected Token: {:?}",Token::RParen))
                    }
                },
                &Token::NodeRef(ref row,ref col) => {
                    v.push(NodeRef::new(row.clone(),col.clone()));
                    Ok((Expr::Ref(NodeRef::new(row.clone(),col.clone())),v))
                },
                _ => Err(format!("unexpected tokens: {:?}",t))
            },
            None => Err("No more tokens".to_owned())
        }
    }

    fn parse_infix(&mut self, left: Expr, precedence: Prec, v: Vec<NodeRef>) -> Result<(Expr,Vec<NodeRef>), String> {
        match self.input.next() {
            Some(t) => match t {
                &Token::Op(ref symbol) => {
                    let bin_op = match symbol {
                        &OpToken::Plus => BinOp::Add,
                        &OpToken::Minus => BinOp::Sub,
                        &OpToken::Asterisk => BinOp::Mult,
                        &OpToken::FSlash => BinOp::Div
                    };

                    let (right,v) = self.parse_expr(precedence,v).unwrap();

                    Ok((Expr::BinExpr(
                        bin_op,
                        box left,
                        box right
                    ),v))
                },
                _ => Err(format!("Unexpected Token {:?}",t))
            },
            None => Err("No more tokens".to_owned())
        }
    }

    fn expect_peek(&mut self, tok: &Token) -> bool {
        if let Some(&token) = self.input.peek() {
            if token == tok {
                self.input.next();
                true
            } else {
                // trigger an error when it is not the case
                false
            }
        } else {
            false
        }
    }

    fn get_precedence(op_token: &OpToken) -> Prec {
        match op_token {
            &OpToken::Plus | &OpToken::Minus => Prec::SUM,
            &OpToken::Asterisk | &OpToken::FSlash => Prec::PROD,
        }
    }
}

#[derive(PartialEq,PartialOrd)]
enum Prec {
    LOWEST = 10,
    SUM = 20,
    PROD = 30,
    PREFIX = 40,
}

#[cfg(test)]
mod tests {

    use super::*;
    fn pretty_printer(s: &str) -> String {
        let mut l = ::lexer::Lexer::new(s);
        let tokens = l.lex();
        let mut p = Parser::new(&tokens);
        let (expr, _) = p.parse().unwrap();
        expr.to_string()
    }

    #[test]
    fn test_parse() {
        let mut l = ::lexer::Lexer::new("4 - 10 + 6 - 5");
        let tokens = l.lex();
        let mut p = Parser::new(&tokens);
        let (expr,_) = p.parse().unwrap();
        let expected_expr = Expr::BinExpr(
                                BinOp::Sub,
                                box Expr::BinExpr(
                                    BinOp::Add,
                                    box Expr::BinExpr(BinOp::Sub, box Expr::Val(4), box Expr::Val(10)),
                                    box Expr::Val(6),
                                ),
                                box Expr::Val(5)
                        );
        assert_eq!(expected_expr, expr);

    }

    #[test]
    fn test_expr_1() {
        let s = "-4 + -5 - --6";
        let expected_pp = "(((-4) + (-5)) - (-(-6)))";
        assert_eq!(expected_pp, pretty_printer(s));
    }

    #[test]
    fn test_expr_2() {
        let s = "4 + 1 - (5 - 2)";
        let expected_pp = "((4 + 1) - (5 - 2))";
        assert_eq!(expected_pp, pretty_printer(s));
    }

    #[test]
    fn test_node_ref() {
        let s = "C1 + C2 / (D4 - CA44)";
        let mut l = ::lexer::Lexer::new(s);
        let tokens = l.lex();
        let mut p = Parser::new(&tokens);
        let (_,v) = p.parse().unwrap();
        let expected = vec![
                        "C1".to_owned(),
                        "C2".to_owned(),
                        "D4".to_owned(),
                        "CA44".to_owned(),
        ];

        let v: Vec<String> = v.into_iter().map(|s| s.to_string()).collect();
        assert_eq!(expected,v);

    }

}
