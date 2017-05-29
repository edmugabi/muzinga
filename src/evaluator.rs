use std::fmt;

use super::sheet::Sheet;
use super::sheet::NodeRef;

#[derive(Clone,PartialEq)]
pub enum Expr {
    Val(i32),
    Ref(NodeRef),
    BinExpr(BinOp, Box<Expr>,Box<Expr>),
    UnExpr(UnOp, Box<Expr>)
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Expr::Val(val) => write!(f, "{}",val),
            &Expr::BinExpr(ref op, ref e1, ref e2) => {
                write!(f, "({} {} {})", e1.to_string(), op.to_string(), e2.to_string())
            },
            &Expr::UnExpr(ref op, ref e) => {
                write!(f, "({}{})", op.to_string(), e.to_string())
            },
            &Expr::Ref(ref s) => {
                write!(f, "{}", s)
            }
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Expr {
    pub fn eval(&self, env: &Sheet) -> Value { // make sheet a ref
        match self {
            &Expr::Val(val) => Value::Int(val),
            &Expr::BinExpr(ref op, ref e1, ref e2) => {

                match (op, e1.eval(env), e2.eval(env)) {
                    (&BinOp::Add, Value::Int(v1), Value::Int(v2)) => Value::Int(v1 + v2),
                    (&BinOp::Sub, Value::Int(v1), Value::Int(v2)) => Value::Int(v1 - v2),
                    (&BinOp::Mult, Value::Int(v1), Value::Int(v2)) => Value::Int(v1 * v2),
                    (&BinOp::Div, Value::Int(v1), Value::Int(v2)) => Value::Int(v1 / v2),
                }
            },
            &Expr::UnExpr(ref op, ref e) => {
                match (op, e.eval(env)) {
                    (&UnOp::Neg, Value::Int(v)) => Value::Int(-v)
                }
            },

            &Expr::Ref(ref s) => {
                env.get_val(s)
            }
        }
    }
}

#[derive(PartialEq,Clone)]
pub enum Value {
    Int(i32)
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Value::Int(i) => write!(f,"{}",i)
        }
    }
}

#[derive(Clone,Debug,PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mult,
    Div,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &BinOp::Add => write!(f,"+"),
            &BinOp::Sub => write!(f,"-"),
            &BinOp::Mult => write!(f,"*"),
            &BinOp::Div => write!(f,"/"),
        }
    }
}

#[derive(Clone,Debug,PartialEq)]
pub enum UnOp {
    Neg
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &UnOp::Neg => write!(f, "-"),
        }
    }
}

#[test]
fn test_eval() {
    let sheet = Sheet::new();
    let four = Expr::Val(4);
    let ten = Expr::Val(10);
    let plus_expr = Expr::BinExpr(BinOp::Add, Box::new(four.clone()),Box::new(ten.clone()));
    let minus_expr = Expr::BinExpr(BinOp::Sub, Box::new(four.clone()),Box::new(ten.clone()));
    let neg_expr = Expr::UnExpr(UnOp::Neg, Box::new(four.clone()));

    assert_eq!(Value::Int(4), four.clone().eval(&sheet));
    assert_eq!(Value::Int(14),plus_expr.eval(&sheet));
    assert_eq!(Value::Int(-6),minus_expr.eval(&sheet));
    assert_eq!(Value::Int(-4),neg_expr.eval(&sheet));
}
