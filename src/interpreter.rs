use crate::ast::{Operator, Node};
use crate::parser::parse;

pub trait Compile {
    type Output;

    fn from_ast(ast: Vec<Node>) -> Self::Output;

    fn from_source(source: &str) -> Self::Output {
        println!("Compiling from the source: {}", source);
        let ast: Vec<Node> = parse(source).unwrap();
        println!("{:?}", ast);
        Self::from_ast(ast)
    }
}

pub struct Engine;

impl Compile for Engine {
    type Output = Result<i32, ()>;

    fn from_ast(ast: Vec<Node>) -> Self::Output {
        let mut ret = 0i32;
        let evaluator = Eval::new();
        for node in ast {
            ret += evaluator.eval(&node);
        }
        Ok(ret)
    }
}

struct Eval;

impl Eval {
    pub fn new() -> Self {
        Self
    }

    pub fn eval(&self, node: &Node) -> i32 {
        match node {
            Node::Int(n) => *n,
            Node::UnaryExpr {op, child} => {
                let child = self.eval(&child);
                match op {
                    Operator::Plus => child,
                    Operator::Minus => -child,
                }
            }
            Node::BinaryExpr {op, lhs, rhs } => {
                let lhs_ret = self.eval(&lhs);
                let rhs_ret = self.eval(&rhs);

                match op {
                    Operator::Plus => lhs_ret + rhs_ret,
                    Operator::Minus => lhs_ret - rhs_ret,
                }
            }
        }
    }
}
