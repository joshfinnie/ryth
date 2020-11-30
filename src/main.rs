use std::{env, fs};

mod lexer {
    use logos::Logos;

    #[derive(Logos, Debug, PartialEq, Clone)]
    #[allow(non_camel_case_types)]
    pub enum Token {
        #[error]
        #[regex(r"[ \t\n\f]+", logos::skip)]
        ERROR,
        EOF,
        #[regex("[a-zA-Z]+", |lexer| lexer.slice().to_owned())]
        IDENT(String),
        #[regex("[0-9]+", |lexer| lexer.slice().parse())]
        INT(i32),
        #[token(";")]
        SEMICOLON,
        #[token("+")]
        PLUS,
        #[token("_")]
        MINUS,
        #[token("/")]
        SLASH,
        #[token("*")]
        ASTERISK,
        #[token("(")]
        LPAREN,
        #[token(")")]
        RPAREN,
        #[token("Z")]
        ZERO,
        #[token("T")]
        TEN,
    }

    pub fn lex(input: &str) -> Vec<Token> {
        let mut tokens = Token::lexer(input).collect::<Vec<Token>>();
        tokens.push(Token::EOF);

        tokens
    }
}

mod parser {
    use crate::lexer::Token;

    #[derive(Debug, PartialEq, Clone)]
    pub enum Statement {
        Expression(Expr)
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Expr {
        Const(i32),
        String(String),
        Boolean(bool),
        Ident(String),
        Operation{operator: Operator, operands: Vec<Expr>}
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Operator {
        Plus,
        Minus,
        Multiply,
        Divide,
    }

    #[derive(Debug, PartialOrd, PartialEq)]
    enum Precedence {
        Lowest,
        Equals,
        Sum,
        Product,
    }

    impl Token {
        fn precedence(&self) -> Precedence {
            match self {
                Token::PLUS => Precedence::Sum,
                Token::MINUS => Precedence::Sum,
                Token::SLASH => Precedence::Product,
                Token::ASTERISK => Precedence::Product,
                _ => Precedence::Lowest
            }
        }
    }

    pub fn parse(input: &mut Vec<Token>) -> Vec<Statement> {
        let mut program = vec![];

        loop {
            let token = &input[0];

            match token {
                Token::EOF => break,
                _ => program.push(
                    Statement::Expression(
                        parse_expression(input, Precedence::Lowest)
                    )
                )
            }

            assert_eq!(Token::SEMICOLON, input.remove(0));
        }

        program
    }

    fn parse_expression(input: &mut Vec<Token>, _precedence: Precedence) -> Expr {
        let left_expr = match input.remove(0) {
            Token::INT(value) => Expr::Const(value),
            Token::TEN => Expr::Const(10),
            Token::ZERO => Expr::Const(0),
            Token::PLUS => parse_operation(Token::PLUS, input),
            Token::MINUS => parse_operation(Token::MINUS, input),
            _ => {
                panic!("Unexpected parameter found while parsing function args.");
            },
        };

        left_expr
    }

    fn parse_operation(t: Token, input: &mut Vec<Token>) -> Expr {
        let operator = match t {
            Token::PLUS => Operator::Plus,
            Token::MINUS => Operator::Minus,
            Token::SLASH => Operator::Divide,
            Token::ASTERISK => Operator::Multiply,
            _ => panic!("Could not parse operation."),
        };
        let mut operands = vec![];
        loop {
            if input[0] == Token::SEMICOLON {
                break;
            }
            match input.remove(0) {
                Token::INT(value) => operands.push(Expr::Const(value)),
                Token::TEN => operands.push(Expr::Const(10)),
                Token::ZERO => operands.push(Expr::Const(0)),
                Token::PLUS => operands.push(parse_operation(Token::PLUS, input)),
                _ => panic!("could not parse operation, not operating on numbers."),
            }
        }

        Expr::Operation {
            operator,
            operands,
        }
    }
}

mod eval {
    use std::collections::HashMap;

    use crate::parser::{Expr, Operator, Statement};

    pub struct Env {
        env: HashMap<String, Object>,
    }

    impl Env {
        pub fn new() -> Self {
            Env {
                env: HashMap::new(),
            }
        }

        pub fn set(&mut self, key: String, value: Object) {
            self.env.insert(key, value);
        }

        pub fn get(&self, key: &str) -> Option<Object> {
            self.env.get(key).map(|val| val.clone())
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Object {
        Null,
        Interger(i32),
        String(String),
        Boolean(bool),
        Return(Box<Object>),
    }

    fn eval_expr(expr: Expr, env: &mut Env) -> Object {
        match expr {
            Expr::Const(num) => Object::Interger(num),
            Expr::Operation{operator: Operator::Plus, operands} => {
                let mut ints = vec![];
                for operand in operands {
                    match eval_expr(operand, env) {
                        Object::Interger(int) => ints.push(int),
                        _ => panic!("plus operator used on invalid type."),
                    }
                }
                let total = ints.iter().fold(0, |mut sum, &val| {sum += val; sum});
                Object::Interger(total)
            }
            Expr::Operation{operator: Operator::Minus, operands} => {
                let mut ints = vec![];
                for operand in operands {
                    match eval_expr(operand, env) {
                        Object::Interger(int) => ints.push(int),
                        _ => panic!("minus operator used on invalid type."),
                    }
                }
                let mut total = 0;
                for (i, int) in ints.iter().enumerate() {
                    if i == 0 {
                        total += int;
                    } else {
                        total -= int;
                    }
                }
                Object::Interger(total)
            }
            _ => panic!("Not implemented yet!"),
        }
    }

    fn eval_statement(statement: Statement, env: &mut Env) -> Object {
        match statement {
            Statement::Expression(expr) => eval_expr(expr, env),
            _ => panic!("this functionality is not implemented yet."),
        }
    }

    fn eval_statements(statements: Vec<Statement>, env: &mut Env) -> Object {
        let mut result = Object::Null;

        for statement in statements {
            result = eval_statement(statement, env);

            if let &Object::Return(_) = &result {
                return result;
            }
        }

        result
    }

    pub fn eval_return_scope(statements: Vec<Statement>, env: &mut Env) -> Object {
        let result = eval_statements(statements, env);

        match result {
            Object::Return(res) => *res,
            _ => result,
        }
    }
}

fn main() {
    let mut env = eval::Env::new();
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("No input file was provided.");
        std::process::exit(-1);
    }

    let code = &fs::read_to_string(&args[1]).unwrap();
    println!("CODE");
    println!("{:?}", code);

    for c in code.split("\n") {
        let mut tokens = lexer::lex(&c);
        let parsed = parser::parse(&mut tokens);
        let evaled = eval::eval_return_scope(parsed, &mut env);
        println!("EVALUATED");
        println!("{:?}", evaled);
    }

    //let mut tokens = lexer::lex(&code);
    //println!("TOKENS");
    //println!("{:?}", tokens);

    //let parsed = parser::parse(&mut tokens);
    //println!("PARSED AST");
    //println!("{:?}", parsed);

    //let evaled = eval::eval_return_scope(parsed, &mut env);
    //println!("EVALUATED");
    //println!("{:?}", evaled);
}
