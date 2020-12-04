use std::{env, fs};
use std::io::{self, Read};

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
        #[regex(r#""[^"]*""#, |lexer| lexer.slice()[1..(lexer.slice().len()-1)].to_owned())]
        STRING(String),
        #[token(",")]
        COMMA,
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
        #[token("{")]
        LBRACE,
        #[token("}")]
        RBRACE,
        #[token("Z")]
        ZERO,
        #[token("T")]
        TEN,
        #[token("==")]
        EQ,
        #[token("!=")]
        NOTEQ,
        #[token("F")]
        FUNCTION,
        #[token("true")]
        TRUE,
        #[token("false")]
        FALSE,
        #[token("!")]
        BANG,
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
        Boolean(bool),
        Call{function: Box<Expr>, arguments: Vec<Expr>},
        Const(i32),
        Function{parameters: Vec<String>, body: Vec<Statement>},
        Ident(String),
        Operation{operator: Operator, operands: Vec<Expr>},
        Prefix{prefix: Prefix, value: Box<Expr>},
        String(String),
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Prefix {
        Bang,
        Minus,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Operator {
        Divide,
        Equals,
        Minus,
        Multiply,
        NotEquals,
        Plus,
    }

    #[derive(Debug, PartialOrd, PartialEq)]
    enum Precedence {
        Lowest,
        Equals,
        Sum,
        Product,
        Prefix,
    }

    impl Token {
        fn precedence(&self) -> Precedence {
            match self {
                Token::PLUS => Precedence::Sum,
                Token::MINUS => Precedence::Sum,
                Token::SLASH => Precedence::Product,
                Token::ASTERISK => Precedence::Product,
                Token::EQ => Precedence::Equals,
                Token::NOTEQ => Precedence::Equals,
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
            Token::TRUE => Expr::Boolean(true),
            Token::FALSE => Expr::Boolean(false),
            Token::BANG => Expr::Prefix{
                prefix: Prefix::Bang,
                value: Box::new(parse_expression(input, Precedence::Prefix))
            },
            Token::LPAREN => {
                let expr = parse_expression(input, Precedence::Lowest);
                assert_eq!(Token::RPAREN, input.remove(0));

                expr
            },
            Token::PLUS => parse_operation(Token::PLUS, input),
            Token::MINUS => parse_operation(Token::MINUS, input),
            Token::ASTERISK => parse_operation(Token::ASTERISK, input),
            Token::SLASH => parse_operation(Token::SLASH, input),
            Token::IDENT(ident) => {
                if &input[0] == &Token::LPAREN {
                    input.remove(0);
                    let mut args = vec![];
                    loop {
                        match &input[0] {
                            Token::RPAREN => {
                                input.remove(0);
                                break
                            },
                            _ => {
                                args.push(parse_expression(input, Precedence::Lowest));
                            },
                        }

                        match input.remove(0) {
                            Token::RPAREN => break,
                            Token::COMMA => continue,
                            _ => panic!("unexpected parameter found while parsing function.")
                        }
                    }

                    Expr::Call {
                        function: Box::new(Expr::Ident(ident)),
                        arguments: args
                    }
                } else {
                    Expr::Ident(ident)
                }
            },
            Token::FUNCTION => {
                let mut parameters = vec![];
                assert_eq!(Token::LPAREN, input.remove(0));
                loop {
                    match input.remove(0) {
                        Token::RPAREN => break,
                        Token::IDENT(ident) => {
                            parameters.push(ident);
                            match input.remove(0) {
                                Token::RPAREN => break,
                                Token::COMMA => continue,
                                _ => panic!("unexpected parameter found in function."),
                            }
                        },
                        _ => panic!("unexpected parameter found in function."),
                    }
                }

                assert_eq!(Token::LBRACE, input.remove(0));
                let body = parse(input);
                assert_eq!(Token::RBRACE, input.remove(0));

                Expr::Function {
                    parameters,
                    body,
                }
            },
            Token::STRING(string) => Expr::String(string),
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
            Token::EQ => Operator::Equals,
            Token::NOTEQ => Operator::NotEquals,
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
                Token::MINUS => operands.push(parse_operation(Token::MINUS, input)),
                Token::ASTERISK => operands.push(parse_operation(Token::ASTERISK, input)),
                Token::SLASH => operands.push(parse_operation(Token::SLASH, input)),
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

    use crate::parser::{Expr, Prefix, Operator, Statement, parse};
    use crate::lexer::lex;

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
        Integer(i32),
        String(String),
        Boolean(bool),
        Return(Box<Object>),
        Function{parameters: Vec<String>, body: Vec<Statement>},
    }

    fn eval_expr(expr: Expr, env: &mut Env) -> Object {
        match expr {
            Expr::String(string) => Object::String(string),
            Expr::Const(num) => Object::Integer(num),
            Expr::Boolean(boolean) => Object::Boolean(boolean),
            Expr::Prefix {prefix: Prefix::Bang, value: expr} => {
                match eval_expr(*expr, env) {
                    Object::Boolean(boolean) => Object::Boolean(!boolean),
                    _ => panic!("Bang Operator only valid for boolean types.")
                }
            },
            Expr::Operation{operator: Operator::Plus, operands} => {
                let mut ints = vec![];
                for operand in operands {
                    match eval_expr(operand, env) {
                        Object::Integer(int) => ints.push(int),
                        _ => panic!("plus operator used on invalid type."),
                    }
                }
                let total = ints.iter().fold(0, |mut sum, &val| {sum += val; sum});
                Object::Integer(total)
            }
            Expr::Operation{operator: Operator::Minus, operands} => {
                let mut ints = vec![];
                for operand in operands {
                    match eval_expr(operand, env) {
                        Object::Integer(int) => ints.push(int),
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
                Object::Integer(total)
            }
            Expr::Operation{operator: Operator::Multiply, operands} => {
                let mut ints = vec![];
                for operand in operands {
                    match eval_expr(operand, env) {
                        Object::Integer(int) => ints.push(int),
                        _ => panic!("multiply operator used on invalid type."),
                    }
                }
                let total = ints.iter().fold(1, |mut sum, &val| {sum *= val; sum});
                Object::Integer(total)
            },
            // TODO: Get division to work...
            Expr::Operation{operator: Operator::Divide, operands} => {
                let mut ints = vec![];
                for operand in operands {
                    match eval_expr(operand, env) {
                        Object::Integer(int) => ints.push(int),
                        _ => panic!("divide operator used on invalid type."),
                    }
                }
                let total = ints.iter().fold(1.0, |mut sum, &val| {sum /= val as f32; sum});
                Object::Integer(total as i32)
            },
            Expr::Ident(ident) => env.get(&ident).expect("attempted access to invalid binding"),
            Expr::Function{parameters, body} => Object::Function {parameters, body},
            Expr::Call{function, arguments} => {
                let (parameters, body) = match *function {
                    Expr::Ident(func_name) => {
                        match env.get(&func_name) {
                            Some(Object::Function {parameters, body}) => (parameters, body),
                            None => {
                                let arguments = arguments.into_iter().map(|expr| eval_expr(expr, env)).collect();
                                return eval_builtin(&func_name, arguments).expect("error calling function");
                            },
                            _ => panic!("attempted to call a non-function."),
                        }
                    },
                    Expr::Function {parameters, body} => (parameters, body),
                    _ => panic!("attempted to call a non-function."),
                };

                assert_eq!(parameters.len(), arguments.len(), "called function with wrong number of parameters.");

                let mut env_func = Env::new();
                for (parameter, arg_value) in parameters.into_iter().zip(arguments.into_iter()) {
                    env_func.set(parameter, eval_expr(arg_value, env));
                }

                eval_return_scope(body, &mut env_func)
            },
            _ => panic!("Not implemented yet!"),
        }
    }

    fn eval_builtin(func_name: &str, arguments: Vec<Object>) -> Option<Object> {
        match (func_name, arguments.as_slice()) {
            ("len", [Object::String(string)]) => Some(Object::Integer(string.len() as i32)),
            _ => None,
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

    pub fn display_object(obj: Object) {
        match obj {
            Object::Integer(num) => println!("{}", num),
            Object::String(string) => println!("{}", string),
            Object::Boolean(val) => println!("{}", val),
            Object::Null => println!("null"),
            Object::Return(obj) => display_object(*obj),
            Object::Function{parameters: _, body: _} => println!("function"),
        }
    }

    pub fn run(code: &str, env: &mut Env) -> Object {
        let mut tokens = lex(&code);
        let parsed = parse(&mut tokens);
        let obj = eval_return_scope(parsed, env);

        obj
    }
}

fn main() {
    let mut env = eval::Env::new();
    let args: Vec<String> = env::args().collect();

    match &args.len() {
        1 => {
            let mut code = String::new();
            let mut stdin = io::stdin();
            stdin.read_to_string(&mut code).expect("Could not read from standard input.");
            let evaled = eval::run(&code, &mut env);
            eval::display_object(evaled);
        },
        2 => {
            let code = &fs::read_to_string(&args[1]).unwrap();
            let evaled = eval::run(&code, &mut env);
            eval::display_object(evaled);
        },
        _ => panic!("too many arguments."),
    };
}
