use ordered_float::OrderedFloat;

#[derive(PartialEq, Debug, Clone)]
pub struct Program(pub Vec<ExpressionStatement>);

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ExpressionStatement {
    pub expr: Expression,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionParams {
    pub value: Expression,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum PrefixOperator {
    Plus,
    Minus,
    Divide,
    Multiply,
    Exponent,
    Equal,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expression {
    FloatLiteral(OrderedFloat<f64>),
    Ident(String),
    Str(String),

    Function {
        params: Vec<FunctionParams>,
        body: Vec<ExpressionStatement>,
    },

    ForLoop {
        variable: String,
        iterable: Box<Expression>,
        body: Box<Expression>,
    },

    Range {
        start: Box<Expression>,
        end: Box<Expression>,
    },

    WhileLoop(usize),

    Ternary {
        var: Box<Expression>,
        true_: Box<Expression>,
        false_: Box<Expression>,
    },

    Factorial(usize),

    Tail(usize),

    Tuple(Box<Expression>, Box<Expression>),

    ReadInput(Option<String>),

    Prefix {
        operator: PrefixOperator,
        first_expr: Box<Expression>,
        second_expr: Box<Expression>,
    },

    If,
    Return,

    Join {
        separator: Box<Expression>,
        list: Box<Expression>,
    },

    Bifurcate(Box<Expression>),

    Head(Box<Expression>),

    Assign {
        variable: String,
        value: Box<Expression>,
    },

    DoubleAssign(Box<Expression>),
}
