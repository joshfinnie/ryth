use std::fmt::Display;
use std::hash::{Hash, Hasher};

/// Represents the location of a token within the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

/// Defines all single-character tokens that represent a fixed data value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantKind {
    Zero,       // Z (reserved var 0)
    Ten,        // T (reserved var 10)
    Input,      // Q (user input value)
    NullStr,    // k (empty string/null value)
    Whitespace, // d (single space constant)
    Newline,    // b (newline constant)
    LoopVar,    // z (implicit loop variable)
}

/// Defines the kind and type of value a token holds.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // --- Data-Carrying Tokens (Literals) ---
    FloatLiteral(f64),
    Str(String),
    Ident(String),

    // --- Constants (Fixed Values) ---
    Constant(ConstantKind),

    // --- Binary Prefix Operators (Require 2 Operands) ---
    Plus,    // + x y
    Minus,   // - x y
    Asterix, // * x y
    Slash,   // / x y
    Carrot,  // ^ x y (Exponent)
    Equal,   // = x y (Assignment/Equality, context-dependent)
    Percent, // % x y (Modulus)

    // --- Unary Prefix Operators (Require 1 Operand) ---
    Bang,       // ! x (Factorial)
    LowercaseT, // t x (Tail)
    LowercaseJ, // j x y (Join - join list y with separator x)

    // --- Control Flow & Statements ---
    F, // For loop
    V, // For loop (range 0 to x)
    W, // While loop
    D, // Def (Function definition)
    I, // If statement
    R, // Return
    A, // Double assignment into H and G
    B, // Bifurcate (creates [x, quoted(x)])

    // --- Delimiters & Grouping ---
    QuestionMark, // Ternary operator (?)
    LeftParen,    // Tuple grouping (
    // We assume RightParen/RightBracket/Comma tokens will be added if needed for complex syntax.
    Eof,
}

// Custom implementation of Hash for TokenKind to work in the Parser's HashMap.
// This is necessary because FloatLiteral(f64) is not directly hashable.
// We only care about the *type* of token for the map key, not the content.
impl Eq for TokenKind {}

impl Hash for TokenKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Only hash on the discriminant (type), ignoring the inner value of literals
        match self {
            TokenKind::FloatLiteral(_) => "FloatLiteral".hash(state),
            TokenKind::Str(_) => "Str".hash(state),
            TokenKind::Ident(_) => "Ident".hash(state),
            TokenKind::Constant(c) => c.hash(state),
            TokenKind::Plus => "Plus".hash(state),
            TokenKind::Minus => "Minus".hash(state),
            TokenKind::Asterix => "Asterix".hash(state),
            TokenKind::Slash => "Slash".hash(state),
            TokenKind::Carrot => "Carrot".hash(state),
            TokenKind::Equal => "Equal".hash(state),
            TokenKind::Percent => "Percent".hash(state),
            TokenKind::Bang => "Bang".hash(state),
            TokenKind::LowercaseT => "LowercaseT".hash(state),
            TokenKind::LowercaseJ => "LowercaseJ".hash(state),
            TokenKind::F => "F".hash(state),
            TokenKind::V => "V".hash(state),
            TokenKind::W => "W".hash(state),
            TokenKind::D => "D".hash(state),
            TokenKind::I => "I".hash(state),
            TokenKind::R => "R".hash(state),
            TokenKind::A => "A".hash(state),
            TokenKind::B => "B".hash(state),
            TokenKind::QuestionMark => "QuestionMark".hash(state),
            TokenKind::LeftParen => "LeftParen".hash(state),
            TokenKind::Eof => "Eof".hash(state),
        }
    }
}

/// The structure returned by the Lexer, containing type and position data.
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, start: usize, size: usize) -> Self {
        Token {
            kind,
            span: Span {
                start,
                end: start + size - 1,
            },
        }
    }
}

// Optional: Display implementation for easier debugging.
impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // ... (Your original display logic remains here)
        write!(f, "{:?}", self)
    }
}
