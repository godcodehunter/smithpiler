use sana::{Spanned, Sana};

#[derive(Debug)]
pub struct ParserError(usize);

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Unrecognizable token encountered: {}", self.0))
    }
}

type ParserItem = Result<(usize, Token, usize), ParserError>;

pub fn token_iter<'input>(input: &'input str, dump_lexer: bool) -> impl Iterator<Item=ParserItem> + 'input {
    Token::lexer(input)
        .map(move |tok| {
            if dump_lexer {
                println!("{}:{} {:?} {}", tok.start, tok.end, tok.value, &input[tok.start..tok.end]);
            }
            tok
        })
        .filter(|tok| tok.value != Token::Whitespace)
        .map(|tok| match tok {
            Spanned { value: Token::Error, start, ..} =>
                Err(ParserError(start)),
            Spanned { value, start, end} =>
                Ok((start, value, end)),
        })
}

#[derive(Debug, Clone, Copy, PartialEq, Sana)]
pub enum Token {
    #[token("auto", priority = 1)]
    Auto,
    #[token("break", priority = 1)]
    Break,
    #[token("case", priority = 1)]
    Case,
    #[token("char", priority = 1)]
    Char,
    #[token("const", priority = 1)]
    Const,
    #[token("continue", priority = 1)]
    Continue,
    #[token("default", priority = 1)]
    Default,
    #[token("do", priority = 1)]
    Do,
    #[token("double", priority = 1)]
    Double,
    #[token("else", priority = 1)]
    Else,
    #[token("enum", priority = 1)]
    Enum,
    #[token("extern", priority = 1)]
    Extern,
    #[token("float", priority = 1)]
    Float,
    #[token("for", priority = 1)]
    For,
    #[token("goto", priority = 1)]
    Goto,
    #[token("if", priority = 1)]
    If,
    #[token("inline", priority = 1)]
    Inline,
    #[token("int", priority = 1)]
    Int,
    #[token("long", priority = 1)]
    Long,
    #[token("register", priority = 1)]
    Register,
    #[token("restrict", priority = 1)]
    Restrict,
    #[token("return", priority = 1)]
    Return,
    #[token("short", priority = 1)]
    Short,
    #[token("signed", priority = 1)]
    Signed,
    #[token("sizeof", priority = 1)]
    Sizeof,
    #[token("static", priority = 1)]
    Static,
    #[token("struct", priority = 1)]
    Struct,
    #[token("switch", priority = 1)]
    Switch,
    #[token("typedef", priority = 1)]
    Typedef,
    #[token("union", priority = 1)]
    Union,
    #[token("unsigned", priority = 1)]
    Unsigned,
    #[token("void", priority = 1)]
    Void,
    #[token("volatile", priority = 1)]
    Volatile,
    #[token("while", priority = 1)]
    While,
    #[token("_Alignas", priority = 1)]
    Alignas,
    #[token("_Alignof", priority = 1)]
    Alignof,
    #[token("_Atomic", priority = 1)]
    Atomic,
    #[token("_Bool", priority = 1)]
    Bool,
    #[token("_Complex", priority = 1)]
    Complex,
    #[token("_Generic", priority = 1)]
    Generic,
    #[token("_Imaginary", priority = 1)]
    Imaginary, 
    #[token("_Noreturn", priority = 1)]
    Noreturn,
    #[token("_Static_assert", priority = 1)]
    StaticAssert,
    #[token("_Thread_local", priority = 1)]
    ThreadLocal,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")] 
    Identifier,

    #[regex("[0-9]+")]
    Constant,

    #[regex("\"\"")]
    StringLiteral,

    #[regex("@@TODO")]
    TypedefName,
    
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("(")]
    LParenthesis,
    #[token(")")]
    RParenthesis,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(".")]
    Dot,
    #[token("->")]
    Arrow,
    #[token("++")]
    TwoPluses,
    #[token("--")]
    TwoMinuses,
    #[token("&")]
    Ampersand,
    #[token("*")]
    Asterisk ,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("~")]
    Tilde,
    #[token("!")]
    ExclamationMark,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<<")]
    TwoSmaller,
    #[token(">>")]
    TwoLager,
    #[token("<")]
    Smaller,
    #[token(">")]
    Lager,
    #[token("<=")]
    LeOrEq,
    #[token(">=")]
    GeOrEq,
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEq,
    #[token("^")]
    Caret,
    #[token("|")]
    Bar,
    #[token("&&")]
    WAmpersand,
    #[token("||")]
    WBar,
    #[token("?")]
    QuestionMark,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("...")]
    ThreeDots,
    #[token("=")]
    Assign,
    #[token("*=")]
    AsteriskAssign,
    #[token("/=")]
    SlashAssign,
    #[token("%=")]
    PercentAssign,
    #[token("+=")]
    PlusAssign,
    #[token("-=")]
    MinusWHyphen,
    #[token("<<=")]
    LGuillemetsAssign,
    #[token(">>=")]
    RGuillemetsAssign,
    #[token("&=")]
    AmpersandAssign,
    #[token("^=")]
    CaretAssign,
    #[token("|=")]
    VBarAssign,
    #[token(",")]
    Comma,

    #[regex("[ \t\r\n]+")]
    Whitespace,

    #[error]
    Error,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //     if tok.is_err() {
        //         dbg!(tok.unwrap_err());
        //         break;
        //     }
        //     let c = tok.unwrap();
        // println!("{}:{} {:?} {}", tok.0, tok.2, tok.1, &input[tok.0..tok.2]);
        todo!()
    }
}




