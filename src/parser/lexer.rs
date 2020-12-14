use std::str::CharIndices;

pub enum Tok {
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    Alignas,
    Alignof,
    Atomic,
    Bool,
    Complex,
    Generic,
    Imaginary, 
    Noreturn,
    StaticAssert,
    ThreadLocal,

    Identifier,
    Constant,
    StringLiteral,
    //Punctuators
    LeftSquareBracket,
    RightSquaredBracket,
    LeftCurvedParenthesis,
    RightCurvedParenthesis,
    LeftCurlyBrace,
    RightCurlyBrace,
    Dot,
    Arrow,
    TwoPluses,
    TwoMinuses,
    Ampersand,
    Star,
    Plus,
    Minus,
    Wave,
    ExclamationMark,
    Slash,
    Percent,
    TwoSmaller,
    TwoLager,
    Smaller,
    Lager,
    LeOrEq,
    GeOrEq,
    Equal,
    NotEq,
    CheckMark,
    VerticalLine,
    TwoAmpersand,
    TwoVerticalLine,
    QuestionMark,
    Colon,
    Semicolon,
    ThreeDots,
    Asign,
    StarAsign,
    SlashAsign,
    VerticalAsign,
    Comma,
    Lattice,
    TwoLattice,
    S1,
    S2,
    S3,
    S4,
    S5,
    S6,
}

enum LexicalError {
    
}

pub struct Lexer<'input> {
    chars: CharIndices<'input>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer { chars: input.char_indices() }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.chars.next() {
                Some((i, ' ')) => return Some(Ok((i, Tok::Space, i+1))),
                Some((i, '\t')) => return Some(Ok((i, Tok::Tab, i+1))),
                Some((i, '\n')) => return Some(Ok((i, Tok::Linefeed, i+1))),

                None => return None, // End of file
                _ => continue, // Comment; skip this character
            }
        }
    }
}

// keyword

// "auto" "break" "case" "char" "const" 
// "continue" "default" "do" "double" "else"
// "enum" "extern" "float" "for" "goto" "if"
// "inline" "int" "long" "register" "restrict" "return"
// "short" "signed" "sizeof" "static" "struct" 
// "switch" "typedef" "union" "unsigned" "void"
// "volatile" "while" "_Alignas" "_Alignof" "_Atomic"
// "_Bool" "_Complex" "_Generic" "_Imaginary" "_Noreturn"
// "_Static_assert" "_Thread_local"


//////////////////////////////////////////////////
/// Constants ////////////////////////////////////
//////////////////////////////////////////////////

// pub Constant: Box<expr::Expr> = {
//     IntegerConstant => ,
//     FloatingConstant => ,
//     EnumerationConstant => ,
//     CharacterConstant, 
// };

// IntegerConstant: = {
//     decimal-constant integer-suffixopt
//     octal-constant integer-suffixopt
//     hexadecimal-constant integer-suffixopt
// };

// DecimalConstant: = {
//     nonzero-digit
//     decimal-constant digit
// };

// OctalConstant: = {
//     "0"
//     OctalConstant OctalDigit
// };

// HexadecimalConstant: = {
//     HexadecimalPrefix HexadecimalDigit
//     HexadecimalConstant HexadecimalDigit
// };

// HexadecimalPrefix: = {
//     "0x", 
//     "0X",
// };

// NonzeroDigit: = {
//     "1" "2" "3" "4" "5" "6" "7" "8" "9"
// };

// OctalDigit: = {
//     "0" "1" "2" "3" "4" "5" "6" "7"
// };

// CharacterConstantPrefix: expr::CharLiteralPrefix = {
//     "L" => expr::CharLiteralPrefix::L,
//     "u" => expr::CharLiteralPrefix::LCU,
//     "U" => expr::CharLiteralPrefix::UCU, 
// };

// pub CharacterConstant: expr::CharLiteral = {
//     <pref: CharacterConstantPrefix> "'" <val: CCharSequence> "'" 
//         => expr::CharLiteral{prefix: Some(pref), value: val},
//     "'" <val: CCharSequence> "'" 
//         => expr::CharLiteral{prefix: None, value: val},
// };

// //TODO: this is a very naive implementation that only partially conforms to the standard
// CCharSequence: u8 = <s:r"[[:alpha:]]"> => s.bytes().nth(0).unwrap();

// StringLiteral: Box<expr::Expr> = 
//     <prefix: EncodingPrefix?> "\"" <value: SCharSequence?> "\"" 
//         => expr::Expr::Literal(expr::Literal::Str(expr::StringLiteral{prefix, value}))
// ;

// EncodingPrefix: expr::StringLiteralPrefix = {
//     "u8" => expr::StringLiteralPrefix::U8,
//     "u" => expr::StringLiteralPrefix::LCU,
//     "U" => expr::StringLiteralPrefix::UCU,
//     "L" => expr::StringLiteralPrefix::L,
// };

// SCharSequence = r"[[:alpha:]]+;

// "[" "]" "(" ")" "{" "}" "." "->"
// "++" "--" "&" "*" "+" "-" "~" '!"
// "/" "%" "<<" ">>" "<" ">" "<=" ">="
// "==" "!=" "^" "|" "&&" "||"
// "?" ":" ";" "..."
// "=" "*=" "/=" "%=" "+=" "-=" "<<="
// ">>=" "&=" "^=" "|=" "," "#" "##"
// "<:" ":>" "<%" "%>" "%:" "%:%:"

