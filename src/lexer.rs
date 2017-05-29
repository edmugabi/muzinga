use std::str::Chars;
use std::iter::Peekable;

#[derive(Debug,PartialEq)]
pub enum Token {
    Op(OpToken),
    Int(String),
    Ref(String),
    NodeRef(i32,String),
    LParen,
    RParen,
    Assign,
    EOF,
    Illegal,
}

#[derive(Debug,PartialEq)]
pub enum OpToken {
    Plus,
    Minus,
    Asterisk,
    FSlash,
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer { input: input.chars().peekable() }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop  {
            let token = self.next_token();
            if token != Token::EOF {
                tokens.push(token);
            } else {
                break;
            }
        }
        
        tokens
    }


    fn next_char(&mut self) -> Option<char> {
        self.input.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn peek_is_alphanumeric(&mut self) -> bool {
        match self.peek_char() {
            Some(&ch) => is_letter(ch) || ch.is_numeric(),
            None => false,
        }
    }

    fn peek_is_alphabetic(&mut self) -> bool {
        match self.peek_char() {
            Some(&ch) => ch.is_alphabetic(),
            None => false,
        }
    }

    fn read_identifier(&mut self, first: char) -> String {
        let mut ident = String::new();
        ident.push(first);

        while self.peek_is_alphanumeric() {
            ident.push(self.next_char().unwrap());
        }

        ident
    }

    fn read_col_ident(&mut self, first: char) -> String {
        let mut ident = String::new();
        ident.push(first);

        while self.peek_is_alphabetic() {
            ident.push(self.next_char().unwrap());
        }

        ident
    }

    fn read_number(&mut self, first: char) -> String {
        let mut number = String::new();
        number.push(first);

        while let Some(&c) = self.peek_char() {
            if !c.is_numeric() {
                break;
            }
            number.push(self.next_char().unwrap());
        }

        number
    }

    fn eat_whitespace(&mut self) {
        while let Some(&c) = self.peek_char() {
            if !c.is_whitespace() {
                break;
            }
            self.next_char();
        }
    }

    fn next_token(&mut self) -> Token {
        self.eat_whitespace();
        match self.next_char() {
            Some('=') => Token::Assign,
            Some('+') => Token::Op(OpToken::Plus),
            Some('-') => Token::Op(OpToken::Minus),
            Some('(') => Token::LParen,
            Some(')') => Token::RParen,
            Some('*') => Token::Op(OpToken::Asterisk),
            Some('/') => Token::Op(OpToken::FSlash),
            Some(ch @ _) => {
                if is_letter(ch) {

                    let col = self.read_col_ident(ch);
                    match self.next_char() {
                        Some(ch @ _) => {
                            if ch.is_numeric() {
                                let row = self.read_number(ch).parse::<i32>().unwrap();
                                return Token::NodeRef(row,col);
                            }
                        },
                        None => return Token::Ref(col),
                    }
                    let ident_left = self.read_identifier(ch);
                    lookup_ident(&(col + &ident_left))

                } else if ch.is_numeric() {
                    Token::Int(self.read_number(ch))
                }  else {
                    Token::Illegal
                }
            },

            None => Token::EOF,
        }
    }
}

fn lookup_ident(ident: &str) -> Token {
    match ident {
        _      => Token::Ref(ident.to_owned()),
    }
}



fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

#[test]
fn test_next_token() {
    let mut l = Lexer::new("c1 + 6");
    assert_eq!(Token::NodeRef(1,"c".to_owned()), l.next_token());
    assert_eq!(Token::Op(OpToken::Plus), l.next_token());
    assert_eq!(Token::Int("6".to_owned()), l.next_token());
    assert_eq!(Token::EOF, l.next_token());
}

#[test]
fn test_parens() {
    let mut l = Lexer::new("(-3) / (-7 * --6)");
    let res = vec![
                Token::LParen,
                Token::Op(OpToken::Minus),
                Token::Int("3".to_owned()),
                Token::RParen,
                Token::Op(OpToken::FSlash),
                Token::LParen,
                Token::Op(OpToken::Minus),
                Token::Int("7".to_owned()),
                Token::Op(OpToken::Asterisk),
                Token::Op(OpToken::Minus),
                Token::Op(OpToken::Minus),
                Token::Int("6".to_owned()),
                Token::RParen,
    ];
    assert_eq!(res,l.lex());
}

#[test]
fn test_assign_sc() {
    let sc = "b1 = c1 + 5";
    let mut l = Lexer::new(sc);
    let expected_res = vec![
                        Token::NodeRef(1,"b".to_owned()),
                        Token::Assign,
                        Token::NodeRef(1,"c".to_owned()),
                        Token::Op(OpToken::Plus),
                        Token::Int("5".to_owned()),
    ];
    assert_eq!(expected_res, l.lex());
}















//
