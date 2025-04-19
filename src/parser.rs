pub struct Lex {}

#[derive(Debug, PartialEq, Eq)]
pub enum StackOp {
    Push,
    Pop,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Instr {
    Add,
    Sub,
    Neg,
    Eq,
    Gt,
    Lt,
    And,
    Or,
    Not,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Seg {
    Argument,
    Local,
    Static,
    Constant,
    This,
    That,
    Pointer,
    Temp,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    StackOp(StackOp),
    Instr(Instr),
    Const(usize),
    Seg(Seg),
}

#[derive(Debug, PartialEq, Eq)]
pub enum LexError {
    InvalidToken(String),
}

impl Lex {
    pub fn lex(s: &str) -> Result<Vec<Vec<Token>>, LexError> {
        s.lines().try_fold(Vec::new(), |mut acc, line| {
            let tokens: Vec<_> = line
                .split_whitespace()
                .filter(|l| !l.is_empty())
                .map(|part| Self::match_token(part))
                .collect::<Result<_, _>>()?;

            if !tokens.is_empty() {
                acc.push(tokens);
            }

            Ok(acc)
        })
    }

    fn match_token(part: &str) -> Result<Token, LexError> {
        match part {
            "push" => Ok(Token::StackOp(StackOp::Push)),
            "pop" => Ok(Token::StackOp(StackOp::Pop)),
            "add" => Ok(Token::Instr(Instr::Add)),
            "sub" => Ok(Token::Instr(Instr::Sub)),
            "neg" => Ok(Token::Instr(Instr::Neg)),
            "eq" => Ok(Token::Instr(Instr::Eq)),
            "gt" => Ok(Token::Instr(Instr::Gt)),
            "lt" => Ok(Token::Instr(Instr::Lt)),
            "and" => Ok(Token::Instr(Instr::And)),
            "or" => Ok(Token::Instr(Instr::Or)),
            "not" => Ok(Token::Instr(Instr::Not)),
            "constant" => Ok(Token::Seg(Seg::Constant)),
            "local" => Ok(Token::Seg(Seg::Local)),
            "static" => Ok(Token::Seg(Seg::Static)),
            "this" => Ok(Token::Seg(Seg::This)),
            "that" => Ok(Token::Seg(Seg::That)),
            "pointer" => Ok(Token::Seg(Seg::Pointer)),
            "temp" => Ok(Token::Seg(Seg::Temp)),
            "argument" => Ok(Token::Seg(Seg::Argument)),
            _ => {
                let num = part.parse::<usize>();
                if let Ok(num) = num {
                    Ok(Token::Const(num))
                } else {
                    Err(LexError::InvalidToken(part.to_string()))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{Instr, LexError, Seg, StackOp, Token};

    use super::Lex;

    #[test]
    fn test_lex() {
        let s = r#"
        pop  constant a
        push constant b
        "#;
        let tokens = Lex::lex(s);

        // Assert that it errors with InvalidToken
        assert_eq!(tokens, Err(LexError::InvalidToken("a".to_string())));

        let s = r#"
        push constant 1
        push constant 2
        add
        pop local 0
        "#;
        let tokens = Lex::lex(s);
        let expected = vec![
            vec![Token::StackOp(StackOp::Push), Token::Seg(Seg::Constant), Token::Const(1)],
            vec![Token::StackOp(StackOp::Push), Token::Seg(Seg::Constant), Token::Const(2)],
            vec![Token::Instr(Instr::Add)],
            vec![Token::StackOp(StackOp::Pop), Token::Seg(Seg::Local), Token::Const(0)],
        ];
        assert_eq!(tokens, Ok(expected));
    }
}
