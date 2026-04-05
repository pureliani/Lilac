use super::{PunctuationKind, Tokenizer};

impl<'a> Tokenizer<'a> {
    pub fn tokenize_punctuation(&mut self, punct: &str) -> Option<PunctuationKind> {
        match punct {
            ":" => match self.peek(1) {
                Some(":") => {
                    self.consume();
                    self.consume();
                    Some(PunctuationKind::DoubleCol)
                }
                _ => {
                    self.consume();
                    Some(PunctuationKind::Col)
                }
            },
            "|" => match self.peek(1) {
                Some("|") => {
                    self.consume();
                    self.consume();
                    Some(PunctuationKind::DoubleOr)
                }
                _ => {
                    self.consume();
                    Some(PunctuationKind::Or)
                }
            },
            "&" => match self.peek(1) {
                Some("&") => {
                    self.consume();
                    self.consume();
                    Some(PunctuationKind::DoubleAnd)
                }
                _ => {
                    self.consume();
                    Some(PunctuationKind::And)
                }
            },
            "=" => match self.peek(1) {
                Some("=") => {
                    self.consume();
                    self.consume();
                    Some(PunctuationKind::DoubleEq)
                }
                _ => {
                    self.consume();
                    Some(PunctuationKind::Eq)
                }
            },
            "<" => match self.peek(1) {
                Some("=") => {
                    self.consume();
                    self.consume();
                    Some(PunctuationKind::Lte)
                }
                _ => {
                    self.consume();
                    Some(PunctuationKind::Lt)
                }
            },
            ">" => match self.peek(1) {
                Some("=") => {
                    self.consume();
                    self.consume();
                    Some(PunctuationKind::Gte)
                }
                _ => {
                    self.consume();
                    Some(PunctuationKind::Gt)
                }
            },
            "!" => match self.peek(1) {
                Some("=") => {
                    self.consume();
                    self.consume();
                    Some(PunctuationKind::NotEq)
                }
                _ => {
                    self.consume();
                    Some(PunctuationKind::Not)
                }
            },
            ";" => {
                self.consume();
                Some(PunctuationKind::SemiCol)
            }
            "." => {
                self.consume();
                Some(PunctuationKind::Dot)
            }
            "(" => {
                self.consume();
                Some(PunctuationKind::LParen)
            }
            ")" => {
                self.consume();
                Some(PunctuationKind::RParen)
            }
            "[" => {
                self.consume();
                Some(PunctuationKind::LBracket)
            }
            "]" => {
                self.consume();
                Some(PunctuationKind::RBracket)
            }
            "+" => {
                self.consume();
                Some(PunctuationKind::Plus)
            }
            "-" => {
                self.consume();
                Some(PunctuationKind::Minus)
            }
            "*" => {
                self.consume();
                Some(PunctuationKind::Star)
            }
            "/" => {
                self.consume();
                Some(PunctuationKind::Slash)
            }
            "%" => {
                self.consume();
                Some(PunctuationKind::Percent)
            }
            "," => {
                self.consume();
                Some(PunctuationKind::Comma)
            }
            "$" => {
                self.consume();
                Some(PunctuationKind::Dollar)
            }
            "?" => {
                self.consume();
                Some(PunctuationKind::Question)
            }
            "#" => {
                self.consume();
                Some(PunctuationKind::Hash)
            }
            _ => None,
        }
    }
}
