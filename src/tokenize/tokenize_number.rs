use super::{is_digit, is_letter, NumberKind, TokenizationErrorKind, Tokenizer};

impl<'a> Tokenizer<'a> {
    pub fn tokenize_number(&mut self) -> Result<NumberKind, TokenizationErrorKind> {
        let start = self.grapheme_offset;
        let mut has_dot = false;

        while let Some(c) = self.current() {
            if is_digit(c) {
                self.consume();
            } else if c == "." && !has_dot {
                has_dot = true;
                self.consume();
            } else if c == "." && has_dot {
                return Err(TokenizationErrorKind::InvalidFloatingNumber);
            } else if is_letter(c) {
                self.consume();
            } else {
                break;
            }
        }

        let number_str = self.slice(start, self.grapheme_offset);

        parse_number(number_str)
    }
}

const SUFFIX_INFOS: [(&str, bool); 10] = [
    ("f64", true),
    ("f32", true),
    ("u64", false),
    ("u32", false),
    ("u16", false),
    ("u8", false),
    ("i64", false),
    ("i32", false),
    ("i16", false),
    ("i8", false),
];

fn parse_number(full_number_str: &str) -> Result<NumberKind, TokenizationErrorKind> {
    for (suffix_str, is_float_suffix) in SUFFIX_INFOS {
        if let Some(numeric_part) = full_number_str.strip_suffix(suffix_str) {
            if !is_float_suffix && numeric_part.contains('.') {
                // e.g. "1.0u8"
                return Err(TokenizationErrorKind::InvalidIntegerNumber);
            }
            if is_float_suffix && numeric_part == "." {
                // e.g. ".f32"
                return Err(TokenizationErrorKind::InvalidFloatingNumber);
            }

            let result: Result<NumberKind, _> = match suffix_str {
                "f64" => numeric_part
                    .parse::<f64>()
                    .map(NumberKind::F64)
                    .or(Err(TokenizationErrorKind::InvalidFloatingNumber)),
                "f32" => numeric_part
                    .parse::<f32>()
                    .map(NumberKind::F32)
                    .or(Err(TokenizationErrorKind::InvalidFloatingNumber)),
                "usize" => numeric_part
                    .parse::<usize>()
                    .map(NumberKind::USize)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "u64" => numeric_part
                    .parse::<u64>()
                    .map(NumberKind::U64)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "u32" => numeric_part
                    .parse::<u32>()
                    .map(NumberKind::U32)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "u16" => numeric_part
                    .parse::<u16>()
                    .map(NumberKind::U16)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "u8" => numeric_part
                    .parse::<u8>()
                    .map(NumberKind::U8)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "isize" => numeric_part
                    .parse::<isize>()
                    .map(NumberKind::ISize)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "i64" => numeric_part
                    .parse::<i64>()
                    .map(NumberKind::I64)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "i32" => numeric_part
                    .parse::<i32>()
                    .map(NumberKind::I32)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "i16" => numeric_part
                    .parse::<i16>()
                    .map(NumberKind::I16)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "i8" => numeric_part
                    .parse::<i8>()
                    .map(NumberKind::I8)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                _ => unreachable!("Suffix matched in loop but not in match block"),
            };

            return result;
        }
    }

    if full_number_str.contains('.') {
        full_number_str
            .parse::<f64>()
            .map(NumberKind::F64)
            .or(Err(TokenizationErrorKind::InvalidFloatingNumber))
    } else {
        full_number_str
            .parse::<i32>()
            .map(NumberKind::I32)
            .or(Err(TokenizationErrorKind::InvalidIntegerNumber))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{ModulePath, Position, Span},
        globals::reset_globals,
        tokenize::{NumberKind, Token, TokenKind, Tokenizer},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn tokenizes_numbers_with_suffixes() {
        reset_globals();
        let path = ModulePath::default();

        let test_cases = vec![
            (
                "1.",
                NumberKind::F64(1.0f64),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 3,
                        byte_offset: 2,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1.5",
                NumberKind::F64(1.5f64),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 4,
                        byte_offset: 3,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1",
                NumberKind::I64(1i64),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 2,
                        byte_offset: 1,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1.5f64",
                NumberKind::F64(1.5f64),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 7,
                        byte_offset: 6,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1.5f32",
                NumberKind::F32(1.5f32),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 7,
                        byte_offset: 6,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1f64",
                NumberKind::F64(1f64),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1f32",
                NumberKind::F32(1f32),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1u8",
                NumberKind::U8(1u8),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 4,
                        byte_offset: 3,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1u16",
                NumberKind::U16(1u16),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1u32",
                NumberKind::U32(1u32),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1u64",
                NumberKind::U64(1u64),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1i8",
                NumberKind::I8(1i8),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 4,
                        byte_offset: 3,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1i16",
                NumberKind::I16(1i16),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1i32",
                NumberKind::I32(1i32),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1i64",
                NumberKind::I64(1i64),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
        ];

        for (input, expected_kind, span) in test_cases {
            let (tokens, errors) = Tokenizer::tokenize(input, path.clone());

            assert_eq!(errors, vec![]);

            assert_eq!(
                tokens,
                vec![Token {
                    span,
                    kind: TokenKind::Number(expected_kind),
                }]
            );
        }
    }
}
