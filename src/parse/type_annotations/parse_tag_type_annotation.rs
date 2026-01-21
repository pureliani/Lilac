use crate::{
    ast::type_annotation::{TagAnnotation, TypeAnnotation, TypeAnnotationKind},
    parse::{Parser, ParsingError},
    tokenize::{PunctuationKind, TokenKind},
};

impl Parser {
    pub fn parse_tag_type_annotation(&mut self) -> Result<TypeAnnotation, ParsingError> {
        let start_offset = self.offset;

        let mut tags = vec![];

        loop {
            let start_offset = self.offset;
            self.consume_punctuation(PunctuationKind::Hash)?;
            let identifier = self.consume_identifier()?;
            let value_type =
                if self.match_token(0, TokenKind::Punctuation(PunctuationKind::LParen)) {
                    self.consume_punctuation(PunctuationKind::LParen)?;
                    let value_type = self.parse_type_annotation(0)?;
                    self.consume_punctuation(PunctuationKind::RParen)?;
                    Some(Box::new(value_type))
                } else {
                    None
                };
            let span = self.get_span(start_offset, self.offset - 1)?;

            tags.push(TagAnnotation {
                identifier,
                value_type,
                span,
            });

            if self.match_token(0, TokenKind::Punctuation(PunctuationKind::Or)) {
                self.advance();
                continue;
            } else {
                break;
            }
        }
        let span = self.get_span(start_offset, self.offset - 1)?;

        if tags.len() > 1 {
            Ok(TypeAnnotation {
                kind: TypeAnnotationKind::Union(tags),
                span,
            })
        } else {
            Ok(TypeAnnotation {
                kind: TypeAnnotationKind::Tag(
                    tags.pop().expect("Expected at least single tag item"),
                ),
                span,
            })
        }
    }
}
