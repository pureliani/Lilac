use crate::{
    ast::{
        decl::{FnDecl, Param},
        expr::{Expr, ExprKind},
        type_annotation::{TypeAnnotation, TypeAnnotationKind},
    },
    globals::next_declaration_id,
    parse::{Parser, ParsingError},
    tokenize::{KeywordKind, PunctuationKind, TokenKind},
};

impl Parser {
    pub fn parse_fn_expr(&mut self) -> Result<Expr, ParsingError> {
        let documentation = self.consume_optional_doc();

        let start_offset = self.offset;

        let is_exported = if self.match_token(0, TokenKind::Keyword(KeywordKind::Export))
        {
            self.consume_keyword(KeywordKind::Export)?;
            true
        } else {
            false
        };

        self.consume_keyword(KeywordKind::Fn)?;
        let identifier = self.consume_identifier()?;
        self.consume_punctuation(PunctuationKind::LParen)?;
        let params = self.comma_separated(
            |p| {
                let identifier = p.consume_identifier()?;
                p.consume_punctuation(PunctuationKind::Col)?;
                let constraint = p.parse_type_annotation(0)?;

                Ok(Param {
                    constraint,
                    identifier,
                })
            },
            |p| p.match_token(0, TokenKind::Punctuation(PunctuationKind::RParen)),
        )?;
        self.consume_punctuation(PunctuationKind::RParen)?;

        let return_type =
            if self.match_token(0, TokenKind::Punctuation(PunctuationKind::Col)) {
                self.consume_punctuation(PunctuationKind::Col)?;
                self.parse_type_annotation(0)?
            } else {
                TypeAnnotation {
                    kind: TypeAnnotationKind::Void,
                    span: self.get_span(start_offset, self.offset - 1)?,
                }
            };

        let body = self.parse_codeblock_expr()?;

        let id = next_declaration_id();

        Ok(Expr {
            kind: ExprKind::Fn(Box::new(FnDecl {
                id,
                identifier,
                params,
                return_type,
                body,
                documentation,
                is_exported,
            })),
            span: self.get_span(start_offset, self.offset - 1)?,
        })
    }
}
