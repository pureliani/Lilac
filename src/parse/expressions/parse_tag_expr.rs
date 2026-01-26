use crate::{
    ast::expr::{Expr, ExprKind},
    globals::TAG_INTERNER,
    parse::{Parser, ParsingError},
    tokenize::PunctuationKind,
};

impl Parser {
    pub fn parse_tag_expr(&mut self) -> Result<Expr, ParsingError> {
        let start_offset = self.offset;
        self.consume_punctuation(PunctuationKind::Hash)?;
        let identifier = self.consume_identifier()?;
        let span = self.get_span(start_offset, self.offset - 1)?;

        let tag_id = TAG_INTERNER.intern(&identifier.name);

        Ok(Expr {
            kind: ExprKind::Tag(tag_id),
            span,
        })
    }
}
