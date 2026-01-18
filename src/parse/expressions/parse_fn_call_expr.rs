use crate::{
    ast::expr::{Expr, ExprKind},
    parse::{Parser, ParsingError},
    tokenize::{PunctuationKind, TokenKind},
};

impl Parser {
    pub fn parse_fn_call_args(&mut self) -> Result<Vec<Expr>, ParsingError> {
        self.consume_punctuation(PunctuationKind::LParen)?;
        let args = self.comma_separated(
            |p| p.parse_expr(0),
            |p| p.match_token(0, TokenKind::Punctuation(PunctuationKind::RParen)),
        )?;
        self.consume_punctuation(PunctuationKind::RParen)?;
        Ok(args)
    }

    pub fn parse_fn_call_expr(&mut self, left: Expr) -> Result<Expr, ParsingError> {
        let start_offset = self.offset;

        let args = self.parse_fn_call_args()?;
        let mut span = left.span.clone();
        let end = self.get_span(start_offset, self.offset - 1)?;
        span.end = end.end;

        Ok(Expr {
            kind: ExprKind::FnCall {
                left: Box::new(left),
                args,
            },
            span,
        })
    }
}
