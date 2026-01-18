use crate::{
    ast::{
        expr::{BlockContents, Expr},
        stmt::{Stmt, StmtKind},
        Span,
    },
    parse::{statements::is_start_of_stmt, Parser, ParsingError, ParsingErrorKind},
    tokenize::{PunctuationKind, TokenKind},
};

use super::is_start_of_expr;

impl Parser {
    pub fn parse_codeblock_expr(&mut self) -> Result<BlockContents, ParsingError> {
        let start_offset = self.offset;
        self.consume_punctuation(PunctuationKind::LBrace)?;

        let mut statements = Vec::new();
        let mut final_expr: Option<Box<Expr>> = None;

        loop {
            if self.match_token(0, TokenKind::Punctuation(PunctuationKind::RBrace)) {
                break;
            }

            let current_token = self
                .current()
                .ok_or_else(|| self.unexpected_end_of_input())?;

            if is_start_of_stmt(&current_token.kind) {
                if let Some(old_expr) = final_expr.take() {
                    statements.push(Stmt {
                        span: old_expr.span.clone(),
                        kind: StmtKind::Expression(*old_expr),
                    })
                }

                statements.push(self.parse_stmt()?);
            } else if is_start_of_expr(&current_token.kind) {
                if let Some(old_expr) = final_expr.take() {
                    statements.push(Stmt {
                        span: old_expr.span.clone(),
                        kind: StmtKind::Expression(*old_expr),
                    })
                }

                let expr = self.parse_expr(0)?;

                if self.match_token(0, TokenKind::Punctuation(PunctuationKind::Eq))
                    && !self.match_token(1, TokenKind::Punctuation(PunctuationKind::Eq))
                {
                    let stmt = self.parse_assignment_stmt(expr)?;
                    statements.push(stmt);
                } else if self
                    .match_token(0, TokenKind::Punctuation(PunctuationKind::SemiCol))
                {
                    let semi_token = self.current().unwrap();
                    let stmt = Stmt {
                        span: Span {
                            start: expr.span.start,
                            end: semi_token.span.end,
                            path: self.path.clone(),
                        },
                        kind: StmtKind::Expression(expr),
                    };
                    statements.push(stmt);
                    self.advance();
                } else {
                    final_expr = Some(Box::new(expr));
                }
            } else {
                return Err(ParsingError {
                    kind: ParsingErrorKind::ExpectedStatementOrExpression {
                        found: current_token.clone(),
                    },
                    span: current_token.span.clone(),
                });
            }
        }

        self.consume_punctuation(PunctuationKind::RBrace)?;

        let span = self.get_span(start_offset, self.offset - 1)?;

        Ok(BlockContents {
            statements,
            final_expr,
            span,
        })
    }
}
