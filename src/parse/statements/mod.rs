pub mod parse_assignment_stmt;
pub mod parse_break_stmt;
pub mod parse_continue_stmt;
pub mod parse_expr_stmt;
pub mod parse_from_stmt;
pub mod parse_return_stmt;
pub mod parse_type_alias_decl;
pub mod parse_var_decl;
pub mod parse_while_stmt;

use crate::{
    ast::{
        stmt::{Stmt, StmtKind},
        Span,
    },
    parse::{Parser, ParsingErrorKind},
    tokenize::{KeywordKind, PunctuationKind, TokenKind},
};

use super::ParsingError;

pub fn is_start_of_stmt(token_kind: &TokenKind) -> bool {
    matches!(
        token_kind,
        TokenKind::Keyword(
            KeywordKind::From
                | KeywordKind::While
                | KeywordKind::Return
                | KeywordKind::Break
                | KeywordKind::Continue
                | KeywordKind::Type
                | KeywordKind::Let
                | KeywordKind::Export
        ) | TokenKind::Doc(_)
    )
}

impl Parser {
    pub fn parse_stmt(&mut self) -> Result<Stmt, ParsingError> {
        let result = self.parse_stmt_no_sync();

        if result.is_err() {
            self.synchronize_stmt();
        }

        result
    }

    pub fn parse_stmt_no_sync(&mut self) -> Result<Stmt, ParsingError> {
        let mut lookahead_index = 0;

        let has_doc = if matches_token!(self, lookahead_index, TokenKind::Doc(_)) {
            lookahead_index += 1;
            true
        } else {
            false
        };

        let has_export = if matches_token!(
            self,
            lookahead_index,
            TokenKind::Keyword(KeywordKind::Export)
        ) {
            lookahead_index += 1;
            true
        } else {
            false
        };

        if matches_token!(self, lookahead_index, TokenKind::Keyword(KeywordKind::Type)) {
            return self.parse_type_alias_decl();
        }

        if matches_token!(self, lookahead_index, TokenKind::Keyword(KeywordKind::Let)) {
            return self.parse_var_decl();
        }

        if matches_token!(self, lookahead_index, TokenKind::Keyword(KeywordKind::Fn)) {
            let expr = self.parse_fn_expr()?;
            return Ok(Stmt {
                span: expr.span.clone(),
                kind: StmtKind::Expression(expr),
            });
        }

        if has_export {
            let invalid_token = self
                .tokens
                .get(self.offset + lookahead_index)
                .unwrap_or_else(|| self.tokens.last().unwrap());

            return Err(ParsingError {
                kind: ParsingErrorKind::ExpectedStatementOrExpression {
                    found: invalid_token.clone(),
                },
                span: invalid_token.span.clone(),
            });
        }

        if has_doc {
            return Err(ParsingError {
                kind: ParsingErrorKind::DocMustBeFollowedByDeclaration,
                span: self.tokens.get(self.offset).unwrap().span.clone(),
            });
        }

        if matches_token!(self, 0, TokenKind::Keyword(KeywordKind::From)) {
            return self.parse_from_stmt();
        }
        if matches_token!(self, 0, TokenKind::Keyword(KeywordKind::While)) {
            return self.parse_while_stmt();
        }
        if matches_token!(self, 0, TokenKind::Keyword(KeywordKind::Return)) {
            return self.parse_return_stmt();
        }
        if matches_token!(self, 0, TokenKind::Keyword(KeywordKind::Break)) {
            return self.parse_break_stmt();
        }
        if matches_token!(self, 0, TokenKind::Keyword(KeywordKind::Continue)) {
            return self.parse_continue_stmt();
        }

        let lhs = self.parse_expr(0)?;

        if matches_token!(self, 0, TokenKind::Punctuation(PunctuationKind::Eq))
            && !matches_token!(self, 1, TokenKind::Punctuation(PunctuationKind::Eq))
        {
            self.parse_assignment_stmt(lhs)
        } else {
            let mut end_span = lhs.span.clone();
            if matches_token!(self, 0, TokenKind::Punctuation(PunctuationKind::SemiCol)) {
                end_span = self.current().unwrap().span.clone();
                self.advance();
            }

            Ok(Stmt {
                span: Span {
                    start: lhs.span.start,
                    end: end_span.end,
                    path: self.path.clone(),
                },
                kind: StmtKind::Expression(lhs),
            })
        }
    }

    pub fn synchronize_stmt(&mut self) {
        loop {
            match self.current() {
                Some(token) => {
                    if token.kind == TokenKind::Punctuation(PunctuationKind::SemiCol) {
                        self.advance();
                        return;
                    }

                    self.advance();
                }
                None => return,
            }
        }
    }
}
