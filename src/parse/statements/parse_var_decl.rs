use crate::{
    ast::{
        decl::VarDecl,
        stmt::{Stmt, StmtKind},
    },
    globals::next_declaration_id,
    parse::{Parser, ParsingError},
    tokenize::{KeywordKind, PunctuationKind, TokenKind},
};

impl Parser {
    pub fn parse_var_decl(&mut self) -> Result<Stmt, ParsingError> {
        let documentation = self.consume_optional_doc();

        let start_offset = self.offset;

        self.consume_keyword(KeywordKind::Let)?;

        let name = self.consume_identifier()?;

        let constraint =
            if self.match_token(0, TokenKind::Punctuation(PunctuationKind::Col)) {
                self.advance();
                Some(self.parse_type_annotation(0)?)
            } else {
                None
            };

        self.consume_punctuation(PunctuationKind::Eq)?;

        let value = self.parse_expr(0)?;

        self.consume_punctuation(PunctuationKind::SemiCol)?;

        let span = self.get_span(start_offset, self.offset - 1)?;

        let id = next_declaration_id();

        Ok(Stmt {
            kind: StmtKind::VarDecl(VarDecl {
                id,
                documentation,
                identifier: name,
                constraint,
                value,
            }),
            span,
        })
    }
}
