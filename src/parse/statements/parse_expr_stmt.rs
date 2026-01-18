use crate::{
    ast::{
        expr::Expr,
        stmt::{Stmt, StmtKind},
    },
    parse::{Parser, ParsingError},
};

impl Parser {
    pub fn parse_expr_stmt(&mut self, lhs: Expr) -> Result<Stmt, ParsingError> {
        Ok(Stmt {
            span: lhs.span.clone(),
            kind: StmtKind::Expression(lhs),
        })
    }
}
