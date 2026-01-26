pub mod assignment;
pub mod from;
pub mod r#return;
pub mod type_alias_decl;
pub mod var_decl;
pub mod r#while;

use crate::{
    ast::{
        expr::ExprKind,
        stmt::{Stmt, StmtKind},
    },
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        expressions::r#if::IfContext,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_statements(&mut self, statements: Vec<Stmt>) {
        for statement in statements {
            if self.bb().terminator.is_some() {
                self.errors.push(SemanticError {
                    kind: SemanticErrorKind::UnreachableCode,
                    span: statement.span,
                });
                break;
            }

            let result = match statement.kind {
                StmtKind::Expression(expr) => {
                    if let ExprKind::If {
                        branches,
                        else_branch,
                    } = expr.kind
                    {
                        self.build_if(branches, else_branch, IfContext::Statement)
                            .map(|_| ())
                    } else {
                        self.build_expr(expr).map(|_| ())
                    }
                }
                StmtKind::TypeAliasDecl(decl) => {
                    self.as_module().build_type_alias_decl(decl, statement.span)
                }
                StmtKind::VarDecl(var_decl) => self.build_var_decl(var_decl),
                StmtKind::Return { value } => {
                    self.build_return_stmt(value, statement.span)
                }
                StmtKind::Assignment { target, value } => {
                    self.build_assignment_stmt(target, value)
                }
                StmtKind::From { path, identifiers } => {
                    self.as_module()
                        .build_from_stmt(path, identifiers, statement.span)
                }
                StmtKind::While { condition, body } => {
                    self.build_while_stmt(*condition, body)
                }
                StmtKind::Break => {
                    if let Some(targets) = self.current_scope.within_loop_body() {
                        self.jmp(targets.on_break);
                        Ok(())
                    } else {
                        Err(SemanticError {
                            kind: SemanticErrorKind::BreakKeywordOutsideLoop,
                            span: statement.span,
                        })
                    }
                }
                StmtKind::Continue => {
                    if let Some(targets) = self.current_scope.within_loop_body() {
                        self.jmp(targets.on_continue);
                        Ok(())
                    } else {
                        Err(SemanticError {
                            kind: SemanticErrorKind::ContinueKeywordOutsideLoop,
                            span: statement.span,
                        })
                    }
                }
            };

            if let Err(e) = result {
                self.errors.push(e);
            }
        }
    }
}
