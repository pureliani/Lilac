pub mod access;
pub mod and;
pub mod binary_op;
pub mod bool_literal;
pub mod codeblock;
pub mod r#fn;
pub mod fn_call;
pub mod identifier;
pub mod r#if;
pub mod index;
pub mod is_variant;
pub mod list_literal;
pub mod r#match;
pub mod number_literal;
pub mod or;
pub mod static_access;
pub mod string;
pub mod struct_init;
pub mod tag;
pub mod typecast;
pub mod unary_op;

use crate::{
    ast::expr::{Expr, ExprKind},
    hir::builders::{Builder, InBlock, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_expr(&mut self, expr: Expr) -> ValueId {
        match expr.kind {
            ExprKind::Not { right } => todo!(),
            ExprKind::Neg { right } => todo!(),
            ExprKind::Add { left, right } => todo!(),
            ExprKind::Subtract { left, right } => todo!(),
            ExprKind::Multiply { left, right } => todo!(),
            ExprKind::Divide { left, right } => todo!(),
            ExprKind::Modulo { left, right } => todo!(),
            ExprKind::LessThan { left, right } => todo!(),
            ExprKind::LessThanOrEqual { left, right } => todo!(),
            ExprKind::GreaterThan { left, right } => todo!(),
            ExprKind::GreaterThanOrEqual { left, right } => todo!(),
            ExprKind::Equal { left, right } => todo!(),
            ExprKind::NotEqual { left, right } => todo!(),
            ExprKind::And { left, right } => todo!(),
            ExprKind::Or { left, right } => todo!(),
            ExprKind::Struct(items) => todo!(),
            ExprKind::Access { .. } => self.build_access_expr(expr),
            ExprKind::StaticAccess { left, field } => todo!(),
            ExprKind::Index { left, index } => self.build_index_expr(left, index),
            ExprKind::TypeCast { left, target } => todo!(),
            ExprKind::IsVariant { left, variants } => todo!(),
            ExprKind::Tag { name, value } => todo!(),
            ExprKind::FnCall { left, args } => todo!(),
            ExprKind::BoolLiteral(_) => todo!(),
            ExprKind::Number(number_kind) => todo!(),
            ExprKind::String(string_node) => todo!(),
            ExprKind::Identifier(identifier_node) => {
                self.build_identifier_expr(identifier_node.clone())
            }
            ExprKind::Fn(fn_decl) => todo!(),
            ExprKind::Match { conditions, arms } => todo!(),
            ExprKind::If {
                branches,
                else_branch,
            } => todo!(),
            ExprKind::List(exprs) => todo!(),
            ExprKind::CodeBlock(block_contents) => todo!(),
        }
    }
}
