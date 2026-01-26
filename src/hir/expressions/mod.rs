pub mod access;
pub mod and;
pub mod binary_op;
pub mod bool_literal;
pub mod codeblock;
pub mod r#fn;
pub mod fn_call;
pub mod identifier;
pub mod r#if;
pub mod is_type;
pub mod list_literal;
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
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
        expressions::r#if::IfContext,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_expr(&mut self, expr: Expr) -> Result<ValueId, SemanticError> {
        let span = expr.span.clone();
        match expr.kind {
            ExprKind::Not { right } => self.build_not_expr(*right),
            ExprKind::Neg { right } => self.build_neg_expr(*right),

            ExprKind::Add { left, right } => self.build_add_expr(*left, *right),
            ExprKind::Subtract { left, right } => self.build_sub_expr(*left, *right),
            ExprKind::Multiply { left, right } => self.build_mul_expr(*left, *right),
            ExprKind::Divide { left, right } => self.build_div_expr(*left, *right),
            ExprKind::Modulo { left, right } => self.build_mod_expr(*left, *right),
            ExprKind::LessThan { left, right } => self.build_lt_expr(*left, *right),
            ExprKind::LessThanOrEqual { left, right } => {
                self.build_lte_expr(*left, *right)
            }
            ExprKind::GreaterThan { left, right } => self.build_gt_expr(*left, *right),
            ExprKind::GreaterThanOrEqual { left, right } => {
                self.build_gte_expr(*left, *right)
            }
            ExprKind::Equal { left, right } => self.build_eq_expr(*left, *right),
            ExprKind::NotEqual { left, right } => self.build_neq_expr(*left, *right),

            ExprKind::And { left, right } => self.build_and_expr(*left, *right),
            ExprKind::Or { left, right } => self.build_or_expr(*left, *right),

            ExprKind::BoolLiteral(value) => self.build_bool_literal(value),
            ExprKind::Number(number_kind) => self.build_number_literal(number_kind),
            ExprKind::String(string_node) => self.build_string_literal(string_node),

            ExprKind::Struct(fields) => self.build_struct_init_expr(fields),
            ExprKind::List(items) => self.build_list_literal_expr(items, span),
            ExprKind::Tag(tag_id) => self.build_tag_expr(tag_id),

            ExprKind::Access { left, field } => self.build_access_expr(*left, field),
            ExprKind::StaticAccess { left, field } => {
                self.build_static_access_expr(*left, field)
            }
            ExprKind::If {
                branches,
                else_branch,
            } => self.build_if(branches, else_branch, IfContext::Expression),

            ExprKind::CodeBlock(block_contents) => {
                self.build_codeblock_expr(block_contents)
            }

            ExprKind::Fn(fn_decl) => self.build_fn_expr(*fn_decl),
            ExprKind::FnCall { left, args } => self.build_fn_call_expr(*left, args, span),

            ExprKind::Identifier(identifier_node) => {
                self.build_identifier_expr(identifier_node)
            }
            ExprKind::TypeCast { left, target } => {
                self.build_typecast_expr(*left, target)
            }
            ExprKind::IsType { left, ty } => self.build_is_type_expr(*left, ty),
        }
    }
}
