use crate::ast::{
    decl::{FnDecl, TypeAliasDecl, VarDecl},
    expr::{BlockContents, Expr, ExprKind, MatchArm, MatchPattern},
    stmt::{Stmt, StmtKind},
    type_annotation::{TagAnnotation, TypeAnnotation, TypeAnnotationKind},
    IdentifierNode, StringNode,
};
use crate::tokenize::NumberKind;

pub trait ASTVisitor<'ast>: Sized {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        walk_expr(self, expr);
    }

    fn visit_mut_expr(&mut self, inner: &'ast Expr) {
        self.visit_expr(inner);
    }

    fn visit_block(&mut self, block: &'ast BlockContents) {
        walk_block(self, block);
    }

    fn visit_type(&mut self, ty: &'ast TypeAnnotation) {
        walk_type(self, ty);
    }

    /// e.g. `x + 1`, `print(x)`
    fn visit_ident_usage(&mut self, _id: IdentifierNode) {}

    /// e.g. `let x`, `fn foo(x)`
    fn visit_ident_decl(&mut self, _id: IdentifierNode) {}

    /// Called for non-variable identifiers: struct fields, enum tags, static methods
    fn visit_ident_label(&mut self, _id: IdentifierNode) {}

    /// Called for type names e.g. `x: MyType`
    fn visit_ident_type(&mut self, _id: IdentifierNode) {}

    fn visit_expr_stmt(&mut self, expr: &'ast Expr) {
        self.visit_expr(expr);
    }

    fn visit_type_alias_decl_stmt(&mut self, decl: &'ast TypeAliasDecl) {
        self.visit_type_alias_decl(decl);
    }

    fn visit_var_decl_stmt(&mut self, decl: &'ast VarDecl) {
        self.visit_var_decl(decl);
    }

    fn visit_break_stmt(&mut self) {}
    fn visit_continue_stmt(&mut self) {}

    fn visit_return_stmt(&mut self, value: &'ast Expr) {
        self.visit_expr(value);
    }

    fn visit_assignment_stmt(&mut self, target: &'ast Expr, value: &'ast Expr) {
        self.visit_expr(value);
        self.visit_expr(target);
    }

    fn visit_from_stmt(
        &mut self,
        _path: &'ast StringNode,
        _ids: &'ast [(IdentifierNode, Option<IdentifierNode>)],
    ) {
    }

    fn visit_while_stmt(&mut self, condition: &'ast Expr, body: &'ast BlockContents) {
        self.visit_expr(condition);
        self.visit_block(body);
    }

    fn visit_not_expr(&mut self, right: &'ast Expr) {
        self.visit_expr(right);
    }
    fn visit_neg_expr(&mut self, right: &'ast Expr) {
        self.visit_expr(right);
    }

    fn visit_add_expr(&mut self, left: &'ast Expr, right: &'ast Expr) {
        self.visit_binary_defaults(left, right);
    }
    fn visit_sub_expr(&mut self, left: &'ast Expr, right: &'ast Expr) {
        self.visit_binary_defaults(left, right);
    }
    fn visit_mul_expr(&mut self, left: &'ast Expr, right: &'ast Expr) {
        self.visit_binary_defaults(left, right);
    }
    fn visit_div_expr(&mut self, left: &'ast Expr, right: &'ast Expr) {
        self.visit_binary_defaults(left, right);
    }
    fn visit_mod_expr(&mut self, left: &'ast Expr, right: &'ast Expr) {
        self.visit_binary_defaults(left, right);
    }
    fn visit_lt_expr(&mut self, left: &'ast Expr, right: &'ast Expr) {
        self.visit_binary_defaults(left, right);
    }
    fn visit_lte_expr(&mut self, left: &'ast Expr, right: &'ast Expr) {
        self.visit_binary_defaults(left, right);
    }
    fn visit_gt_expr(&mut self, left: &'ast Expr, right: &'ast Expr) {
        self.visit_binary_defaults(left, right);
    }
    fn visit_gte_expr(&mut self, left: &'ast Expr, right: &'ast Expr) {
        self.visit_binary_defaults(left, right);
    }
    fn visit_eq_expr(&mut self, left: &'ast Expr, right: &'ast Expr) {
        self.visit_binary_defaults(left, right);
    }
    fn visit_neq_expr(&mut self, left: &'ast Expr, right: &'ast Expr) {
        self.visit_binary_defaults(left, right);
    }
    fn visit_and_expr(&mut self, left: &'ast Expr, right: &'ast Expr) {
        self.visit_binary_defaults(left, right);
    }
    fn visit_or_expr(&mut self, left: &'ast Expr, right: &'ast Expr) {
        self.visit_binary_defaults(left, right);
    }

    fn visit_struct_init_expr(&mut self, fields: &'ast [(IdentifierNode, Expr)]) {
        for (id, expr) in fields {
            self.visit_ident_label(id.clone());
            self.visit_expr(expr);
        }
    }

    fn visit_access_expr(&mut self, left: &'ast Expr, field: IdentifierNode) {
        self.visit_expr(left);
        self.visit_ident_label(field);
    }

    fn visit_static_access_expr(&mut self, left: &'ast Expr, field: IdentifierNode) {
        self.visit_expr(left);
        self.visit_ident_label(field);
    }

    fn visit_type_cast_expr(&mut self, left: &'ast Expr, target: &'ast TypeAnnotation) {
        self.visit_expr(left);
        self.visit_type(target);
    }

    fn visit_tag_expr(&mut self, name: IdentifierNode, value: Option<&'ast Expr>) {
        self.visit_ident_label(name);
        if let Some(val) = value {
            self.visit_expr(val);
        }
    }

    fn visit_fn_call_expr(&mut self, left: &'ast Expr, args: &'ast [Expr]) {
        self.visit_expr(left);
        for arg in args {
            self.visit_expr(arg);
        }
    }

    fn visit_match_expr(&mut self, conditions: &'ast [Expr], arms: &'ast [MatchArm]) {
        for cond in conditions {
            self.visit_expr(cond);
        }
        for arm in arms {
            self.visit_match_arm(arm);
        }
    }

    fn visit_if_expr(
        &mut self,
        branches: &'ast [(Box<Expr>, BlockContents)],
        else_branch: Option<&'ast BlockContents>,
    ) {
        for (cond, body) in branches {
            self.visit_expr(cond);
            self.visit_block(body);
        }
        if let Some(body) = else_branch {
            self.visit_block(body);
        }
    }

    fn visit_list_literal_expr(&mut self, items: &'ast [Expr]) {
        for item in items {
            self.visit_expr(item);
        }
    }

    fn visit_codeblock_expr(&mut self, block: &'ast BlockContents) {
        self.visit_block(block);
    }

    fn visit_index_expr(&mut self, left: &'ast Expr, index: &'ast Expr) {
        self.visit_expr(left);
        self.visit_expr(index);
    }

    fn visit_fn_expr(&mut self, decl: &'ast FnDecl) {
        self.visit_fn_decl(decl);
    }

    fn visit_bool_literal(&mut self, _val: bool) {}
    fn visit_number_literal(&mut self, _val: NumberKind) {}
    fn visit_string_literal(&mut self, _val: &'ast StringNode) {}

    fn visit_identifier_expr(&mut self, id: IdentifierNode) {
        self.visit_ident_usage(id);
    }

    fn visit_fn_decl(&mut self, decl: &'ast FnDecl) {
        self.visit_ident_decl(decl.identifier.clone());
        for param in &decl.params {
            self.visit_ident_decl(param.identifier.clone());
            self.visit_type(&param.constraint);
        }
        self.visit_type(&decl.return_type);
        self.visit_block(&decl.body);
    }

    fn visit_var_decl(&mut self, decl: &'ast VarDecl) {
        self.visit_ident_decl(decl.identifier.clone());
        if let Some(constraint) = &decl.constraint {
            self.visit_type(constraint);
        }
        self.visit_expr(&decl.value);
    }

    fn visit_type_alias_decl(&mut self, decl: &'ast TypeAliasDecl) {
        self.visit_ident_decl(decl.identifier.clone());
        self.visit_type(&decl.value);
    }

    fn visit_match_arm(&mut self, arm: &'ast MatchArm) {
        for pattern in &arm.pattern {
            match pattern {
                MatchPattern::VariantWithValue(v, b) => {
                    self.visit_ident_label(v.clone());
                    self.visit_ident_decl(b.clone());
                }
                MatchPattern::Variant(v) => {
                    self.visit_ident_label(v.clone());
                }
            }
        }
        self.visit_expr(&arm.expression);
    }

    fn visit_binary_defaults(&mut self, left: &'ast Expr, right: &'ast Expr) {
        self.visit_expr(left);
        self.visit_expr(right);
    }

    fn visit_is_variant_expr(
        &mut self,
        left: &'ast Expr,
        _variants: &'ast [TagAnnotation],
    ) {
        self.visit_expr(left);
    }
}

pub fn walk_stmt<'ast, V: ASTVisitor<'ast>>(v: &mut V, stmt: &'ast Stmt) {
    match &stmt.kind {
        StmtKind::Expression(e) => v.visit_expr_stmt(e),
        StmtKind::TypeAliasDecl(d) => v.visit_type_alias_decl_stmt(d),
        StmtKind::VarDecl(d) => v.visit_var_decl_stmt(d),
        StmtKind::Break => v.visit_break_stmt(),
        StmtKind::Continue => v.visit_continue_stmt(),
        StmtKind::Return { value } => v.visit_return_stmt(value),
        StmtKind::Assignment { target, value } => v.visit_assignment_stmt(target, value),
        StmtKind::From { path, identifiers } => v.visit_from_stmt(path, identifiers),
        StmtKind::While { condition, body } => v.visit_while_stmt(condition, body),
    }
}

pub fn walk_expr<'ast, V: ASTVisitor<'ast>>(v: &mut V, expr: &'ast Expr) {
    match &expr.kind {
        ExprKind::Identifier(id) => v.visit_identifier_expr(id.clone()),
        ExprKind::Not { right } => v.visit_not_expr(right),
        ExprKind::Neg { right } => v.visit_neg_expr(right),
        ExprKind::Add { left, right } => v.visit_add_expr(left, right),
        ExprKind::Subtract { left, right } => v.visit_sub_expr(left, right),
        ExprKind::Multiply { left, right } => v.visit_mul_expr(left, right),
        ExprKind::Divide { left, right } => v.visit_div_expr(left, right),
        ExprKind::Modulo { left, right } => v.visit_mod_expr(left, right),
        ExprKind::LessThan { left, right } => v.visit_lt_expr(left, right),
        ExprKind::LessThanOrEqual { left, right } => v.visit_lte_expr(left, right),
        ExprKind::GreaterThan { left, right } => v.visit_gt_expr(left, right),
        ExprKind::GreaterThanOrEqual { left, right } => v.visit_gte_expr(left, right),
        ExprKind::Equal { left, right } => v.visit_eq_expr(left, right),
        ExprKind::NotEqual { left, right } => v.visit_neq_expr(left, right),
        ExprKind::And { left, right } => v.visit_and_expr(left, right),
        ExprKind::Or { left, right } => v.visit_or_expr(left, right),
        ExprKind::Struct(fields) => v.visit_struct_init_expr(fields),
        ExprKind::Access { left, field } => v.visit_access_expr(left, field.clone()),
        ExprKind::StaticAccess { left, field } => {
            v.visit_static_access_expr(left, field.clone())
        }
        ExprKind::TypeCast { left, target } => v.visit_type_cast_expr(left, target),
        ExprKind::Tag { name, value } => v.visit_tag_expr(name.clone(), value.as_deref()),
        ExprKind::FnCall { left, args } => v.visit_fn_call_expr(left, args),
        ExprKind::BoolLiteral(b) => v.visit_bool_literal(*b),
        ExprKind::Number(n) => v.visit_number_literal(*n),
        ExprKind::String(s) => v.visit_string_literal(s),
        ExprKind::Fn(decl) => v.visit_fn_expr(decl),
        ExprKind::Match { conditions, arms } => v.visit_match_expr(conditions, arms),
        ExprKind::If {
            branches,
            else_branch,
        } => v.visit_if_expr(branches, else_branch.as_ref()),
        ExprKind::List(items) => v.visit_list_literal_expr(items),
        ExprKind::CodeBlock(block) => v.visit_codeblock_expr(block),
        ExprKind::Index { left, index } => v.visit_index_expr(left, index),
        ExprKind::IsVariant { left, variants } => v.visit_is_variant_expr(left, variants),
    }
}

pub fn walk_block<'ast, V: ASTVisitor<'ast>>(v: &mut V, block: &'ast BlockContents) {
    for stmt in &block.statements {
        v.visit_stmt(stmt);
    }
    if let Some(expr) = &block.final_expr {
        v.visit_expr(expr);
    }
}

pub fn walk_type<'ast, V: ASTVisitor<'ast>>(v: &mut V, ty: &'ast TypeAnnotation) {
    match &ty.kind {
        TypeAnnotationKind::Identifier(id) => v.visit_ident_type(id.clone()),
        TypeAnnotationKind::Struct(fields) => {
            for f in fields {
                v.visit_ident_label(f.identifier.clone());
                v.visit_type(&f.constraint);
            }
        }
        TypeAnnotationKind::Tag(tag) => {
            v.visit_ident_label(tag.identifier.clone());
            if let Some(val) = &tag.value_type {
                v.visit_type(val);
            }
        }
        TypeAnnotationKind::Union(tags) => {
            for tag in tags {
                v.visit_ident_label(tag.identifier.clone());
                if let Some(val) = &tag.value_type {
                    v.visit_type(val);
                }
            }
        }
        TypeAnnotationKind::List(inner) => v.visit_type(inner),
        TypeAnnotationKind::FnType {
            params,
            return_type,
        } => {
            for param in params {
                v.visit_ident_label(param.identifier.clone());
                v.visit_type(&param.constraint);
            }
            v.visit_type(return_type);
        }
        TypeAnnotationKind::Void => {}
        TypeAnnotationKind::Bool => {}
        TypeAnnotationKind::U8 => {}
        TypeAnnotationKind::U16 => {}
        TypeAnnotationKind::U32 => {}
        TypeAnnotationKind::U64 => {}
        TypeAnnotationKind::I8 => {}
        TypeAnnotationKind::I16 => {}
        TypeAnnotationKind::I32 => {}
        TypeAnnotationKind::I64 => {}
        TypeAnnotationKind::F32 => {}
        TypeAnnotationKind::F64 => {}
        TypeAnnotationKind::String => {}
    }
}
