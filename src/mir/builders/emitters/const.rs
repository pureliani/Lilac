use crate::{
    ast::DeclarationId,
    compile::interner::StringId,
    mir::{
        builders::{Builder, InBlock, ValueId},
        instructions::{ConstInstr, Instruction},
        types::{
            checked_declaration::{CheckedDeclaration, CheckedParam, FnType},
            checked_type::{LiteralType, Type},
            ordered_float::OrderedNumberKind,
        },
    },
    tokenize::NumberKind,
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_number(&mut self, val: NumberKind) -> ValueId {
        let ty = Type::from_number_kind(&val);
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Const(ConstInstr::ConstNumber { dest, val }));
        dest
    }

    pub fn emit_number_literal(&mut self, val: NumberKind) -> ValueId {
        self.new_value_id(Type::Literal(LiteralType::Number(OrderedNumberKind(val))))
    }

    pub fn emit_bool(&mut self, val: bool) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Const(ConstInstr::ConstBool { dest, val }));
        dest
    }

    pub fn emit_bool_literal(&mut self, val: bool) -> ValueId {
        self.new_value_id(Type::Literal(LiteralType::Bool(val)))
    }

    pub fn emit_string(&mut self, val: StringId) -> ValueId {
        let dest = self.new_value_id(Type::String);
        self.push_instruction(Instruction::Const(ConstInstr::ConstString { dest, val }));
        dest
    }

    pub fn emit_string_literal(&mut self, val: StringId) -> ValueId {
        self.new_value_id(Type::Literal(LiteralType::String(val)))
    }

    pub fn emit_const_void(&mut self) -> ValueId {
        self.new_value_id(Type::Void)
    }

    pub fn emit_const_null(&mut self) -> ValueId {
        self.new_value_id(Type::Null)
    }

    pub fn emit_const_fn(&mut self, decl_id: DeclarationId) -> ValueId {
        let decl = self
            .program
            .declarations
            .get(&decl_id)
            .expect("INTERNAL COMPILER ERROR: Function declaration not found");

        let (params, return_type) = match decl {
            CheckedDeclaration::Function(f) => {
                let checked_params = f
                    .params
                    .iter()
                    .map(|p| CheckedParam {
                        identifier: p.identifier.clone(),
                        ty: p.ty.clone(),
                    })
                    .collect();
                (checked_params, Box::new(f.return_type.clone()))
            }
            _ => panic!("INTERNAL COMPILER ERROR: Declaration is not a function"),
        };

        let ty = Type::Fn(FnType {
            params,
            return_type,
        });

        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Const(ConstInstr::ConstFn { dest, decl_id }));
        dest
    }
}
