use crate::{
    ast::DeclarationId,
    hir::{
        builders::{Builder, ConstantId, InBlock, ValueId},
        instructions::{ConstInstr, Instruction},
        types::{
            checked_declaration::{CheckedDeclaration, FnType},
            checked_type::{StructKind, Type},
        },
    },
    tokenize::NumberKind,
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_const_number(&mut self, val: NumberKind) -> ValueId {
        let ty = match val {
            NumberKind::I64(_) => Type::I64,
            NumberKind::I32(_) => Type::I32,
            NumberKind::I16(_) => Type::I16,
            NumberKind::I8(_) => Type::I8,
            NumberKind::F32(_) => Type::F32,
            NumberKind::F64(_) => Type::F64,
            NumberKind::U64(_) => Type::U64,
            NumberKind::U32(_) => Type::U32,
            NumberKind::U16(_) => Type::U16,
            NumberKind::U8(_) => Type::U8,
            NumberKind::ISize(_) => Type::ISize,
            NumberKind::USize(_) => Type::USize,
        };

        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Const(ConstInstr::ConstNumber { dest, val }));
        dest
    }

    pub fn emit_const_bool(&mut self, val: bool) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Const(ConstInstr::ConstBool { dest, val }));
        dest
    }

    pub fn emit_const_string(&mut self, constant_id: ConstantId) -> ValueId {
        let inner = Box::new(Type::Struct(StructKind::StringHeader));
        let ty = Type::Pointer(inner);

        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Const(ConstInstr::ConstString {
            dest,
            constant_id,
        }));
        dest
    }

    pub fn emit_const_void(&mut self) -> ValueId {
        let dest = self.new_value_id(Type::Void);
        self.push_instruction(Instruction::Const(ConstInstr::ConstVoid { dest }));
        dest
    }

    pub fn emit_const_fn(&mut self, decl_id: DeclarationId) -> ValueId {
        let decl = self
            .program
            .declarations
            .get(&decl_id)
            .expect("INTERNAL COMPILER ERROR: Function declaration not found");

        let (params, return_type) = match decl {
            CheckedDeclaration::Function(f) => {
                (f.params.clone(), Box::new(f.return_type.clone()))
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
