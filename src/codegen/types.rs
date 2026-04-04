use inkwell::types::{AnyType, AnyTypeEnum, BasicTypeEnum};
use inkwell::AddressSpace;

use crate::codegen::CodeGenerator;
use crate::compile::interner::TypeId;
use crate::mir::types::checked_declaration::FnType;
use crate::mir::types::checked_type::{LiteralType, Type};
use crate::mir::utils::layout::get_layout_of;

impl<'ctx, 'a> CodeGenerator<'ctx, 'a> {
    pub fn get_any_type(&self, ty_id: TypeId) -> AnyTypeEnum<'ctx> {
        let ty = self.type_interner.resolve(ty_id);
        match ty {
            Type::Literal(LiteralType::Void | LiteralType::Never) => {
                self.context.void_type().into()
            }
            _ => self.get_basic_type(ty_id).as_any_type_enum(),
        }
    }

    pub fn get_basic_type(&self, ty_id: TypeId) -> BasicTypeEnum<'ctx> {
        let ty = self.type_interner.resolve(ty_id);

        let layout = get_layout_of(
            &ty,
            self.type_interner,
            self.program.target_ptr_size,
            self.program.target_ptr_align,
        );

        if layout.size == 0 {
            return self.context.struct_type(&[], false).into();
        }

        match ty {
            Type::Bool(None) => self.context.bool_type().into(),
            Type::U8(None) | Type::I8(None) => self.context.i8_type().into(),
            Type::U16(None) | Type::I16(None) => self.context.i16_type().into(),
            Type::U32(None) | Type::I32(None) => self.context.i32_type().into(),
            Type::U64(None) | Type::I64(None) => self.context.i64_type().into(),
            Type::USize(None) | Type::ISize(None) => {
                let target_data = self.target_machine.get_target_data();
                self.context.ptr_sized_int_type(&target_data, None).into()
            }
            Type::F32(None) => self.context.f32_type().into(),
            Type::F64(None) => self.context.f64_type().into(),
            Type::Pointer(_) | Type::IndirectFn(FnType::Indirect { .. }) => {
                self.context.ptr_type(AddressSpace::default()).into()
            }
            Type::TaglessUnion(_) => {
                let i8_ty = self.context.i8_type();
                i8_ty.array_type(layout.size as u32).into()
            }
            Type::Struct(s) => {
                let fields = s.fields(self.type_interner);
                let mut field_types = Vec::new();
                for (_, f_id) in fields {
                    let f_ty = self.get_basic_type(f_id);
                    field_types.push(f_ty);
                }
                self.context.struct_type(&field_types, false).into()
            }
            _ => unreachable!("ZSTs handled above, physical types handled here"),
        }
    }
}
