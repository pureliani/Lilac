use std::panic;

use crate::{
    globals::STRING_INTERNER,
    hir::types::{
        checked_declaration::CheckedParam,
        checked_type::{StructKind, Type},
    },
};

pub struct Layout {
    pub size: usize,
    pub alignment: usize,
}

impl Layout {
    pub fn new(size: usize, alignment: usize) -> Self {
        Self { size, alignment }
    }
}

// Constants for the target architecture
const PTR_SIZE: usize = size_of::<usize>();
const PTR_ALIGN: usize = size_of::<usize>();
const USIZE_SIZE: usize = size_of::<usize>();
const USIZE_ALIGN: usize = size_of::<usize>();

/// IMPORTANT: Make sure user-defined and closure-environment structs are packed first before calling this function
pub fn get_layout_of(ty: &Type) -> Layout {
    match ty {
        Type::Tag(tag) => {
            if let Some(payload_ty) = &tag.value_type {
                calculate_fields_layout(&[&Type::U16, payload_ty])
            } else {
                Layout::new(2, 2)
            }
        }
        Type::Union(variants) => {
            let mut max_size = 0;
            let mut max_align = 2;
            for v in variants {
                let l = get_layout_of(&Type::Tag(v.clone()));
                max_size = max_size.max(l.size);
                max_align = max_align.max(l.alignment);
            }
            Layout::new(max_size, max_align)
        }
        Type::Void => Layout::new(0, 1),
        Type::Bool | Type::U8 | Type::I8 => Layout::new(1, 1),
        Type::U16 | Type::I16 => Layout::new(2, 2),
        Type::U32 | Type::I32 | Type::F32 => Layout::new(4, 4),
        Type::U64 | Type::I64 | Type::F64 => Layout::new(8, 8),

        Type::Pointer { .. } | Type::Fn(_) | Type::USize | Type::ISize => {
            Layout::new(USIZE_SIZE, USIZE_ALIGN)
        }

        Type::Buffer { size, alignment } => Layout::new(*size, *alignment),

        Type::Unknown => Layout::new(0, 1),

        Type::Struct(s) => {
            let fields = s.fields();
            let types: Vec<&Type> = fields.iter().map(|(_, ty)| ty).collect();

            calculate_fields_layout(&types)
        }
    }
}

pub fn get_alignment_of(ty: &Type) -> usize {
    get_layout_of(ty).alignment
}

/// Helper to calculate layout of fields placed sequentially in memory
fn calculate_fields_layout(field_types: &[&Type]) -> Layout {
    let mut current_offset = 0;
    let mut max_alignment = 1;

    for ty in field_types {
        let field_layout = get_layout_of(ty);

        max_alignment = std::cmp::max(max_alignment, field_layout.alignment);

        let padding = (field_layout.alignment
            - (current_offset % field_layout.alignment))
            % field_layout.alignment;

        current_offset += padding;
        current_offset += field_layout.size;
    }

    let padding_end = (max_alignment - (current_offset % max_alignment)) % max_alignment;
    let total_size = current_offset + padding_end;

    Layout::new(total_size, max_alignment)
}

pub fn pack_struct(struct_kind: StructKind) -> StructKind {
    match struct_kind {
        StructKind::UserDefined(mut fields) => {
            sort_fields(&mut fields);
            StructKind::UserDefined(fields)
        }
        _ => {
            panic!(
                "INTERNAL COMPILER ERROR: Cannot pack struct that is not user defined!"
            );
        }
    }
}

fn sort_fields(fields: &mut [CheckedParam]) {
    fields.sort_by(|field_a, field_b| {
        let align_a = get_alignment_of(&field_a.ty);
        let align_b = get_alignment_of(&field_b.ty);

        // Sort by Alignment (Descending) -> Name (Ascending)
        align_b.cmp(&align_a).then_with(|| {
            let name_a = STRING_INTERNER.resolve(field_a.identifier.name);
            let name_b = STRING_INTERNER.resolve(field_b.identifier.name);

            name_a.cmp(&name_b)
        })
    });
}
