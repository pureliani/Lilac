use crate::{
    globals::STRING_INTERNER,
    hir::{
        builders::{BasicBlock, BasicBlockId, Function, Program, ValueId},
        instructions::{Instruction, Terminator},
        types::{checked_declaration::CheckedDeclaration, checked_type::Type},
        utils::type_to_string::type_to_string,
    },
    tokenize::number_kind_to_suffix,
};
use std::{collections::HashMap, fmt::Write};

pub fn dump_program(program: &Program) {
    let mut out = String::new();
    writeln!(out, "========== HIR DUMP START ==========").unwrap();
    for (_, decl) in program.declarations.iter() {
        if let CheckedDeclaration::Function(f) = decl {
            dump_function(f, program, &mut out);
        }
    }
    writeln!(out, "====================================").unwrap();
    println!("{}", out);
}

fn dump_function(f: &Function, p: &Program, out: &mut String) {
    let fn_name = STRING_INTERNER.resolve(f.identifier.name);
    let return_type = type_to_string(&f.return_type);
    writeln!(out, "fn {fn_name} -> {return_type}:").unwrap();
    dump_block(&f.entry_block, f, p, out);
}

pub fn dump_block(block_id: &BasicBlockId, f: &Function, p: &Program, out: &mut String) {
    let bb = f.blocks.get(block_id).unwrap();
    writeln!(out, "  block_{}:", bb.id.0).unwrap();

    writeln!(out, "    predecessors {{ ").unwrap();
    for p in &bb.predecessors {
        writeln!(out, "      block_{}", p.0).unwrap();
    }
    writeln!(out, "    }} ").unwrap();

    writeln!(out, "    params {{ ").unwrap();
    for p in &bb.params {
        writeln!(out, "      v{}", p.0).unwrap();
    }
    writeln!(out, "    }} ").unwrap();

    writeln!(out, "    mappings {{ ").unwrap();
    for (orig, local) in &bb.original_to_local_valueid {
        writeln!(out, "      v{} (original) -> v{} (local) ", orig.0, local.0).unwrap();
    }
    writeln!(out, "    }} ").unwrap();

    write!(out, "\n\n").unwrap();

    dump_instructions(&bb.instructions, p, out);

    let print_args = |out: &mut String, args: &HashMap<ValueId, ValueId>| {
        write!(out, "(").unwrap();
        let joined_args = args
            .iter()
            .map(|(parameter, argument)| format!("v{} <- v{}", parameter.0, argument.0))
            .collect::<Vec<String>>()
            .join(", ");
        write!(out, "{}", joined_args).unwrap();
        write!(out, ")").unwrap();
    };

    let term = bb.terminator.clone().unwrap();
    match term {
        Terminator::Jump { target, args } => {
            write!(out, "    jmp block_{}", target.0).unwrap();
            print_args(out, &args);
            write!(out, "\n\n").unwrap();
        }
        Terminator::CondJump {
            condition,
            true_target,
            true_args,
            false_target,
            false_args,
        } => {
            write!(
                out,
                "    cond_jmp v{} ? block_{}",
                condition.0, true_target.0
            )
            .unwrap();
            print_args(out, &true_args);
            write!(out, " : block_{}", false_target.0).unwrap();
            print_args(out, &false_args);
            write!(out, "\n\n").unwrap();

            dump_block(&true_target, f, p, out);
            dump_block(&false_target, f, p, out);
        }
        Terminator::Return { value } => {
            writeln!(out, "ret v{}", value.0).unwrap();
        }
        _ => writeln!(out, "{:?}", term).unwrap(),
    }
}

pub fn dump_instructions(instrs: &[Instruction], p: &Program, out: &mut String) {
    let get_ty = |vid: &ValueId| type_to_string(&p.value_types[vid]);
    let get_binary_op_sign = |instr: &Instruction| {
        String::from(match instr {
            Instruction::IAdd { .. } | Instruction::FAdd { .. } => "+",
            Instruction::ISub { .. } | Instruction::FSub { .. } => "-",
            Instruction::SDiv { .. }
            | Instruction::UDiv { .. }
            | Instruction::FDiv { .. } => "/",
            Instruction::SRem { .. }
            | Instruction::URem { .. }
            | Instruction::FRem { .. } => "%",
            Instruction::IMul { .. } | Instruction::FMul { .. } => "*",
            Instruction::IEq { .. } | Instruction::FEq { .. } => "==",
            Instruction::INe { .. } | Instruction::FNe { .. } => "!=",
            Instruction::SLt { .. }
            | Instruction::ULt { .. }
            | Instruction::FLt { .. } => "<",
            Instruction::SLe { .. }
            | Instruction::ULe { .. }
            | Instruction::FLe { .. } => "<=",
            Instruction::UGt { .. }
            | Instruction::SGt { .. }
            | Instruction::FGt { .. } => ">",
            Instruction::SGe { .. }
            | Instruction::UGe { .. }
            | Instruction::FGe { .. } => ">=",
            _ => panic!("Cannot get sign for non binary instructions"),
        })
    };

    for instruction in instrs {
        write!(out, "    ").unwrap();
        match instruction {
            Instruction::ConstNumber { dest, val } => {
                writeln!(
                    out,
                    "v{}: {} = {};",
                    dest.0,
                    number_kind_to_suffix(val),
                    val.to_string()
                )
                .unwrap();
            }
            Instruction::ConstBool { dest, val } => {
                writeln!(out, "v{}: bool = {};", dest.0, val).unwrap();
            }
            Instruction::ConstString { dest, constant_id } => {
                let literal =
                    String::from_utf8(p.constant_data.get(constant_id).unwrap().clone())
                        .unwrap();
                writeln!(out, "v{}: string = \"{}\";", dest.0, literal).unwrap();
            }
            Instruction::ConstFn { dest, decl_id } => todo!(),
            Instruction::ConstVoid { dest } => {
                writeln!(out, "v{}: void = void;", dest.0).unwrap();
            }
            Instruction::IAdd { dest, lhs, rhs }
            | Instruction::ISub { dest, lhs, rhs }
            | Instruction::IMul { dest, lhs, rhs }
            | Instruction::SDiv { dest, lhs, rhs }
            | Instruction::UDiv { dest, lhs, rhs }
            | Instruction::SRem { dest, lhs, rhs }
            | Instruction::URem { dest, lhs, rhs }
            | Instruction::FRem { dest, lhs, rhs }
            | Instruction::FAdd { dest, lhs, rhs }
            | Instruction::FSub { dest, lhs, rhs }
            | Instruction::FMul { dest, lhs, rhs }
            | Instruction::FDiv { dest, lhs, rhs }
            | Instruction::IEq { dest, lhs, rhs }
            | Instruction::INe { dest, lhs, rhs }
            | Instruction::SLt { dest, lhs, rhs }
            | Instruction::SLe { dest, lhs, rhs }
            | Instruction::SGt { dest, lhs, rhs }
            | Instruction::SGe { dest, lhs, rhs }
            | Instruction::ULt { dest, lhs, rhs }
            | Instruction::ULe { dest, lhs, rhs }
            | Instruction::UGt { dest, lhs, rhs }
            | Instruction::UGe { dest, lhs, rhs }
            | Instruction::FEq { dest, lhs, rhs }
            | Instruction::FNe { dest, lhs, rhs }
            | Instruction::FLt { dest, lhs, rhs }
            | Instruction::FLe { dest, lhs, rhs }
            | Instruction::FGt { dest, lhs, rhs }
            | Instruction::FGe { dest, lhs, rhs } => {
                writeln!(
                    out,
                    "v{}: {} = {} {} {};",
                    dest.0,
                    get_ty(dest),
                    lhs.0,
                    get_binary_op_sign(instruction),
                    rhs.0
                )
                .unwrap();
            }
            Instruction::INeg { dest, src } | Instruction::FNeg { dest, src } => {
                writeln!(out, "v{}: {} = -{};", dest.0, get_ty(dest), src.0).unwrap();
            }
            Instruction::BNot { dest, src } => {
                writeln!(out, "v{}: {} = !{};", dest.0, get_ty(dest), src.0).unwrap();
            }
            Instruction::HeapFree { ptr } => {
                writeln!(out, "free(v{})", ptr.0).unwrap();
            }
            Instruction::StackAlloc { destination, count } => {
                let inner_ty = match &p.value_types[destination] {
                    Type::Pointer { constraint, .. } => type_to_string(constraint),
                    _ => "unknown".to_string(),
                };
                writeln!(
                    out,
                    "v{}: {} = stackAlloc({} x {});",
                    destination.0,
                    get_ty(destination),
                    count,
                    inner_ty
                )
                .unwrap();
            }
            Instruction::HeapAlloc { destination, count } => {
                let inner_ty = match &p.value_types[destination] {
                    Type::Pointer { constraint, .. } => type_to_string(constraint),
                    _ => "unknown".to_string(),
                };
                writeln!(
                    out,
                    "v{}: {} = heapAlloc(v{} x {});",
                    destination.0,
                    get_ty(destination),
                    count.0,
                    inner_ty
                )
                .unwrap();
            }
            Instruction::Store { ptr, value } => {
                writeln!(out, "*v{} = v{};", ptr.0, value.0).unwrap();
            }
            Instruction::Load { destination, ptr } => {
                writeln!(
                    out,
                    "v{}: {} = *v{};",
                    destination.0,
                    get_ty(destination),
                    ptr.0
                )
                .unwrap();
            }

            Instruction::GetFieldPtr {
                destination,
                base_ptr,
                field_index,
            } => {
                let base_ty = &p.value_types[base_ptr];
                let field_name = match base_ty {
                    Type::Pointer { narrowed_to, .. } => match &**narrowed_to {
                        Type::Struct(s) => {
                            STRING_INTERNER.resolve(s.fields()[*field_index].0)
                        }
                        _ => format!("{}", field_index),
                    },
                    _ => format!("{}", field_index),
                };
                writeln!(
                    out,
                    "v{}: {} = &v{}.{};",
                    destination.0,
                    get_ty(destination),
                    base_ptr.0,
                    field_name
                )
                .unwrap();
            }
            Instruction::PtrOffset {
                destination,
                base_ptr,
                index,
            } => {
                writeln!(
                    out,
                    "v{}: {} = {} + {};",
                    destination.0,
                    get_ty(destination),
                    base_ptr.0,
                    index.0
                )
                .unwrap();
            }
            Instruction::FunctionCall {
                destination,
                func,
                args,
            } => {
                let args = args
                    .iter()
                    .map(|a| format!("v{}", a.0))
                    .collect::<Vec<String>>()
                    .join(", ");

                writeln!(
                    out,
                    "v{}: {} = call v{}({});",
                    destination.0,
                    get_ty(destination),
                    func.0,
                    args
                )
                .unwrap();
            }
            Instruction::RefineType {
                dest,
                src,
                new_type,
            } => {
                let new_t = type_to_string(new_type);
                writeln!(
                    out,
                    "v{}: {} = v{}::refineType({});",
                    dest.0, &new_t, src.0, new_t
                )
                .unwrap();
            }
            Instruction::Nop => {}

            x => writeln!(out, "{:?}", x).unwrap(),
        }
    }
}
