use std::iter;
use std::ops::Deref;
use std::str::FromStr;

use redscript::ast::{Constant, Expr, Ident, LiteralType, Pos, Seq};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{Instr, Offset};
use redscript::definition::{Definition, Local, LocalFlags, ParameterFlags, Type};
use redscript::error::Error;
use strum::{Display, EnumString};

use crate::ir::Conversion;
use crate::scope::{FunctionMatch, FunctionName, Scope};
use crate::{Reference, TypeId};

pub struct Assembler {
    pub code: im::Vector<Instr>,
    pub locals: im::Vector<PoolIndex<Local>>,
    position: u16,
}

impl Assembler {
    fn new() -> Assembler {
        Assembler {
            code: im::Vector::new(),
            locals: im::Vector::new(),
            position: 0,
        }
    }

    fn offset(&self) -> Offset {
        Offset::new(self.position as i16)
    }

    fn prefix(mut self, instr: Instr) -> Self {
        self.position += instr.size();
        self.code.push_front(shifted(instr));
        self
    }

    fn emit(&mut self, instr: Instr) {
        self.position += instr.size();
        self.code.push_back(shifted(instr));
    }

    fn append(&mut self, code: Assembler) {
        self.position += code.position;
        self.code.append(code.code);
        self.locals.append(code.locals);
    }

    fn compile(
        &mut self,
        expr: &Expr,
        expected: Option<&TypeId>,
        pool: &mut ConstantPool,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        panic!();
        Ok(())
    }

    fn compile_call<'a>(
        &mut self,
        function: FunctionMatch,
        args: impl Iterator<Item = &'a Expr>,
        pool: &mut ConstantPool,
        scope: &mut Scope,
        force_static: bool,
        pos: Pos,
    ) -> Result<(), Error> {
        // let fun = pool.function(function.index)?;
        // let flags = fun.flags;
        // let params: Vec<(ParameterFlags, PoolIndex<Type>)> = fun
        //     .parameters
        //     .iter()
        //     .map(|idx| {
        //         let param = pool.parameter(*idx).unwrap();
        //         (param.flags, param.type_)
        //     })
        //     .collect();
        // let name_idx = pool.definition(function.index)?.name;

        // let mut args_code = Assembler::new();
        // for ((arg, conversion), (flags, type_idx)) in args.zip(function.conversions).zip(params) {
        //     let mut arg_code = Assembler::new();
        //     arg_code.compile_conversion(conversion);
        //     let expected_type = scope.resolve_type_from_pool(type_idx, pool, pos)?;
        //     arg_code.compile(arg, Some(&expected_type), pool, scope)?;
        //     if flags.is_short_circuit() {
        //         args_code.emit(Instr::Skip(arg_code.offset()));
        //     }
        //     args_code.append(arg_code);
        // }
        // // for _ in 0..function.unspecified_args {
        // //     args_code.emit(Instr::Nop);
        // // }
        // args_code.emit(Instr::ParamEnd);
        // if !force_static && !flags.is_final() && !flags.is_static() && !flags.is_native() {
        //     self.emit(Instr::InvokeVirtual(args_code.offset(), 0, name_idx));
        // } else {
        //     self.emit(Instr::InvokeStatic(args_code.offset(), 0, function.index));
        // }
        // self.append(args_code);
        Ok(())
    }

    fn compile_conversion(&mut self, conv: Conversion) {
        match conv {
            Conversion::Identity => {}
            Conversion::RefToWeakRef => self.emit(Instr::RefToWeakRef),
            Conversion::WeakRefToRef => self.emit(Instr::WeakRefToRef),
        }
    }

    fn compile_intrinsic(
        &mut self,
        intrinsic: Intrinsic,
        args: &[Expr],
        pool: &mut ConstantPool,
        scope: &mut Scope,
        pos: Pos,
    ) -> Result<(), Error> {
        if args.len() != intrinsic.arg_count().into() {
            let err = format!("Invalid number of arguments for {}", intrinsic);
            Err(Error::CompileError(err, pos))?
        }
        let type_ = scope.infer_type(&args[0], None, pool)?;
        let type_idx = scope.get_type_index(&type_, pool)?;
        match (intrinsic, type_) {
            (Intrinsic::Equals, _) => {
                self.emit(Instr::Equals(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (Intrinsic::NotEquals, _) => {
                self.emit(Instr::NotEquals(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (Intrinsic::ArrayClear, TypeId::Array(_)) => {
                self.emit(Instr::ArrayClear(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (Intrinsic::ArraySize, TypeId::Array(_)) => {
                self.emit(Instr::ArraySize(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (Intrinsic::ArrayResize, TypeId::Array(_)) => {
                self.emit(Instr::ArrayResize(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (Intrinsic::ArrayFindFirst, TypeId::Array(_)) => {
                self.emit(Instr::ArrayFindFirst(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (Intrinsic::ArrayFindLast, TypeId::Array(_)) => {
                self.emit(Instr::ArrayFindLast(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (Intrinsic::ArrayContains, TypeId::Array(_)) => {
                self.emit(Instr::ArrayContains(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (Intrinsic::ArrayCount, TypeId::Array(_)) => {
                self.emit(Instr::ArrayCount(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (Intrinsic::ArrayPush, TypeId::Array(_)) => {
                self.emit(Instr::ArrayPush(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (Intrinsic::ArrayPop, TypeId::Array(_)) => {
                self.emit(Instr::ArrayPop(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (Intrinsic::ArrayInsert, TypeId::Array(_)) => {
                self.emit(Instr::ArrayInsert(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)?;
                self.compile(&args[2], None, pool, scope)
            }
            (Intrinsic::ArrayRemove, TypeId::Array(_)) => {
                self.emit(Instr::ArrayRemove(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (Intrinsic::ArrayGrow, TypeId::Array(_)) => {
                self.emit(Instr::ArrayGrow(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (Intrinsic::ArrayErase, TypeId::Array(_)) => {
                self.emit(Instr::ArrayErase(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (Intrinsic::ArrayLast, TypeId::Array(_)) => {
                self.emit(Instr::ArrayLast(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (Intrinsic::ToString, _) => {
                self.emit(Instr::ToString(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (Intrinsic::EnumInt, _) => {
                self.emit(Instr::EnumToI32(type_idx, 4));
                self.compile(&args[0], None, pool, scope)
            }
            (Intrinsic::IntEnum, _) => {
                self.emit(Instr::I32ToEnum(type_idx, 4));
                self.compile(&args[0], None, pool, scope)
            }
            (Intrinsic::ToVariant, _) => {
                self.emit(Instr::ToVariant(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (Intrinsic::FromVariant, _) => {
                self.emit(Instr::ToVariant(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (Intrinsic::AsRef, TypeId::ScriptRef(_)) => {
                self.emit(Instr::AsRef(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (Intrinsic::Deref, _) => {
                self.emit(Instr::Deref(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (_, type_) => {
                let err = format!("Invalid intrinsic {} call: {:?}", intrinsic, type_);
                Err(Error::CompileError(err, pos))
            }
        }
    }

    fn from_instr(instr: Instr) -> Assembler {
        let mut code = Assembler::new();
        code.emit(instr);
        code
    }

    fn from_expr(
        expr: &Expr,
        expected: Option<&TypeId>,
        pool: &mut ConstantPool,
        scope: &mut Scope,
    ) -> Result<Assembler, Error> {
        let mut code = Assembler::new();
        code.compile(expr, expected, pool, scope)?;
        Ok(code)
    }

    pub fn from_seq(seq: &Seq, pool: &mut ConstantPool, scope: &mut Scope) -> Result<Assembler, Error> {
        let mut code = Assembler::new();
        for expr in &seq.exprs {
            code.compile(expr, None, pool, scope)?;
        }
        Ok(code)
    }

    fn get_static_reference(expr: &Expr, scope: &Scope) -> Option<Reference> {
        if let Expr::Ident(ident, _) = expr.deref() {
            match scope.resolve_reference(ident.clone(), Pos::ZERO).ok()? {
                r @ Reference::Class(_) => Some(r),
                r @ Reference::Enum(_) => Some(r),
                _ => None,
            }
        } else {
            None
        }
    }
}

fn shifted(instr: Instr) -> Instr {
    let size = instr.size() as i16;
    match instr {
        Instr::InvokeStatic(offset, line, idx) => Instr::InvokeStatic(Offset::new(offset.value + size), line, idx),
        Instr::InvokeVirtual(offset, line, idx) => Instr::InvokeVirtual(Offset::new(offset.value + size), line, idx),
        Instr::Switch(idx, offset) => Instr::Switch(idx, Offset::new(offset.value + size)),
        Instr::SwitchLabel(start, exit) => {
            Instr::SwitchLabel(Offset::new(start.value + size), Offset::new(exit.value + size))
        }
        Instr::Skip(offset) => Instr::Skip(Offset::new(offset.value + size)),
        Instr::Conditional(true_, false_) => {
            Instr::Conditional(Offset::new(true_.value + size), Offset::new(false_.value + size))
        }
        Instr::Context(offset) => Instr::Context(Offset::new(offset.value + size)),
        Instr::Jump(offset) if offset.value > 0 => Instr::Jump(Offset::new(offset.value + size)),
        Instr::JumpIfFalse(offset) if offset.value > 0 => Instr::JumpIfFalse(Offset::new(offset.value + size)),
        Instr::Jump(offset) if offset.value < 0 => Instr::Jump(Offset::new(offset.value - size)),
        Instr::JumpIfFalse(offset) if offset.value < 0 => Instr::JumpIfFalse(Offset::new(offset.value - size)),
        other => other,
    }
}
