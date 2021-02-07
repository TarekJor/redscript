use std::iter;
use std::ops::Deref;
use std::str::FromStr;

use redscript::{
    ast::{Constant, Expr, Ident, Pos, Seq, TypeName},
    bundle::{ConstantPool, PoolIndex},
    definition::{Class, Definition, Enum, Field, Function, Local, LocalFlags, Parameter, ParameterFlags, Type},
    error::Error,
};
use strum::{Display, EnumString};

use crate::{
    scope::{FunctionMatch, FunctionName, Scope},
    Reference, TypeId,
};

#[derive(Debug, Clone)]
pub enum IR {
    Constant(Constant),
    Local(PoolIndex<Local>),
    Param(PoolIndex<Parameter>),
    Field(Box<IR>, PoolIndex<Field>),
    Enum(PoolIndex<Enum>, PoolIndex<i64>),
    Store(Box<IR>, Box<IR>),
    Invoke(PoolIndex<Function>, Option<Box<IR>>, Vec<IR>),
    Intrinsic(Intrinsic, Vec<IR>, TypeId),
    New(PoolIndex<Class>, Vec<IR>),
    Return(Option<Box<IR>>),
    Cast(PoolIndex<Type>, Box<IR>),
    Seq(Vec<IR>),
    Switch(Box<IR>, Vec<IRCase>, Option<Box<IR>>),
    Conditional(Box<IR>, Box<IR>, Box<IR>),
    JumpIfFalse(Box<IR>, Label),
    Jump(Label),
    Label(Label),
    Convert(Conversion, Box<IR>),
    Null,
    This,
    Nop,
}

impl IR {
    pub fn type_(&self, scope: &Scope, pool: &ConstantPool, pos: Pos) -> Result<TypeId, Error> {
        let res = match self {
            IR::Constant(cons) => match cons {
                Constant::String(_, _) => scope.resolve_type(&TypeName::STRING, pool, pos)?,
                Constant::Float(_) => scope.resolve_type(&TypeName::FLOAT, pool, pos)?,
                Constant::Int(_) => scope.resolve_type(&TypeName::INT32, pool, pos)?,
                Constant::Uint(_) => scope.resolve_type(&TypeName::UINT32, pool, pos)?,
                Constant::Bool(_) => scope.resolve_type(&TypeName::BOOL, pool, pos)?,
            },
            IR::Local(idx) => scope.resolve_type_from_pool(pool.local(*idx)?.type_, pool, pos)?,
            IR::Param(idx) => scope.resolve_type_from_pool(pool.parameter(*idx)?.type_, pool, pos)?,
            IR::Field(_, idx) => scope.resolve_type_from_pool(pool.field(*idx)?.type_, pool, pos)?,
            IR::Store(_, _) => TypeId::Void,
            IR::Invoke(idx, _, _) => match pool.function(*idx)?.return_type {
                Some(idx) => scope.resolve_type_from_pool(idx, pool, pos)?,
                None => TypeId::Void,
            },
            IR::Intrinsic(_, _, ret) => *ret,
            IR::New(idx, _) => TypeId::Ref(Box::new(TypeId::Class(*idx))),
            IR::Return(_) => TypeId::Void,
            IR::Cast(idx, _) => scope.resolve_type_from_pool(*idx, pool, pos)?, // TODO handle refs
            IR::Seq(_) => TypeId::Void,
            IR::Switch(_, _, _) => TypeId::Void,
            IR::Conditional(_, lhs, _) => lhs.type_(scope, pool, pos)?,
            IR::JumpIfFalse(_, _) => TypeId::Void,
            IR::Jump(_) => TypeId::Void,
            IR::Convert(Conversion::Identity, val) => val.type_(scope, pool, pos)?,
            IR::Convert(Conversion::RefToWeakRef, inner) => {
                if let TypeId::Ref(inner) = inner.type_(scope, pool, pos)? {
                    TypeId::WeakRef(inner)
                } else {
                    Err(Error::CompileError("boom".to_owned(), pos))?
                }
            }
            IR::Convert(Conversion::WeakRefToRef, val) => {
                if let TypeId::WeakRef(inner) = val.type_(scope, pool, pos)? {
                    TypeId::Ref(inner)
                } else {
                    Err(Error::CompileError("boom".to_owned(), pos))?
                }
            }
            IR::Null => TypeId::Null,
            IR::This => {
                if let Some(this) = scope.this {
                    TypeId::Ref(Box::new(TypeId::Class(this)))
                } else {
                    Err(Error::CompileError("boom".to_owned(), pos))?
                }
            }
            IR::Nop => TypeId::Void,
            IR::Enum(enum_, _) => TypeId::Enum(*enum_),
            IR::Label(_) => TypeId::Void,
        };
        Ok(res)
    }

    pub fn convert(self, to: &TypeId, scope: &Scope, pool: &ConstantPool, pos: Pos) -> Result<IR, Error> {
        let type_ = self.type_(scope, pool, pos)?;
        let conv = find_conversion(&type_, to, pool)?.ok_or_else(|| {
            let err = format!(
                "Can't coerce {} to {}",
                type_.pretty(pool).unwrap(),
                to.pretty(pool).unwrap(),
            );
            Error::CompileError(err, pos)
        })?;
        Ok(self.with_conversion(conv))
    }

    pub fn with_conversion(self, conversion: Conversion) -> IR {
        match conversion {
            Conversion::Identity => self,
            Conversion::RefToWeakRef => IR::Convert(Conversion::RefToWeakRef, Box::new(self)),
            Conversion::WeakRefToRef => IR::Convert(Conversion::WeakRefToRef, Box::new(self)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Conversion {
    Identity,
    RefToWeakRef,
    WeakRefToRef,
}

#[derive(Debug, Clone)]
pub struct IRCase(IR, IR);

#[derive(Debug, Clone)]
pub struct Label(u16);

pub struct Codegen<'a> {
    label_counter: u16,
    locals: Vec<PoolIndex<Local>>,
    pool: &'a mut ConstantPool,
}

impl<'a> Codegen<'a> {
    fn new(pool: &'a mut ConstantPool) -> Codegen<'a> {
        Codegen {
            label_counter: 0,
            locals: Vec::new(),
            pool,
        }
    }

    pub fn gen(
        &mut self,
        expr: &Expr,
        scope: &mut Scope,
        expected: Option<&TypeId>,
        exit: Option<Label>,
    ) -> Result<IR, Error> {
        let res = match expr {
            Expr::Ident(name, pos) => match scope.resolve_reference(name.clone(), *pos)? {
                Reference::Local(idx) => IR::Local(idx),
                Reference::Parameter(idx) => IR::Param(idx),
                _ => panic!("Shouldn't get here"),
            },
            Expr::Constant(cons, pos) => IR::Constant(*cons),
            Expr::Declare(name, type_, init, pos) => {
                let name_idx = self.pool.names.add(name.0.deref().to_owned());
                let (type_, ir) = match (type_, init) {
                    (None, None) => Err(Error::CompileError(
                        "Type or initializer required on let binding".to_owned(),
                        *pos,
                    ))?,
                    (None, Some(val)) => {
                        let ir = self.gen(val.deref(), scope, None, None)?;
                        (ir.type_(scope, self.pool, *pos)?, Some(ir))
                    }
                    (Some(type_name), None) => (scope.resolve_type(&type_name, self.pool, *pos)?, None),
                    (Some(type_name), Some(val)) => {
                        let type_ = scope.resolve_type(&type_name, self.pool, *pos)?;
                        let ir = self.gen(val.deref(), scope, Some(&type_), None)?;
                        (type_, Some(ir.convert(&type_, scope, self.pool, *pos)?))
                    }
                };
                let local = Local::new(scope.get_type_index(&type_, self.pool)?, LocalFlags::new());
                let def = Definition::local(name_idx, scope.function.unwrap().cast(), local);
                let idx = self.pool.push_definition(def).cast();
                self.locals.push(idx);
                scope.push_local(name.clone(), idx);
                if let Some(val) = ir {
                    IR::Store(Box::new(IR::Local(idx)), Box::new(val))
                } else {
                    IR::Nop
                }
            }
            Expr::Assign(lhs, rhs, pos) => {
                let lhs = self.gen(lhs.deref(), scope, None, None)?;
                let rhs = self.gen(rhs.deref(), scope, Some(&lhs.type_(scope, self.pool, *pos)?), None)?;
                let type_ = lhs.type_(scope, self.pool, *pos)?;
                IR::Store(Box::new(lhs), Box::new(rhs.convert(&type_, scope, self.pool, *pos)?))
            }
            Expr::Cast(type_name, expr, pos) => {
                let type_ = scope.resolve_type(&type_name, self.pool, *pos)?;
                IR::Cast(
                    scope.get_type_index(&type_, self.pool)?,
                    Box::new(self.gen(expr.deref(), scope, None, None)?),
                )
            }
            Expr::Call(ident, args, pos) => {
                if let Ok(intrinsic) = Intrinsic::from_str(&ident.0) {
                    self.gen_intrinsic(intrinsic, &args, scope, expected, *pos)?
                } else {
                    let name = FunctionName::global(ident.clone());
                    let match_ = self.resolve_function(name, args.iter(), expected, scope, *pos)?;
                    let mut args_ir = Vec::with_capacity(args.len());
                    for arg in args {
                        args_ir.push(self.gen(arg, scope, expected, None)?);
                    }
                    IR::Invoke(match_.index, None, args_ir)
                }
            }
            Expr::MethodCall(expr, ident, args, pos) => {
                let static_ref = Self::get_static_reference(&expr, scope);
                let ir = self.gen(expr.deref(), scope, None, None)?;
                let type_ = ir.type_(scope, self.pool, *pos)?;
                if let Some(class) = static_ref {
                    // let irs = args.into_iter().map(|x| self.gen(x, scope, expected, exit))
                    let match_ = self.resolve_method(ident.clone(), class, &args, expected, scope, *pos)?;
                    let fun = self.pool.function(match_.index)?;
                    if fun.flags.is_static() {
                        self.gen_call(match_, None, args.iter(), scope, true, *pos)?
                    } else {
                        Err(Error::CompileError(format!("Method {} is not static", ident), *pos))?
                    }
                } else if let TypeId::Class(class) = type_.unwrapped() {
                    let force_static_call = if let Expr::Super(_) = expr.as_ref() {
                        true
                    } else {
                        false
                    };

                    let fun = self.resolve_method(ident.clone(), *class, &args, expected, scope, *pos)?;
                    self.gen_call(fun, Some(expr), args.into_iter(), scope, force_static_call, *pos)?
                } else {
                    let err = format!("Can't call methods on {}", type_.pretty(self.pool)?);
                    Err(Error::CompileError(err, *pos))?
                }
            }
            Expr::BinOp(lhs, rhs, op, pos) => {
                let ident = Ident::new(op.name());
                let args = iter::once(lhs.as_ref()).chain(iter::once(rhs.as_ref()));
                let fun = self.resolve_function(FunctionName::global(ident), args, expected, scope, *pos)?;
                self.gen_call(fun, None, args, scope, true, *pos)?
            }
            Expr::UnOp(expr, op, pos) => {
                let ident = Ident::new(op.name());
                let args = iter::once(expr.as_ref());
                let fun = self.resolve_function(FunctionName::global(ident), args, expected, scope, *pos)?;
                self.gen_call(fun, None, args, scope, true, *pos)?
            }
            Expr::Member(expr, ident, pos) => {
                let ir = self.gen(&expr, scope, None, None)?;
                match ir.type_(scope, self.pool, *pos)?.unwrapped() {
                    TypeId::Class(class) => {
                        let object = self.gen(&expr, scope, None, None)?;
                        let field = scope.resolve_field(ident.clone(), *class, self.pool, *pos)?;
                        IR::Field(Box::new(object), field)
                    }
                    TypeId::Struct(class) => {
                        let object = self.gen(&expr, scope, None, None)?;
                        let field = scope.resolve_field(ident.clone(), *class, self.pool, *pos)?;
                        IR::Field(Box::new(object), field)
                    }
                    TypeId::Enum(enum_) => {
                        let member_idx = scope.resolve_enum_member(ident.clone(), *enum_, self.pool, *pos)?;
                        IR::Enum(*enum_, member_idx)
                    }
                    t => {
                        let err = format!("Can't access a member of {}", t.pretty(self.pool)?);
                        Err(Error::CompileError(err, *pos))?
                    }
                }
            }
            Expr::ArrayElem(expr, idx, pos) => {
                // self.gen_intrinsic(Intrinsic::ArrayElement, &vec![expr, idx], scope, None, *pos)?
                panic!()
            }
            Expr::New(name, args, pos) => match scope.resolve_reference(name.clone(), *pos)? {
                Reference::Class(idx) => {
                    let cls = self.pool.class(idx)?;
                    if cls.flags.is_struct() {
                        if cls.fields.len() != args.len() {
                            let err = format!("Expected {} parameters for {}", cls.fields.len(), name);
                            Err(Error::CompileError(err, *pos))?
                        }
                        let fields = cls.fields.clone();
                        let mut args_ir = Vec::with_capacity(fields.len());
                        for (arg, field_idx) in args.into_iter().zip(fields) {
                            let field = self.pool.field(field_idx)?;
                            let field_type = scope.resolve_type_from_pool(field.type_, self.pool, *pos)?;
                            let ir = self.gen(arg, scope, Some(&field_type), None)?.convert(
                                &field_type,
                                scope,
                                self.pool,
                                *pos,
                            )?;
                            args_ir.push(ir);
                        }
                        IR::New(idx, args_ir)
                    } else if args.is_empty() {
                        IR::New(idx, vec![])
                    } else {
                        let err = format!("Expected 0 parameters for {}", name);
                        Err(Error::CompileError(err, *pos))?
                    }
                }
                _ => Err(Error::CompileError(format!("Cannot construct {}", name), *pos))?,
            },
            Expr::Return(Some(expr), pos) => {
                let fun = self.pool.function(scope.function.unwrap())?;
                if let Some(ret_type) = fun.return_type {
                    let expected = scope.resolve_type_from_pool(ret_type, self.pool, *pos)?;
                    let ir = self
                        .gen(&expr, scope, Some(&expected), None)?
                        .convert(&expected, scope, self.pool, *pos)?;
                    IR::Return(Some(Box::new(ir)))
                } else {
                    Err(Error::CompileError(format!("Function should return nothing"), *pos))?
                }
            }
            Expr::Return(None, pos) => {
                let fun = self.pool.function(scope.function.unwrap())?;
                if let None = fun.return_type {
                    IR::Return(None)
                } else {
                    Err(Error::CompileError(format!("Function should return a value"), *pos))?
                }
            }
            Expr::Seq(seq) => self.gen_seq(seq, scope, exit)?,
            Expr::Switch(expr, cases, default) => {
                let exit = self.new_label();
                let matched = self.gen(&expr, scope, None, None)?;
                let mut result = Vec::with_capacity(cases.len());
                for case in cases {
                    let matcher = self.gen(&case.0, scope, None, None)?;
                    let body = self.gen_seq(&case.1, &mut scope.clone(), Some(exit))?;
                    result.push(IRCase(matcher, body))
                }
                let default_ir = if let Some(body) = default {
                    Some(self.gen_seq(body, &mut scope.clone(), None)?)
                } else {
                    None
                };
                IR::Switch(Box::new(matched), result, default_ir.map(Box::new))
            }
            Expr::If(cond, if_, else_) => {
                let cond_type = scope.resolve_type(&TypeName::BOOL, self.pool, Pos::ZERO)?;
                let cond_code = self.gen(&cond, scope, Some(&cond_type), None)?;
                let if_code = self.gen_seq(if_, &mut scope.clone(), exit)?;
                let else_label = self.new_label();

                let mut ir = vec![IR::JumpIfFalse(Box::new(cond_code), else_label), if_code];

                if let Some(else_expr) = else_ {
                    let end_label = self.new_label();
                    ir.push(IR::Jump(end_label));
                    ir.push(IR::Label(else_label));
                    ir.push(self.gen_seq(else_expr, &mut scope.clone(), exit)?);
                    ir.push(IR::Label(end_label));
                } else {
                    ir.push(IR::Label(else_label));
                };
                IR::Seq(ir)
            }
            Expr::Conditional(cond, true_, false_, _) => {
                let cond_code = self.gen(&cond, scope, None, None)?;
                let true_code = self.gen(&true_, scope, expected, None)?;
                let false_code = self.gen(&false_, scope, expected, None)?;
                IR::Conditional(Box::new(cond_code), Box::new(true_code), Box::new(false_code))
            }
            Expr::While(cond, body) => {
                let exit = self.new_label();
                let cond_code = self.gen(&cond, scope, None, None)?;
                let body_code = self.gen_seq(body, scope, Some(exit))?;
                let ir = vec![cond_code, body_code, IR::Label(exit)];
                IR::Seq(ir)
            }
            Expr::This(_) => IR::This,
            Expr::Super(_) => IR::This,
            Expr::Break if exit.is_some() => IR::Jump(exit.unwrap()),
            Expr::Break => Err(Error::CompileError("boom".to_owned(), Pos::ZERO))?,
            Expr::Null => IR::Null,
            Expr::Goto(_, _) => panic!("goto not supported"),
        };
        Ok(res)
    }

    fn gen_seq(&mut self, seq: &Seq, scope: &mut Scope, exit: Option<Label>) -> Result<IR, Error> {
        let mut ir = Vec::with_capacity(seq.exprs.len());
        for expr in seq.exprs {
            ir.push(self.gen(&expr, scope, None, exit)?);
        }
        Ok(IR::Seq(ir))
    }

    fn gen_call<'b>(
        &mut self,
        function: FunctionMatch,
        obj: Option<&'b Expr>,
        args: impl Iterator<Item = &'b Expr>,
        scope: &mut Scope,
        force_static: bool,
        pos: Pos,
    ) -> Result<IR, Error> {
        let fun = self.pool.function(function.index)?;
        let flags = fun.flags;
        let params: Vec<(ParameterFlags, PoolIndex<Type>)> = fun
            .parameters
            .iter()
            .map(|idx| {
                let param = self.pool.parameter(*idx).unwrap();
                (param.flags, param.type_)
            })
            .collect();
        let obj_ir = match obj {
            Some(obj) => {
                let ir = self.gen(obj, scope, None, None)?;
                if let TypeId::WeakRef(_) = ir.type_(scope, self.pool, pos)? {
                    Some(ir.with_conversion(Conversion::WeakRefToRef))
                } else {
                    Some(ir)
                }
            }
            None => None,
        };

        let mut args_ir = Vec::new();
        for (arg, (flags, type_idx)) in args.zip(params) {
            let expected = scope.resolve_type_from_pool(type_idx, self.pool, pos)?;
            let arg_ir = self.gen(arg, scope, Some(&expected), None)?;
            args_ir.push(arg_ir)
        }

        Ok(IR::Invoke(function.index, obj_ir.map(Box::new), args_ir))
    }

    fn gen_intrinsic(
        &mut self,
        intrinsic: Intrinsic,
        args: &[Expr],
        scope: &mut Scope,
        expected: Option<&TypeId>,
        pos: Pos,
    ) -> Result<IR, Error> {
        if args.len() != intrinsic.arg_count().into() {
            let err = format!("Invalid number of arguments for {}", intrinsic);
            Err(Error::CompileError(err, pos))?
        }
        let subject = self.gen(&args[0], scope, None, None)?;
        let type_ = subject.type_(scope, self.pool, pos)?;
        let res = match (intrinsic, type_) {
            (Intrinsic::Equals, _) => {
                let rhs = self.gen(&args[1], scope, None, None)?;
                let result_type = scope.resolve_type(&TypeName::BOOL, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::Equals, vec![subject, rhs], result_type)
            }
            (Intrinsic::NotEquals, _) => {
                let rhs = self.gen(&args[1], scope, None, None)?;
                let result_type = scope.resolve_type(&TypeName::BOOL, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::NotEquals, vec![subject, rhs], result_type)
            }
            (Intrinsic::ArrayElement, TypeId::Array(elem)) => {
                let idx_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                let idx = self
                    .gen(&args[1], scope, None, None)?
                    .convert(&idx_type, scope, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ArrayElement, vec![subject, idx], *elem)
            }
            (Intrinsic::ArrayClear, TypeId::Array(_)) => {
                IR::Intrinsic(Intrinsic::ArrayClear, vec![subject], TypeId::Void)
            }
            (Intrinsic::ArraySize, TypeId::Array(_)) => {
                let result_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ArraySize, vec![subject], result_type)
            }
            (Intrinsic::ArrayResize, TypeId::Array(_)) => {
                let size_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                let size = self
                    .gen(&args[1], scope, None, None)?
                    .convert(&size_type, scope, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ArrayResize, vec![subject, size], TypeId::Void)
            }
            (Intrinsic::ArrayFindFirst, TypeId::Array(elem)) => {
                let needle = self
                    .gen(&args[1], scope, None, None)?
                    .convert(&elem, scope, self.pool, pos)?;
                let result_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ArrayFindFirst, vec![subject, needle], result_type)
            }
            (Intrinsic::ArrayFindLast, TypeId::Array(elem)) => {
                let needle = self
                    .gen(&args[1], scope, None, None)?
                    .convert(&elem, scope, self.pool, pos)?;
                let result_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ArrayFindLast, vec![subject, needle], result_type)
            }
            (Intrinsic::ArrayContains, TypeId::Array(elem)) => {
                let needle = self
                    .gen(&args[1], scope, None, None)?
                    .convert(&elem, scope, self.pool, pos)?;
                let result_type = scope.resolve_type(&TypeName::BOOL, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ArrayContains, vec![subject, needle], result_type)
            }
            (Intrinsic::ArrayCount, TypeId::Array(elem)) => {
                let needle = self
                    .gen(&args[1], scope, None, None)?
                    .convert(&elem, scope, self.pool, pos)?;
                let result_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ArrayCount, vec![subject, needle], result_type)
            }
            (Intrinsic::ArrayPush, TypeId::Array(elem)) => {
                let val = self
                    .gen(&args[1], scope, None, None)?
                    .convert(&elem, scope, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ArrayPush, vec![subject, val], TypeId::Void)
            }
            (Intrinsic::ArrayPop, TypeId::Array(elem)) => IR::Intrinsic(Intrinsic::ArrayPop, vec![subject], *elem),
            (Intrinsic::ArrayInsert, TypeId::Array(elem)) => {
                let idx_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                let idx = self
                    .gen(&args[1], scope, None, None)?
                    .convert(&idx_type, scope, self.pool, pos)?;
                let val = self
                    .gen(&args[2], scope, None, None)?
                    .convert(&elem, scope, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ArrayInsert, vec![subject, idx, val], TypeId::Void)
            }
            (Intrinsic::ArrayRemove, TypeId::Array(elem)) => {
                let val = self
                    .gen(&args[1], scope, None, None)?
                    .convert(&elem, scope, self.pool, pos)?;
                let result_type = scope.resolve_type(&TypeName::BOOL, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ArrayRemove, vec![subject, val], result_type)
            }
            (Intrinsic::ArrayGrow, TypeId::Array(_)) => {
                let size_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                let size = self
                    .gen(&args[1], scope, None, None)?
                    .convert(&size_type, scope, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ArrayGrow, vec![subject, size], TypeId::Void)
            }
            (Intrinsic::ArrayErase, TypeId::Array(_)) => {
                let idx_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                let idx = self
                    .gen(&args[1], scope, None, None)?
                    .convert(&idx_type, scope, self.pool, pos)?;
                let result_type = scope.resolve_type(&TypeName::BOOL, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ArrayRemove, vec![subject, idx], result_type)
            }
            (Intrinsic::ArrayLast, TypeId::Array(elem)) => IR::Intrinsic(Intrinsic::ArrayPop, vec![subject], *elem),
            (Intrinsic::ToString, _) => {
                let result_type = scope.resolve_type(&TypeName::STRING, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ToString, vec![subject], result_type)
            }
            (Intrinsic::EnumInt, _) => {
                let result_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::EnumInt, vec![subject], result_type)
            }
            (Intrinsic::IntEnum, _) if expected.is_some() => {
                IR::Intrinsic(Intrinsic::EnumInt, vec![subject], *expected.unwrap())
            }
            (Intrinsic::ToVariant, _) => {
                let result_type = scope.resolve_type(&TypeName::VARIANT, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ToVariant, vec![subject], result_type)
            }
            (Intrinsic::FromVariant, _) if expected.is_some() => {
                let arg_type = scope.resolve_type(&TypeName::VARIANT, self.pool, pos)?;
                let val = subject.convert(&arg_type, scope, self.pool, pos)?;
                IR::Intrinsic(Intrinsic::ToVariant, vec![val], *expected.unwrap())
            }
            (Intrinsic::AsRef, type_) => {
                IR::Intrinsic(Intrinsic::AsRef, vec![subject], TypeId::ScriptRef(Box::new(type_)))
            }
            (Intrinsic::Deref, TypeId::ScriptRef(inner)) => IR::Intrinsic(Intrinsic::Deref, vec![subject], *inner),
            (_, type_) => {
                let err = format!(
                    "Invalid intrinsic {} call with argument {}",
                    intrinsic,
                    type_.pretty(self.pool)?
                );
                Err(Error::CompileError(err, pos))?
            }
        };
        Ok(res)
    }

    pub fn resolve_function<'b>(
        &self,
        name: FunctionName,
        args: impl Iterator<Item = &'b Expr> + Clone,
        expected: Option<&TypeId>,
        scope: &mut Scope,
        pos: Pos,
    ) -> Result<FunctionMatch, Error> {
        let overloads = scope
            .functions
            .get(&name)
            .ok_or_else(|| Error::CompileError(format!("Function {} not found", name.pretty(self.pool)), pos))?;
        let mut errors = Vec::new();

        for fun_idx in overloads.0.iter() {
            match self.resolve_function_overload(*fun_idx, args.clone(), expected, scope, pos) {
                Ok(res) => return Ok(res),
                Err(Error::FunctionResolutionError(msg, _)) => errors.push(msg),
                Err(other) => Err(other)?,
            }
        }
        let message = format!(
            "Arguments passed to {} do not match any of the overloads:\n{}",
            name.pretty(self.pool),
            errors.join("\n")
        );
        Err(Error::FunctionResolutionError(message, pos))
    }

    fn resolve_function_overload<'b>(
        &self,
        fun_idx: PoolIndex<Function>,
        args: impl Iterator<Item = &'b Expr>,
        expected: Option<&TypeId>,
        scope: &mut Scope,
        pos: Pos,
    ) -> Result<FunctionMatch, Error> {
        let fun = self.pool.function(fun_idx)?;

        if let Some(expected) = expected {
            let return_type_idx = fun
                .return_type
                .ok_or(Error::CompileError("Void value cannot be used".to_owned(), pos))?;
            let return_type = scope.resolve_type_from_pool(return_type_idx, self.pool, pos)?;
            if find_conversion(&return_type, &expected, self.pool)?.is_none() {
                let message = format!(
                    "Return type {} does not match expected {}",
                    return_type.pretty(self.pool)?,
                    expected.pretty(self.pool)?
                );
                Err(Error::FunctionResolutionError(message, pos))?;
            }
        }

        let mut args_ir = Vec::new();
        for (idx, arg) in args.enumerate() {
            let param_idx = fun.parameters.get(idx).ok_or_else(|| {
                Error::FunctionResolutionError(format!("Too many arguments, expected {}", fun.parameters.len()), pos)
            })?;
            let param = self.pool.parameter(*param_idx)?;
            let param_type = scope.resolve_type_from_pool(param.type_, self.pool, pos)?;
            let arg = self
                .gen(arg, scope, Some(&param_type), None)?
                .convert(&param_type, scope, self.pool, pos)
                .map_err(|err| match err {
                    Error::CompileError(err, pos) => {
                        let message = format!("Parameter at position {} mismatch: {}", idx, err);
                        Error::FunctionResolutionError(message, pos)
                    }
                    err => err,
                });
        }

        let opt_param_count = fun
            .parameters
            .iter()
            .filter_map(|idx| self.pool.parameter(*idx).ok())
            .filter(|param| param.flags.is_optional())
            .count();

        let min_params = fun.parameters.len() - opt_param_count;
        if args_ir.len() >= min_params {
            let match_ = FunctionMatch {
                index: fun_idx,
                args: args_ir,
            };
            Ok(match_)
        } else {
            let message = format!(
                "Expected {}-{} parameters, given {}",
                min_params,
                fun.parameters.len(),
                args_ir.len()
            );
            Err(Error::FunctionResolutionError(message, pos))
        }
    }

    pub fn resolve_method(
        &self,
        name: Ident,
        class_idx: PoolIndex<Class>,
        args: &[Expr],
        expected: Option<&TypeId>,
        scope: &mut Scope,
        pos: Pos,
    ) -> Result<FunctionMatch, Error> {
        let fun_name = FunctionName::instance(class_idx, name.clone());
        match self.resolve_function(fun_name, args.into_iter(), expected, scope, pos) {
            Ok(res) => Ok(res),
            Err(err) => {
                let class = self.pool.class(class_idx)?;
                if class.base != PoolIndex::UNDEFINED {
                    self.resolve_method(name, class.base, args, expected, scope, pos)
                        .map_err(|base_err| match base_err {
                            err @ Error::FunctionResolutionError(_, _) => err,
                            _ => err,
                        })
                } else {
                    Err(err)
                }
            }
        }
    }

    fn new_label(&mut self) -> Label {
        let cnt = self.label_counter;
        self.label_counter += 1;
        Label(cnt)
    }

    fn get_static_reference(expr: &Expr, scope: &Scope) -> Option<PoolIndex<Class>> {
        if let Expr::Ident(ident, _) = expr.deref() {
            match scope.resolve_reference(ident.clone(), Pos::ZERO).ok()? {
                r @ Reference::Class(idx) => Some(idx),
                _ => None,
            }
        } else {
            None
        }
    }
}

pub fn find_conversion(from: &TypeId, to: &TypeId, pool: &ConstantPool) -> Result<Option<Conversion>, Error> {
    let result = if from == to {
        Some(Conversion::Identity)
    } else {
        match (from, to) {
            (TypeId::Null, TypeId::Ref(_)) => Some(Conversion::Identity),
            (TypeId::Null, TypeId::WeakRef(_)) => Some(Conversion::RefToWeakRef),
            (TypeId::Class(from), TypeId::Class(_)) => {
                let class = pool.class(*from)?;
                if class.base != PoolIndex::UNDEFINED {
                    find_conversion(&TypeId::Class(class.base), to, pool)?
                } else {
                    None
                }
            }
            (from @ TypeId::Class(_), TypeId::Ref(to)) => {
                find_conversion(from, to, pool)?.filter(|conv| *conv == Conversion::Identity)
            }
            (TypeId::Ref(from), TypeId::Ref(to)) => {
                find_conversion(from, to, pool)?.filter(|conv| *conv == Conversion::Identity)
            }
            (TypeId::WeakRef(from), TypeId::WeakRef(to)) => {
                find_conversion(from, to, pool)?.filter(|conv| *conv == Conversion::Identity)
            }
            (TypeId::WeakRef(from), TypeId::Ref(to))
                if find_conversion(from, to, pool)? == Some(Conversion::Identity) =>
            {
                Some(Conversion::WeakRefToRef)
            }
            (TypeId::Ref(from), TypeId::WeakRef(to))
                if find_conversion(from, to, pool)? == Some(Conversion::Identity) =>
            {
                Some(Conversion::RefToWeakRef)
            }
            _ => None,
        }
    };
    Ok(result)
}

#[derive(Debug, Clone, Copy, EnumString, Display)]
pub enum Intrinsic {
    Equals,
    NotEquals,
    ArrayElement,
    ArrayClear,
    ArraySize,
    ArrayResize,
    ArrayFindFirst,
    ArrayFindLast,
    ArrayContains,
    ArrayCount,
    ArrayPush,
    ArrayPop,
    ArrayInsert,
    ArrayRemove,
    ArrayGrow,
    ArrayErase,
    ArrayLast,
    ToString,
    EnumInt,
    IntEnum,
    ToVariant,
    FromVariant,
    AsRef,
    Deref,
}

impl Intrinsic {
    pub fn arg_count(&self) -> u8 {
        match self {
            Intrinsic::Equals => 2,
            Intrinsic::NotEquals => 2,
            Intrinsic::ArrayElement => 2,
            Intrinsic::ArrayClear => 1,
            Intrinsic::ArraySize => 1,
            Intrinsic::ArrayResize => 2,
            Intrinsic::ArrayFindFirst => 2,
            Intrinsic::ArrayFindLast => 2,
            Intrinsic::ArrayContains => 2,
            Intrinsic::ArrayCount => 2,
            Intrinsic::ArrayPush => 2,
            Intrinsic::ArrayPop => 1,
            Intrinsic::ArrayInsert => 3,
            Intrinsic::ArrayRemove => 2,
            Intrinsic::ArrayGrow => 2,
            Intrinsic::ArrayErase => 2,
            Intrinsic::ArrayLast => 1,
            Intrinsic::ToString => 1,
            Intrinsic::EnumInt => 1,
            Intrinsic::IntEnum => 1,
            Intrinsic::ToVariant => 1,
            Intrinsic::FromVariant => 1,
            Intrinsic::AsRef => 1,
            Intrinsic::Deref => 1,
        }
    }
}
