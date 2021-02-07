use std::str::FromStr;
use std::{iter, ops::Deref};

use redscript::ast::{Constant, Expr, Ident, LiteralType, Pos, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{Class, Definition, DefinitionValue, Enum, Field, Function, Local, Type};
use redscript::error::Error;

use crate::ir::{Conversion, IR, find_conversion};
use crate::{ir::Intrinsic, parser::FunctionSource};
use crate::{Reference, TypeId};

#[derive(Debug, Clone)]
pub struct FunctionOverloads(pub Vec<PoolIndex<Function>>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionName {
    namespace: Option<PoolIndex<Class>>,
    name: Ident,
}

impl FunctionName {
    pub fn global(name: Ident) -> FunctionName {
        FunctionName { namespace: None, name }
    }

    pub fn instance(class: PoolIndex<Class>, name: Ident) -> FunctionName {
        FunctionName {
            namespace: Some(class),
            name,
        }
    }

    pub fn pretty(&self, pool: &ConstantPool) -> String {
        self.namespace
            .and_then(|c| pool.definition_name(c).ok())
            .map(|n| format!("{}::{}", n, self.name))
            .unwrap_or(self.name.to_string())
    }
}

#[derive(Debug)]
pub struct FunctionMatch {
    pub index: PoolIndex<Function>,
    pub args: Vec<IR>
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub functions: im::HashMap<FunctionName, FunctionOverloads>,
    pub references: im::HashMap<Ident, Reference>,
    pub types: im::HashMap<Ident, PoolIndex<Type>>,
    pub this: Option<PoolIndex<Class>>,
    pub function: Option<PoolIndex<Function>>,
}

impl Scope {
    pub fn new(pool: &ConstantPool) -> Result<Self, Error> {
        let names = pool
            .roots()
            .filter_map(|(idx, def)| {
                let id = Ident(pool.definition_name(idx).unwrap());
                match def.value {
                    DefinitionValue::Class(_) => Some((id, Reference::Class(idx.cast()))),
                    DefinitionValue::Enum(_) => Some((id, Reference::Enum(idx.cast()))),
                    _ => None,
                }
            })
            .collect();

        let types = pool
            .roots()
            .filter_map(|(idx, def)| match def.value {
                DefinitionValue::Type(_) => {
                    let id = pool.definition_name(idx).unwrap();
                    Some((Ident(id), idx.cast()))
                }
                _ => None,
            })
            .collect();

        let mut result = Scope {
            functions: im::HashMap::new(),
            references: names,
            types,
            this: None,
            function: None,
        };

        for (idx, def) in pool.definitions() {
            if let DefinitionValue::Function(_) = def.value {
                let mangled_name = pool.definition_name(idx)?;
                let ident = Ident::new(mangled_name.split(";").next().unwrap().to_owned());
                let name = if def.parent != PoolIndex::UNDEFINED {
                    FunctionName::instance(def.parent.cast(), ident)
                } else {
                    FunctionName::global(ident)
                };
                result.push_function(name, idx.cast())
            }
        }

        Ok(result)
    }

    pub fn with_context(&self, this: Option<PoolIndex<Class>>, function: PoolIndex<Function>) -> Self {
        Scope {
            this,
            function: Some(function),
            ..self.clone()
        }
    }

    pub fn push_local(&mut self, name: Ident, local: PoolIndex<Local>) {
        self.references.insert(name, Reference::Local(local));
    }

    pub fn push_function(&mut self, name: FunctionName, index: PoolIndex<Function>) {
        self.functions
            .entry(name)
            .and_modify(|overloads: &mut FunctionOverloads| overloads.0.push(index))
            .or_insert_with(|| FunctionOverloads(vec![index]));
    }

    pub fn resolve_field(
        &self,
        ident: Ident,
        class_idx: PoolIndex<Class>,
        pool: &ConstantPool,
        pos: Pos,
    ) -> Result<PoolIndex<Field>, Error> {
        let class = pool.class(class_idx)?;
        for field in &class.fields {
            if pool.definition_name(*field)? == ident.0 {
                return Ok(*field);
            }
        }
        let err = format!("Field {} not found on {}", ident, pool.definition_name(class_idx)?);
        if class.base != PoolIndex::UNDEFINED {
            self.resolve_field(ident, class.base, pool, pos)
                .map_err(|_| Error::CompileError(err, pos))
        } else {
            Err(Error::CompileError(err, pos))
        }
    }

    pub fn resolve_enum_member(
        &self,
        ident: Ident,
        enum_idx: PoolIndex<Enum>,
        pool: &ConstantPool,
        pos: Pos,
    ) -> Result<PoolIndex<i64>, Error> {
        let enum_ = pool.enum_(enum_idx)?;
        for field in &enum_.members {
            if pool.definition_name(*field)? == ident.0 {
                return Ok(*field);
            }
        }
        let err = format!("Member {} not found on {}", ident, pool.definition_name(enum_idx)?);
        Err(Error::CompileError(err, pos))
    }

    pub fn resolve_reference(&self, name: Ident, pos: Pos) -> Result<Reference, Error> {
        self.references
            .get(&name)
            .cloned()
            .ok_or(Error::CompileError(format!("Unresolved reference {}", name), pos))
    }

    pub fn get_type_index(&mut self, type_: &TypeId, pool: &mut ConstantPool) -> Result<PoolIndex<Type>, Error> {
        let name = type_.repr(pool)?;
        if let Some(t) = self.types.get(&name) {
            Ok(*t)
        } else {
            let name_idx = pool.names.add(name.0.deref().clone());
            let value = match type_ {
                TypeId::Prim(_) => Type::Prim,
                TypeId::Class(_) | TypeId::Struct(_) | TypeId::Enum(_) => Type::Class,
                TypeId::Ref(inner) => Type::Ref(self.get_type_index(inner, pool)?),
                TypeId::WeakRef(inner) => Type::WeakRef(self.get_type_index(inner, pool)?),
                TypeId::Array(inner) => Type::Array(self.get_type_index(inner, pool)?),
                TypeId::StaticArray(inner, size) => Type::StaticArray(self.get_type_index(inner, pool)?, *size),
                TypeId::ScriptRef(inner) => Type::ScriptRef(self.get_type_index(inner, pool)?),
                TypeId::Null | TypeId::Void => panic!(),
            };
            let idx = pool.push_definition(Definition::type_(name_idx, value)).cast();
            self.types.insert(name, idx);
            Ok(idx)
        }
    }

    pub fn resolve_type<S: AsRef<str>>(
        &self,
        name: &TypeName<S>,
        pool: &ConstantPool,
        location: Pos,
    ) -> Result<TypeId, Error> {
        let result = if let Some(res) = self.types.get(&Ident::new(name.repr())) {
            self.resolve_type_from_pool(*res, pool, location)?
        } else {
            match (name.name.as_ref(), name.arguments.as_slice()) {
                ("ref", [nested]) => TypeId::Ref(Box::new(self.resolve_type(nested, pool, location)?)),
                ("wref", [nested]) => TypeId::WeakRef(Box::new(self.resolve_type(nested, pool, location)?)),
                ("script_ref", [nested]) => TypeId::ScriptRef(Box::new(self.resolve_type(nested, pool, location)?)),
                ("array", [nested]) => TypeId::Array(Box::new(self.resolve_type(nested, pool, location)?)),
                _ => Err(Error::CompileError(format!("Unresolved type {}", name), location))?,
            }
        };
        Ok(result)
    }

    pub fn resolve_type_from_pool(
        &self,
        index: PoolIndex<Type>,
        pool: &ConstantPool,
        pos: Pos,
    ) -> Result<TypeId, Error> {
        let result = match pool.type_(index)? {
            Type::Prim => TypeId::Prim(index),
            Type::Class => {
                let ident = Ident(pool.definition_name(index)?);
                if let Some(Reference::Class(class_idx)) = self.references.get(&ident) {
                    if pool.class(*class_idx)?.flags.is_struct() {
                        TypeId::Struct(*class_idx)
                    } else {
                        TypeId::Class(*class_idx)
                    }
                } else if let Some(Reference::Enum(enum_idx)) = self.references.get(&ident) {
                    TypeId::Enum(*enum_idx)
                } else {
                    Err(Error::CompileError(format!("Class {} not found", ident), pos))?
                }
            }
            Type::Ref(type_) => {
                let inner = self.resolve_type_from_pool(*type_, pool, pos)?;
                TypeId::Ref(Box::new(inner))
            }
            Type::WeakRef(type_) => {
                let inner = self.resolve_type_from_pool(*type_, pool, pos)?;
                TypeId::WeakRef(Box::new(inner))
            }
            Type::Array(type_) => {
                let inner = self.resolve_type_from_pool(*type_, pool, pos)?;
                TypeId::Array(Box::new(inner))
            }
            Type::StaticArray(type_, size) => {
                let inner = self.resolve_type_from_pool(*type_, pool, pos)?;
                TypeId::StaticArray(Box::new(inner), *size)
            }
            Type::ScriptRef(type_) => {
                let inner = self.resolve_type_from_pool(*type_, pool, pos)?;
                TypeId::ScriptRef(Box::new(inner))
            }
        };
        Ok(result)
    }
}

pub struct FunctionId<'a>(pub &'a str, String);

impl<'a> FunctionId<'a> {
    pub fn mangled(&self) -> String {
        format!("{};{}", self.0, self.1)
    }

    pub fn from_source(source: &'a FunctionSource) -> Result<Self, Error> {
        let mut signature = String::new();
        for arg in &source.parameters {
            signature.push_str(arg.type_.mangled().as_str());
        }
        Ok(FunctionId(&source.declaration.name, signature))
    }
}
