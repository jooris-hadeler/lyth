use std::collections::HashMap;
use thiserror::Error;
use crate::parser::ast::{ASTVisitor, Block, Expr, FuncDef, Module, Param, Stmt, StmtKind, Type, TypeDef, TypeKind};

#[derive(Clone, Debug, Error)]
pub enum TypeError {

}

pub struct TypeChecker {
    variable_scopes: Vec<HashMap<Box<str>, TypeKind>>,
    current_signature: Option<(Vec<TypeKind>, Option<Box<TypeKind>>)>,
}

impl TypeChecker {
    fn push_scope(&mut self) {
        self.variable_scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.variable_scopes.pop();
    }

    fn put_variable(&mut self, name: &Box<str>, kind: &TypeKind) {
        self.variable_scopes
            .last_mut()
            .unwrap()
            .insert(name.clone(), kind.clone());
    }

    fn put_function_signature(&mut self, name: &Box<str>, params: &Vec<Param>, return_type: &Option<Type>) {
        let param_types = params.iter()
            .map(|param| param.typ.kind.clone())
            .collect();

        let return_type = return_type
            .map(|typ| Box::new(typ.kind.clone()));

        let function_type = TypeKind::Function(param_types, return_type);

        self.variable_scopes
            .last_mut()
            .unwrap()
            .insert(name.clone(), function_type);
    }

    // TODO: add type casting
    fn is_type_compatible(&self, first: &TypeKind, second: &TypeKind) -> bool {
        match first {
            TypeKind::Integer(width1, signed1) => match second {
                TypeKind::Integer(width2, signed2) if *signed1 == *signed2 => true,

                _ => false,
            }

            TypeKind::Boolean => matches!(second, TypeKind::Boolean),

            TypeKind::Ref(subtype1) => match second {
                TypeKind::Ref(subtype2) =>
                    self.is_type_compatible(subtype1.as_ref(), subtype2.as_ref()),

                _ => false,
            }

            _ => false,
        }
    }

    pub fn check(&mut self, module: &Module) -> Result<(), TypeError> {
        self.push_scope();

        module.functions
            .iter()
            .for_each(|def| self.put_function_signature(&def.name, &def.params, &def.return_type));

        module.types
            .iter()
            .try_for_each(|def| self.visit_type_def(def))?;

        module.functions
            .iter()
            .try_for_each(|def| self.visit_func_def(def))?;

        self.pop_scope();

        Ok(())
    }
}

impl ASTVisitor for TypeChecker {
    type Error = TypeError;

    type TypeDefReturn = ();

    fn visit_type_def(&mut self, type_def: &TypeDef) -> Result<Self::TypeDefReturn, Self::Error> {
        todo!()
    }

    type FuncDefReturn = ();

    fn visit_func_def(&mut self, func_def: &FuncDef) -> Result<Self::FuncDefReturn, Self::Error> {
        let param_types = func_def.params
            .iter()
            .map(|param| param.typ.kind.clone())
            .collect();

        let return_type = func_def.return_type
            .map(|typ| Box::new(typ.kind.clone()));

        self.current_signature = Some((param_types, return_type));
        self.push_scope();

        // setup parameter types
        func_def.params
            .iter()
            .for_each(|param| self.put_variable(&param.name, &param.typ.kind));

        // visit body
        func_def.body
            .as_ref()
            .map_or(Ok(()), |body| self.visit_block(body))?;

        self.pop_scope();
        self.current_signature = None;

        Ok(())
    }

    type BlockReturn = ();

    fn visit_block(&mut self, block: &Block) -> Result<Self::BlockReturn, Self::Error> {
        block.0
            .iter()
            .try_for_each(|stmt| self.visit_stmt(stmt))?;

        Ok(())
    }
    
    type StmtReturn = ();

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<Self::StmtReturn, Self::Error> {
        match &stmt.kind {
            StmtKind::Expr(expr) => self.visit_expr(expr)?,
            StmtKind::Let(name, typ, value) => {
                self.put_variable(name, &typ.kind);

                if let Some(value) = value {
                    let value_type = self.visit_expr(value)?;

                    // type in-compatability: type.kind, value_type
                    if true {
                        todo!("incompatible types")
                    }
                }
            }
            StmtKind::Return(value) => {
                let return_type = self.current_signature
                    .clone()
                    .unwrap()
                    .1;

                match value {
                    Some(value) => {
                        let value_type = self.visit_expr(value)?;

                        if let Some(return_type) = return_type {
                            if value_type != *return_type {
                                todo!("incompatible return type")
                            }
                        } else {
                            todo!("unexpected return value")
                        }
                    }
                    None => {
                        if let Some(return_type) = return_type {
                            todo!("expected return value")
                        }
                    }
                }

            }
        }

        Ok(())
    }

    type ExprReturn = TypeKind;

    fn visit_expr(&mut self, expr: &Expr) -> Result<Self::ExprReturn, Self::Error> {
        todo!()
    }
}