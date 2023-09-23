use std::collections::HashMap;
use crate::parser::ast as ast;

use thiserror::Error;
use crate::lexer::Location;
use crate::parser::ast::{TypeKind, Width};

#[derive(Debug, Clone, Error)]
pub enum CheckerError {}

#[derive(Debug, Clone, Default)]
struct Scope {
    variables: HashMap<Box<str>, Option<ast::Type>>,
}

impl Scope {
    fn add_variable(&mut self, name: Box<str>, typ: Option<ast::Type>) {
        self.variables.insert(name, typ);
    }

    fn set_variable_typ(&mut self, name: Box<str>, typ: ast::Type) {
        self.variables.insert(name, Some(typ));
    }
}

#[derive(Debug, Clone)]
pub struct Checker<'a> {
    module: &'a ast::Module,
    function_signatures: HashMap<Box<str>, (Vec<ast::Type>, Option<ast::Type>)>,
    scopes: Vec<Scope>,
}

impl<'a> Checker<'a> {
    pub fn new(module: &'a ast::Module) -> Self {
        Self { module, function_signatures: HashMap::new(), scopes: Vec::new() }
    }

    pub fn check_module(&mut self) -> Result<(), CheckerError> {
        self.populate_function_signatures();

        // check all functions
        self.module.functions.iter().try_for_each(|f| self.check_function(f))?;

        // for func in self.module.functions.iter() {
        //     self.check_function(func)?;
        // }

        Ok(())
    }

    pub fn populate_function_signatures(&mut self) {
        for func in self.module.functions.iter() {
            let param_types = func.params
                .iter()
                .map(|p| p.typ.clone())
                .collect::<Vec<ast::Type>>();

            self.function_signatures.insert(
                func.name.clone(),
                (param_types, func.return_type.clone()),
            );
        }
    }

    pub fn check_function(&mut self, func: &ast::FuncDef) -> Result<(), CheckerError> {
        if let Some(body) = &func.body {
            self.check_function_body(func, body)?;
        }

        Ok(())
    }

    pub fn check_function_body(&mut self, func: &ast::FuncDef, body: &ast::Block) -> Result<(), CheckerError> {
        self.push_scope();

        body.0.iter().try_for_each(|s| self.check_stmt(func, s))?;

        // for stmt in body.0.iter() {
        //     self.check_stmt(func, stmt)?;
        // }

        self.pop_scope();

        Ok(())
    }

    pub fn check_stmt(&mut self, func: &ast::FuncDef, stmt: &ast::Stmt) -> Result<(), CheckerError> {
        use ast::StmtKind as SK;

        match &stmt.kind {
            SK::Let(name, typ, value) => {
                self.scopes.last_mut().unwrap().add_variable(name.clone(), typ.clone());

                let value_typ = if let Some(value) = value {
                    Some(self.check_expr(value)?)
                } else {
                    None
                };

                // TODO: think about type equality and compatibility
                // if types are unequal or incompatible
                if false {
                    // return an error
                }
            }
            SK::Expr(expr) => { self.check_expr(expr)?; }
            SK::Return(value) => {
                if let Some(value) = value {
                    let typ = self.check_expr(value);

                    // TODO: check type with return type of function
                } else {
                    // TODO: make sure the function has no return type as well
                }
            }
        }

        Ok(())
    }

    pub fn check_expr(&mut self, expr: &ast::Expr) -> Result<ast::Type, CheckerError> {
        Ok(ast::Type {
            kind: TypeKind::Integer(Width::DoubleWord, true),
            loc: Location::default(),
        })
    }

    // TODO: finish this function
    pub fn is_type_compatible(&self, first: &ast::Type, second: &ast::Type) -> bool { true }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }
}