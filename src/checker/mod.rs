use std::cmp::max;
use std::collections::HashMap;
use crate::parser::ast as ast;

use thiserror::Error;
use crate::lexer::Location;
use crate::parser::ast::{BinaryOp, Expr, ExprKind, TypeKind, UnaryOp, Width};

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

const U8_MAX: usize = u8::MAX as usize;
const U16_MAX: usize = u16::MAX as usize;
const U32_MAX: usize = u32::MAX as usize;
const U64_MAX: usize = u64::MAX as usize;

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
        match &expr.kind {
            ExprKind::Identifier(name) => self.check_identifier_expr(name),
            ExprKind::Unary(op, expr) => self.check_unary_expr(op, expr.as_ref()),
            ExprKind::Binary(op, left, right) => self.check_binary_expr(op, left.as_ref(), right.as_ref()),
            ExprKind::Call(func, params) => self.check_call_expr(func.as_ref(), params),
            ExprKind::Boolean(..) => Ok(ast::Type { kind: TypeKind::Boolean, loc: Location::default() }),
            ExprKind::Integer(val) => {
                let size = match *val {
                    0..=U8_MAX => Width::Byte,
                    0..=U16_MAX => Width::Word,
                    0..=U32_MAX => Width::DoubleWord,
                    0..=U64_MAX => Width::QuadWord,
                };

                Ok(ast::Type {
                    kind: TypeKind::Integer(size, false),
                    loc: Location::default(),
                })
            }
        }
    }

    pub fn check_identifier_expr(&mut self, name: &Box<str>) -> Result<ast::Type, CheckerError> {
        // Lookup identifier and its type
        // TODO: check higher scopes if not found here
        let typ = self.scopes.last().unwrap().variables.get(name);

        match typ {
            Some(typ) => match typ {
                Some(typ) => return Ok(typ.clone()),
                None => unimplemented!("unknown type of identifier"),
            }
            None => {}
        };

        // Check function signatures
        let func = self.function_signatures.get(name);

        match func {
            Some(func) => Ok(ast::Type {
                kind: TypeKind::Function(func.0.clone(), func.1.clone()),
                loc: Location::default(),
            }),
            None => unimplemented!("no variable or function of name found")
        }
    }

    pub fn check_unary_expr(&mut self, op: &UnaryOp, expr: &Expr) -> Result<ast::Type, CheckerError> {
        todo!()
    }

    pub fn check_binary_expr(&mut self, op: &BinaryOp, left: &Expr, right: &Expr) -> Result<ast::Type, CheckerError> {
        todo!()
    }

    pub fn check_call_expr(&mut self, func: &Expr, params: &Vec<Expr>) -> Result<ast::Type, CheckerError> {
        let typ = self.check_expr(func)?;

        if let TypeKind::Function(param_types, return_type) = typ {
            for (expr, typ) in params.iter().zip(param_types.iter()) {
                let result = self.check_expr(expr)?;

                if result.kind != typ.kind {
                    unimplemented!("types incompatible")
                }
            }

            let kind = return_type.map_or(TypeKind::Void, |t| t.kind);
            Ok(ast::Type {
                kind,
                loc: Location::default(),
            })
        } else {
            unimplemented!("error not a function cant call")
        }
    }

    // TODO: finish this function
    pub fn is_type_compatible(&self, first: &ast::Type, second: &ast::Type) -> bool {
        matches!(first.kind, TypeKind::Integer(..)) && matches!(second.kind, TypeKind::Integer(..))
            || matches!(first.kind, TypeKind::Boolean) && matches!(second.kind, TypeKind::Boolean)
    }

    pub fn get_result_type(&self, first: &ast::Type, second: &ast::Type) -> Result<ast::Type, CheckerError> {
        let kind = self.get_result_type_kind(&first.kind, &second.kind)?;

        Ok(ast::Type { kind, loc: Location::default() })
    }

    pub fn get_result_type_kind(&self, first: &TypeKind, second: &TypeKind) -> Result<TypeKind, CheckerError> {
        match first {
            TypeKind::Integer(width_first, signed_first) => match second {
                TypeKind::Integer(width_second, signed_second) => {
                    let width = max(width_first, width_second);
                    let signed = signed_first || signed_second;

                    Ok(TypeKind::Integer(width.clone(), signed))
                }

                _ => unimplemented!("incompatible types")
            }
            TypeKind::Boolean => match second {
                TypeKind::Boolean => Ok(TypeKind::Boolean),

                _ => unimplemented!("incompatible types")
            }
            TypeKind::Identifier(name_first) => match second {
                TypeKind::Identifier(name_second) => Ok(TypeKind::Identifier(name_second.clone())),

                _ => unimplemented!("incompatible types")
            }
            TypeKind::Ref(inner_first) => match second {
                TypeKind::Ref(inner_second) => {
                    let result = self.get_result_type_kind(inner_first.as_ref(), inner_second.as_ref())?;
                    Ok(TypeKind::Ref(Box::new(result)))
                }

                _ => unimplemented!("incompatible types")
            }
            TypeKind::Function(..) => unimplemented!("shouldn't be used in mathematical expressions")
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }
}