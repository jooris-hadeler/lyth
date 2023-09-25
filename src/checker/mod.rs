use std::cmp::max;
use std::collections::HashMap;
use thiserror::Error;

use crate::parser::ast::*;

mod scope;

use crate::checker::scope::ScopeStack;
use crate::lexer::Location;

#[derive(Debug, Clone, Error)]
pub enum CheckerError {
    #[error("{0}:{1}-{2}: no variable or function of name `{3}` found", loc.file, loc.start, loc.end, name)]
    Lookup {
        loc: Location,
        name: Box<str>,
    },
    #[error("{0}:{1}-{2}: type mismatch: expected {3:?} but {4:?} was found", loc.file, loc.start, loc.end, first, second)]
    TypeMismatch {
        loc: Location,
        first: TypeKind,
        second: TypeKind,
    },
    #[error("{0}:{1}-{2}: incompatible types: {3:?} is not compatible with {4:?}", loc.file, loc.start, loc.end, first, second)]
    Incompatible {
        loc: Location,
        first: TypeKind,
        second: TypeKind,
    },
    #[error("{0}")]
    Custom(&'static str),

    #[error("{0}:{1}-{2}: {3}", loc.file, loc.start, loc.end, msg)]
    LocatedCustom {
        loc: Location,
        msg: &'static str,
    },
}


#[derive(Debug, Clone)]
pub struct Checker<'a> {
    module: &'a Module,
    function_signatures: HashMap<Box<str>, (Vec<TypeKind>, Option<TypeKind>)>,
    scope_stack: ScopeStack,
}

const U8_MAX: usize = u8::MAX as usize;
const U16_MAX: usize = u16::MAX as usize;
const U32_MAX: usize = u32::MAX as usize;
const U64_MAX: usize = u64::MAX as usize;

impl<'a> Checker<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self { module, function_signatures: HashMap::new(), scope_stack: ScopeStack::default() }
    }

    pub fn check_module(&mut self) -> Result<(), CheckerError> {
        self.populate_function_signatures();

        // check all functions
        self.module.functions.iter().try_for_each(|f| self.check_function(f))?;

        Ok(())
    }

    pub fn populate_function_signatures(&mut self) {
        for func in self.module.functions.iter() {
            let param_types = func.params
                .iter()
                .map(|p| p.typ.kind.clone())
                .collect::<Vec<TypeKind>>();

            self.function_signatures.insert(
                func.name.clone(),
                (param_types, func.return_type.as_ref().map(|t| t.kind.clone())),
            );
        }
    }

    pub fn check_function(&mut self, func: &FuncDef) -> Result<(), CheckerError> {
        if let Some(body) = &func.body {
            self.check_function_body(func, body)?;
        }

        Ok(())
    }

    pub fn check_function_body(&mut self, func: &FuncDef, body: &Block) -> Result<(), CheckerError> {
        self.scope_stack.push_scope();

        for param in func.params.iter() {
            self.scope_stack.current_mut().add_variable(param.name.clone(), Some(param.typ.kind.clone()));
        }

        body.0.iter().try_for_each(|s| self.check_stmt(func, s))?;

        self.scope_stack.pop_scope();

        Ok(())
    }

    pub fn check_stmt(&mut self, func: &FuncDef, stmt: &Stmt) -> Result<(), CheckerError> {
        use StmtKind as SK;

        match &stmt.kind {
            SK::Let(name, typ, value) => {
                let mut let_typ = typ.clone().map(|t| t.kind);

                // TODO: think about type equality and compatibility
                // if types are unequal or incompatible
                match typ {
                    Some(typ) => match value {
                        Some(value) => {
                            let res = self.check_expr(value)?;

                            if !self.is_type_compatible(&typ.kind, &res) {
                                return Err(CheckerError::TypeMismatch {
                                    loc: stmt.loc.clone(),
                                    first: typ.kind.clone(),
                                    second: res.clone(),
                                });
                            }
                        }
                        None => {}
                    },
                    None => match value {
                        Some(value) => {
                            let_typ = Some(self.check_expr(value)?);
                        }
                        None => {}
                    }
                }

                self.scope_stack.current_mut().add_variable(name.clone(), let_typ);
            }
            SK::Expr(expr) => { self.check_expr(expr)?; }
            SK::Return(value) => {
                if let Some(value) = value {
                    let typ = self.check_expr(value)?;

                    match &func.return_type {
                        Some(return_typ) => {
                            if !self.is_type_compatible(&return_typ.kind, &typ) {
                                return Err(CheckerError::TypeMismatch {
                                    loc: stmt.loc.clone(),
                                    first: return_typ.kind.clone(),
                                    second: typ.clone(),
                                });
                            }
                        }
                        None => return Err(CheckerError::LocatedCustom {
                            loc: stmt.loc.clone(),
                            msg: "found return value but none was expected",
                        })
                    }
                } else {
                    match &func.return_type {
                        Some(..) => return Err(CheckerError::LocatedCustom {
                            loc: stmt.loc.clone(),
                            msg: "expected return value but none was found",
                        }),
                        None => {}
                    }
                }
            }
        }

        Ok(())
    }

    pub fn check_expr(&mut self, expr: &Expr) -> Result<TypeKind, CheckerError> {
        match &expr.kind {
            ExprKind::Identifier(name) => self.check_identifier_expr(name, &expr.loc),
            ExprKind::Unary(op, expr) => self.check_unary_expr(op, expr.as_ref()),
            ExprKind::Binary(op, left, right) => self.check_binary_expr(op, left.as_ref(), right.as_ref()),
            ExprKind::Call(func, params) => self.check_call_expr(func.as_ref(), params),
            ExprKind::Boolean(..) => Ok(TypeKind::Boolean),
            ExprKind::Integer(val) => {
                let size = match *val {
                    0..=U8_MAX => Width::Byte,
                    0..=U16_MAX => Width::Word,
                    0..=U32_MAX => Width::DoubleWord,
                    0..=U64_MAX => Width::QuadWord,
                    _ => return Err(CheckerError::LocatedCustom {
                        loc: expr.loc.clone(),
                        msg: "integer type could not be identified",
                    })
                };

                Ok(TypeKind::Integer(size, false))
            }
        }
    }

    pub fn check_identifier_expr(&mut self, name: &Box<str>, loc: &Location) -> Result<TypeKind, CheckerError> {
        // Lookup identifier and its type
        // TODO: check higher scopes if not found here
        let typ = self.scope_stack.lookup(name);

        match typ {
            Some(typ) => match typ {
                Some(typ) => return Ok(typ.clone()),
                None => return Err(CheckerError::LocatedCustom {
                    loc: loc.clone(),
                    msg: "unknown type of identifier",
                }),
            }
            None => {}
        };

        // Check function signatures
        let func = self.function_signatures.get(name);

        match func {
            Some(func) => Ok(TypeKind::Function(
                func.0.clone(),
                func.1.clone().map(|t| Box::new(t)),
            )),
            None => Err(CheckerError::Lookup {
                loc: loc.clone(),
                name: name.clone(),
            })
        }
    }

    pub fn check_unary_expr(&mut self, _op: &UnaryOp, expr: &Expr) -> Result<TypeKind, CheckerError> {
        // TODO: maybe types change not sure yet
        self.check_expr(expr)
    }

    pub fn check_binary_expr(&mut self, _op: &BinaryOp, left: &Expr, right: &Expr) -> Result<TypeKind, CheckerError> {
        let mut loc = left.loc.clone();
        loc.end = right.loc.end;

        let left = self.check_expr(left)?;
        let right = self.check_expr(right)?;

        self.get_result_type(&left, &right, &loc)
    }

    pub fn check_call_expr(&mut self, func: &Expr, params: &Vec<Expr>) -> Result<TypeKind, CheckerError> {
        let typ = self.check_expr(func)?;

        if let TypeKind::Function(param_types, return_type) = typ {
            for (expr, typ) in params.iter().zip(param_types.iter()) {
                let result = self.check_expr(expr)?;

                if result != *typ {
                    return Err(CheckerError::TypeMismatch {
                        loc: expr.loc.clone(),
                        first: typ.clone(),
                        second: result.clone(),
                    });
                }
            }

            let kind = return_type.map_or(TypeKind::Void, |t| *t);
            Ok(kind)
        } else {
            Err(CheckerError::LocatedCustom {
                loc: func.loc.clone(),
                msg: "cannot call a non function",
            })
        }
    }

    // TODO: finish this function
    pub fn is_type_compatible(&self, first: &TypeKind, second: &TypeKind) -> bool {
        match first {
            TypeKind::Integer(width_first, signed_first) => match second {
                TypeKind::Integer(width_second, signed_second) => {
                    !(width_first < width_second
                        || *signed_first && !*signed_second
                        || !*signed_first && *signed_second)
                }

                _ => false,
            }
            TypeKind::Boolean => matches!(second, TypeKind::Boolean),

            _ => false,
        }
    }

    pub fn get_result_type(&self, first: &TypeKind, second: &TypeKind, loc: &Location) -> Result<TypeKind, CheckerError> {
        match first {
            TypeKind::Integer(width_first, signed_first) => match second {
                TypeKind::Integer(width_second, signed_second) => {
                    let width = max(width_first, width_second);
                    let signed = *signed_first || *signed_second;

                    Ok(TypeKind::Integer(width.clone(), signed))
                }

                _ => Err(CheckerError::Incompatible {
                    loc: loc.clone(),
                    first: first.clone(),
                    second: second.clone(),
                })
            }
            TypeKind::Boolean => match second {
                TypeKind::Boolean => Ok(TypeKind::Boolean),

                _ => Err(CheckerError::Incompatible {
                    loc: loc.clone(),
                    first: first.clone(),
                    second: second.clone(),
                })
            }
            TypeKind::Identifier(name_first) => match second {
                TypeKind::Identifier(name_second) => if name_first == name_second {
                    Ok(TypeKind::Identifier(name_second.clone()))
                } else {
                    Err(CheckerError::Incompatible {
                        loc: loc.clone(),
                        first: first.clone(),
                        second: second.clone(),
                    })
                },

                _ => Err(CheckerError::Incompatible {
                    loc: loc.clone(),
                    first: first.clone(),
                    second: second.clone(),
                })
            }
            TypeKind::Ref(inner_first) => match second {
                TypeKind::Ref(inner_second) => {
                    let result = self.get_result_type(inner_first.as_ref(), inner_second.as_ref(), loc)?;
                    Ok(TypeKind::Ref(Box::new(result)))
                }

                _ => Err(CheckerError::Incompatible {
                    loc: loc.clone(),
                    first: first.clone(),
                    second: second.clone(),
                })
            }
            TypeKind::Function(..) => Err(CheckerError::LocatedCustom {
                loc: loc.clone(),
                msg: "function types shouldn't be used in mathematical expressions",
            }),
            TypeKind::Void => Err(CheckerError::LocatedCustom {
                loc: loc.clone(),
                msg: "!! this shouldn't happen !!",
            })
        }
    }
}