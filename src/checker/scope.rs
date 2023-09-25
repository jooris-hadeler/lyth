use std::collections::HashMap;
use crate::parser::ast::TypeKind;

#[derive(Debug, Clone, Default)]
pub struct ScopeStack {
    scopes: Vec<Scope>,
}

impl ScopeStack {
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn current(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }


    pub fn lookup(&self, name: &Box<str>) -> Option<&Option<TypeKind>> {
        for scope in self.scopes.iter().rev() {
            let result = scope.lookup(name);
            if result.is_some() {
                return result;
            }
        }

        None
    }
}

#[derive(Debug, Clone, Default)]
pub struct Scope {
    variables: HashMap<Box<str>, Option<TypeKind>>,
}

impl Scope {
    pub fn lookup(&self, name: &Box<str>) -> Option<&Option<TypeKind>> {
        self.variables.get(name)
    }

    pub fn add_variable(&mut self, name: Box<str>, typ: Option<TypeKind>) {
        self.variables.insert(name, typ);
    }

    pub fn set_variable_typ(&mut self, name: Box<str>, typ: TypeKind) {
        self.variables.insert(name, Some(typ));
    }
}