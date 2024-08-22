use std::collections::HashMap;
//use backtrace::Backtrace;

use crate::engine::runtime::RuntimeVal;

#[derive(Debug, Clone)]
pub struct Environment {
    parent: Option<Box<Environment>>,
    vars: HashMap<String, RuntimeVal>,
    opers: HashMap<String, RuntimeVal>,
}

impl Environment {
    pub fn new(parent: Option<Box<Environment>>) -> Self {
        Environment {
            parent,
            vars: HashMap::new(),
            opers: HashMap::new(),
        }
    }

    pub fn declare_var(
        &mut self,
        var_name: String,
        value: RuntimeVal,
    ) -> Result<RuntimeVal, String> {
        if self.vars.contains_key(&var_name) {
            return Err(format!("Variable {} is already decleared", var_name));
        }

        self.vars.insert(var_name, value.clone());
        Ok(value)
    }

    pub fn assign_var(
        &mut self,
        var_name: String,
        value: RuntimeVal,
    ) -> Result<RuntimeVal, String> {
        let env = self.resolve_var(&var_name).unwrap();
        env.vars.insert(var_name.clone(), value);
        Ok(env.lookup_var(var_name))
    }

    pub fn declare_oper(
        &mut self,
        oper_name: String,
        oper_body: RuntimeVal,
    ) -> Result<RuntimeVal, String> {
        if self.opers.contains_key(&oper_name) {
            return Err(format!("Operation {} is already decleared", oper_name));
        }
        self.opers.insert(oper_name, oper_body.clone());
        Ok(oper_body)
    }

    pub fn lookup_oper(&mut self, oper_name: String) -> RuntimeVal {
        let env = self.resolve_oper(&oper_name).unwrap();
        env.opers.get(&oper_name).unwrap().clone()
    }

    pub fn lookup_var(&mut self, var_name: String) -> RuntimeVal {

        let env = self.resolve_var(&var_name).unwrap();
        env.vars.get(&var_name).unwrap().clone()
    }

    pub fn resolve_var(&mut self, var_name: &str) -> Result<&mut Self, String> {
        //println!("{:#?}", self);
        if self.vars.contains_key(var_name) {
            return Ok(self);
        } else if self.parent.is_none() {
            return Err(format!("Cannot resolve variable w: {}", var_name));
        }

        if let Some(parent) = self.parent.as_mut() {
            parent.resolve_var(var_name)
        } else {
            Err(format!("Cannot resolve variable: {}", var_name))
        }
    }

    pub fn resolve_oper(&mut self, oper_name: &str) -> Result<&mut Self, String> {
        if self.opers.contains_key(oper_name) {
            return Ok(self);
        } else if self.parent.is_none() {
            return Err(format!("Cannot resolve operation: {}", oper_name));
        }

        if let Some(parent) = self.parent.as_mut() {
            parent.resolve_oper(oper_name)
        } else {
            Err(format!("Cannot resolve variable: {}", oper_name))
        }
    }
}
