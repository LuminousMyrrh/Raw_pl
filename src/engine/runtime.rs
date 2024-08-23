use core::fmt;

use crate::lexing::ast::*;
use crate::engine::environment::*;
use RuntimeValType::*;

#[derive(Debug, Clone)]
pub struct RuntimeVal {
    pub rt_type: RuntimeValType,
}

impl RuntimeVal {
    pub fn new(rt_type: RuntimeValType) -> Self {
        RuntimeVal { rt_type }
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeValType {
    IntRT(i64),
    FloatRT(f64),
    Stri(String),
    OperationRT {
        name: String,
        args: Option<Vec<Identifier>>,
        body: Vec<Statement>,
        decl_env: Environment,
    },
    ReturnRT(Box<RuntimeVal>, Option<Type>),
    Bool(bool),
}

pub fn eval(stmt_node: Statement, env: &mut Environment) -> Option<RuntimeVal> {
    //println!("evaling node: {:#?}", stmt_node);
    match stmt_node {
        Statement::OperState(s) => Some(eval_operation_declaration(s, env)),
        Statement::OperCallState(s) => Some(eval_oper_call(s, env).unwrap()),
        Statement::VarState(var) => Some(eval_var(var, env)),
        Statement::IfState(ifstmt) => Some(eval_if_stmt(ifstmt, env).unwrap()),
        Statement::BinaryState(bin) => Some(eval_binary(bin, env).unwrap()),
        Statement::Val(v) => match v {
            //
            VarValue::ExprVal(expr) => match expr.e_type {
                ExpressionType::IntNumber(i) => Some(RuntimeVal::new(IntRT(i))),
                ExpressionType::FloatNumber(f) => Some(RuntimeVal::new(FloatRT(f))),
                ExpressionType::BinaryExpression { .. } => Some(eval_binary(expr, env).unwrap()),
                _ => todo!(), // Bool must be covered
            },
            VarValue::StringVal(val) => Some(RuntimeVal::new(Stri(val))),
        },
        Statement::IdentState(id) => match env.lookup_var(id.val).rt_type {
            IntRT(v) => Some(RuntimeVal::new(IntRT(v))),
            FloatRT(v) => Some(RuntimeVal::new(FloatRT(v))),
            Stri(s) => Some(RuntimeVal::new(Stri(s))),
            _ => {
                println!("no way bro");
                None
            }
        },
        Statement::AssignmentState(s) => Some(eval_assignment_expr(s, env)),
        Statement::RetState(r) => Some(eval_return(r, env)),
    }
}

pub fn eval_binary(stmt_node: Expression, env: &mut Environment) -> Result<RuntimeVal, String> {
    match stmt_node.e_type {
        ExpressionType::BinaryExpression {
            left,
            operator,
            right,
        } => {
            let lhs = eval(*left, env).unwrap();
            let rhs = eval(*right, env).unwrap();

            match (lhs.rt_type, rhs.rt_type) {
                (IntRT(l), IntRT(r)) => {
                    Ok(eval_int_numeric_binary(l, r, operator).unwrap())
                }
                (FloatRT(l), FloatRT(r)) => {
                    Ok(eval_float_numeric_binary(l, r, operator).unwrap())
                }
                _ => Err("Invalid operand types for binary expression".to_string()),
            }
        }
        _ => Err("Invalid expression type".to_string()),
    }
}

fn eval_int_numeric_binary(lhs: i64, rhs: i64, operator: String) -> Result<RuntimeVal, String> {
    let res: i64 = match operator.as_str() {
        "+" => lhs + rhs,
        "-" => lhs - rhs,
        "*" => lhs * rhs,
        "/" => {
            if rhs == 0 || lhs == 0 {
                return Err("Cannot divide zero".to_string());
            }

            lhs / rhs
        }
        _ => return Err(format!("Unknown operator: {}", operator)),
    };

    Ok(RuntimeVal::new(RuntimeValType::IntRT(res)))
}

fn eval_float_numeric_binary(lhs: f64, rhs: f64, operator: String) -> Result<RuntimeVal, String> {
    let res: f64 = match operator.as_str() {
        "+" => lhs + rhs,
        "-" => lhs - rhs,
        "*" => lhs * rhs,
        "/" => {
            if rhs == 0.0 || lhs == 0.0 {
                return Err("Cannot divide zero".to_string());
            }
            lhs / rhs
        }
        _ => return Err(format!("Unknown operator: {}", operator)),
    };

    Ok(RuntimeVal::new(RuntimeValType::FloatRT(res)))
}

pub fn eval_program(prog: Ast, env: &mut Environment) -> Result<(), String> {
    let mut last_evaled: Option<RuntimeVal> = None;

    for stmt in prog.statements {
        last_evaled = eval(stmt, env);
        //println!("{:#?}", last_evaled.clone().unwrap());
    }

    if last_evaled.is_some() {
        Ok(())
    } else {
        Err(format!("Cannot evaluate statement: {:#?}", last_evaled))
    }
}

fn eval_return(stmt_node: RetStatement, env: &mut Environment) -> RuntimeVal {
    RuntimeVal::new(RuntimeValType::ReturnRT(
        Box::new(eval(*stmt_node.ret_value.unwrap(), env).unwrap()),
        stmt_node.ret_type,
    ))
}

fn eval_assignment_expr(stmt_node: AssignmentExpr, env: &mut Environment) -> RuntimeVal {
    let val = eval(*stmt_node.value, env).unwrap();
    env.assign_var(stmt_node.assigne.val, val).unwrap()
}

fn eval_var(stmt_node: Variable, env: &mut Environment) -> RuntimeVal {
    let val = eval(*stmt_node.val, env);
    env.declare_var(stmt_node.name.val, val.unwrap()).unwrap()
}

fn eval_operation_declaration(
    operation_dec: OperationDeclaration,
    env: &mut Environment,
) -> RuntimeVal {
    let oper = OperationRT {
        name: operation_dec.name.clone(),
        args: operation_dec.oper_args,
        body: operation_dec.body.unwrap(),
        //ret: Some(*operation_dec.ret.unwrap()),
        decl_env: env.clone(), // <- need to fix that sheet
    };
    env.declare_oper(operation_dec.name, RuntimeVal::new(oper))
        .unwrap()
}

fn eval_if_stmt(stmt_node: IfStatement, env: &mut Environment) -> Result<RuntimeVal, String> {
    if let Some(tp) = stmt_node.test_part {
        match tp.e_type {
            ExpressionType::BinaryExpression {
                left,
                operator,
                right } => {
                let result = eval_logical_expr(*left, *right, operator, env)?;
                match result.rt_type {
                    RuntimeValType::Bool(value) => {
                        if value {
                            let mut last_evaled: Option<RuntimeVal> = None;
                            for s in stmt_node.consequent.iter() {
                                last_evaled = eval(s.to_owned(), env);
                            }
                            Ok(last_evaled.unwrap())
                        } else {
                            // Execute the alternite part
                            if let Some(alt) = stmt_node.alternite {
                                eval_if_stmt(*alt, env)
                            } else {
                                Err(format!("Runtime error while evaluating alternite part: {:#?}", stmt_node.alternite))
                            }
                        }
                    },
                    _ => Err("wtf".to_string())
                }
            }
            _ => Err("Runtime error: Test part must be Binary expression!".to_string())
        }
    } else {
        let mut last_evaled: Option<RuntimeVal> = None;
        for s in stmt_node.consequent.iter() {
            last_evaled = eval(s.to_owned(), env);
        }
        Ok(last_evaled.unwrap())
    }
}

fn eval_logical_expr(left: Statement, right: Statement, operator: String, env: &mut Environment) -> Result<RuntimeVal, String> {
    let l = eval(left, env).unwrap();
    let r = eval(right, env).unwrap();

    match (l.rt_type, r.rt_type) {
        (IntRT(lhs), IntRT(rhs)) => {
            match operator.as_str() {
                "==" => Ok(RuntimeVal::new(RuntimeValType::Bool(lhs == rhs))),
                "!=" => Ok(RuntimeVal::new(RuntimeValType::Bool(lhs != rhs))),
                "<" => Ok(RuntimeVal::new(RuntimeValType::Bool(lhs < rhs))),
                ">" => Ok(RuntimeVal::new(RuntimeValType::Bool(lhs > rhs))),
                _ => Err("Invalid operator!".to_string()),
            }
        }
        _ => Err("Invalid operand types for binary expression".to_string()),
    }
}

fn eval_oper_call(oper: OperCall, env: &mut Environment) -> Result<RuntimeVal, String> {
    let call_args = oper.clone().o_args.unwrap_or_default(); // Handle the Option type
    let rt_call_args: Vec<Option<RuntimeVal>> =
        call_args.into_iter().map(|a| eval(a, env)).collect();

    if !oper.clone().native {
        let func = env.lookup_oper(oper.clone().oper_name);
        let func_args = match func.clone().rt_type {
            OperationRT { args, .. } => args,
            _ => return Err("Error: Unexpected function type".to_string()),
        };

        let mut scope = Environment::new(match func.clone().rt_type {
            OperationRT { decl_env, .. } => Some(Box::new(decl_env)),
            _ => {
                return Err(format!(
                    "Error: Unknown function type: {:#?}",
                    oper.oper_name
                ))
            }
        });

        // Code to declare variables in the scope
        if !rt_call_args.is_empty() {
            if let Some(fc) = func_args.clone() {
                if !fc.is_empty() {
                    for (i, arg) in fc.iter().enumerate() {
                        scope
                            .declare_var(arg.val.clone(), rt_call_args[i].clone().unwrap())
                            .unwrap();
                    }
                }
            }
        }

        let func_body = match func.rt_type {
            OperationRT { body, .. } => body,
            _ => return Err("No body? Idk how this happen".to_string()),
        };

        let mut res: Option<RuntimeVal> = None; // Initialize the result

        if !func_body.is_empty() {
            for s in func_body.iter() {
                match s {
                    Statement::RetState(_) => {
                        res = Some(eval(s.to_owned(), &mut scope).unwrap());
                        return Ok(res.unwrap());
                    }
                    _ => res = Some(eval(s.to_owned(), &mut scope).unwrap()),
                }
            }
        };
        return Ok(if let Some(r) = res {
            r
        } else {
            return Err("Okey this is shit".to_string());
        });
    }

    match oper.clone().oper_name.as_str() {
        "print" => {
            if !rt_call_args.is_empty() {
                for arg in rt_call_args.into_iter().flatten() {
                    print!("{}", arg);
                }
            } else {
                println!("empty");
            }
            Ok(RuntimeVal::new(RuntimeValType::Stri("".to_string())))
        }
        "println" => {
            if !rt_call_args.is_empty() {
                for arg in rt_call_args.into_iter().flatten() {
                    println!("{}", arg);
                }
            }
            Ok(RuntimeVal::new(RuntimeValType::Stri("".to_string())))
        }
        _ => Ok(RuntimeVal::new(RuntimeValType::Stri("".to_string()))),
    }
}

impl fmt::Display for RuntimeVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.rt_type {
            RuntimeValType::FloatRT(num) => write!(f, "{}", num),
            RuntimeValType::IntRT(num) => write!(f, "{}", num),
            RuntimeValType::Stri(s) => write!(f, "{}", s),
            RuntimeValType::ReturnRT(rv, _) => write!(f, "{}", rv),
            RuntimeValType::OperationRT { .. } => write!(f, "Cannot output Operation!"),
            RuntimeValType::Bool ( b ) => write!(f, "{}", b),
        }
    }
}
