use crate::scan::token::Token;
use std::fmt;
use Statement::*;

#[derive(Debug)]
pub struct Ast {
    pub statements: Vec<Statement>,
}

impl Ast {
    pub fn new(statements: Vec<Statement>) -> Self {
        Ast { statements }
    }
    pub fn push_statement(&mut self, val: Statement) {
        self.statements.push(val);
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    OperState(OperationDeclaration),
    OperCallState(OperCall),

    VarState(Variable),

    IfState(IfStatement),
    RetState(RetStatement),

    BinaryState(Expression),
    AssignmentState(AssignmentExpr),
    IdentState(Identifier),

    Val(VarValue),
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Identifier,
    pub val: Box<Statement>,
}

impl Variable {
    pub fn new(name: Identifier, val: Statement) -> Self {
        Variable {
            name,
            val: Box::new(val),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub val: String,
}

impl Identifier {
    pub fn new(token: Token) -> Self {
        Identifier {
            val: token.value,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AssignmentExpr {
    pub assigne: Identifier,
    pub value: Box<Statement>,
}

impl AssignmentExpr {
    pub fn new(assigne: Identifier, value: Box<Statement>) -> Self {
        AssignmentExpr { assigne, value }
    }
}

// EXPRESSION

#[derive(Debug, Clone)]
pub struct Expression {
    pub e_type: ExpressionType,
}

impl Expression {
    pub fn new(e_type: ExpressionType) -> Self {
        Expression { e_type }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionType {
    IntNumber(i64),
    FloatNumber(f64),
    //Bool(bool),
    BinaryExpression {
        left: Box<Statement>,
        operator: String,
        right: Box<Statement>,
    },
}

// EXPRESSION

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub test_part: Option<Box<Expression>>, // one == 1 and similar

    pub consequent: Vec<Statement>, // part which will be executed if test_part is true
    pub alternite: Option<Box<IfStatement>>,
}

impl IfStatement {
    pub fn new(
        test_part: Option<Box<Expression>>,
        consequent: Vec<Statement>,
        alternite: Option<Box<IfStatement>>,
    ) -> Self {
        IfStatement {
            test_part,
            consequent,
            alternite,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Num,
    RawStr,
    Bool,
    Ide,
}

#[derive(Debug, Clone)]
pub struct RetStatement {
    pub ret_value: Option<Box<Statement>>,
    pub ret_type: Option<Type>,
}

impl RetStatement {
    pub fn new(ret_value: Option<Box<Statement>>) -> Self {
        RetStatement {
            ret_value: ret_value.clone(),
            ret_type: if let Some(r_v) = ret_value {
                match *r_v {
                    BinaryState(_) => Some(Type::Num),
                    IdentState(_) => Some(Type::Ide),
                    _ => None,
                }
            } else {
                None
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum VarValue {
    ExprVal(Expression),
    StringVal(String),
}

// OPER IMPLEMENTATION

#[derive(Debug, Clone)]
pub struct OperationDeclaration {
    pub name: String,
    pub oper_args: Option<Vec<Identifier>>,
    pub body: Option<Vec<Statement>>,
}

impl OperationDeclaration {
    pub fn new(
        name: String,
        oper_args: Option<Vec<Identifier>>,
        body: Option<Vec<Statement>>,
    ) -> Self {
        OperationDeclaration {
            name,
            oper_args,
            body,
        }
    }
}

#[derive(Debug, Clone)]
pub struct OperCall {
    pub oper_name: String,
    pub o_args: Option<Vec<Statement>>,
    pub native: bool,
}

impl OperCall {
    pub fn new(oper_name: String, o_args: Option<Vec<Statement>>, native: bool) -> Self {
        OperCall {
            oper_name,
            o_args,
            native,
        }
    }
}

// DISPLAY IMPLEMENTATION

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Ast {{\n")?;
        for statement in &self.statements {
            writeln!(f, "  {}\n", statement)?;
        }
        writeln!(f, "}}")
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Var name: {} value: {:#?}", self.name, self.val)?;
        Ok(())
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.val)?;
        Ok(())
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.e_type {
            ExpressionType::IntNumber(v) => writeln!(f, "({}", v),
            ExpressionType::FloatNumber(v) => writeln!(f, "({}", v),
            _ => writeln!(f, "Nothing"),
        }
    }
}

impl fmt::Display for RetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Ret value: {:#?}", self.ret_value)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::OperState(oper_statement) => {
                writeln!(f, "Oper Statement {:#?}", oper_statement)
            }
            Statement::OperCallState(oper_statement) => {
                writeln!(f, "Oper Call Statement {:#?}", oper_statement)
            }
            Statement::VarState(var_statement) => writeln!(f, "Var Statement {}", var_statement),
            Statement::IfState(if_statement) => writeln!(f, "IfState {:#?}", if_statement),
            Statement::RetState(ret_statement) => writeln!(f, "RetState {}", ret_statement),
            Statement::IdentState(ident) => writeln!(f, "Ident {}", ident),
            Statement::BinaryState(bin) => writeln!(f, "Binary {}", bin),
            Statement::AssignmentState(bin) => writeln!(f, "ass satte {:#?}", bin),
            Statement::Val(v) => writeln!(f, "Val {:#?}", v),
        }
    }
}
