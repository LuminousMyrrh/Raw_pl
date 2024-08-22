use crate::lexing::ast::Statement::IfState;
use crate::lexing::ast::Statement::Val;
use crate::lexing::ast::Statement::VarState;
use crate::lexing::ast::{Statement, *};

use crate::scan::token::{Token, TokenType, TokenType::*};

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    line: usize,
    c_token: Token,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.clone(),
            index: 0,
            line: 0,
            c_token: tokens[0].clone(),
        }
    }

    fn next_token(&self) -> Result<Token, String> {
        match self.tokens.get(self.index + 1) {
            Some(token) => Ok(token.clone()),
            None => Err("Out of range".to_string()),
        }
    }

    pub fn parse(&mut self) -> Result<AST, String> {
        let mut prog: AST = AST::new(Vec::new());

        loop {
            if self.c_token.t_type == Semicolon {
                self.advance()?;
                self.line += 1;
                continue;
            } else if self.c_token.t_type == Eof {
                break;
            }

            prog.push_statement(self.parse_statement().unwrap());
        }

        println!("prog: {:#?}", prog);
        Ok(prog)
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.c_token.t_type {
            TokenType::Var => Ok(VarState(self.parse_var()?)),
            TokenType::If => Ok(IfState(self.parse_if()?)),
            TokenType::Repay => Ok(self.parse_return()?),
            TokenType::IntNum | TokenType::FloatNum => Ok(*self.parse_expr()?),
            TokenType::Ident => Ok(self.parse_ident_statement().unwrap().unwrap()),
            TokenType::RawStr => Ok(Val(self.parse_string())),
            TokenType::Oper => Ok(*self.parse_operation()?.unwrap()),
            TokenType::True => todo!(),
            TokenType::False => todo!(),
            _ => {
                Err(self.report_error().err().unwrap())
            }
        }
    }

    fn report_error(&self) -> Result<(), String> {
        let mut temp_tokens: Vec<Token> = Vec::new();
        let mut idx = self.index;
        let mut temp_token = self.c_token.clone();

        while (temp_token.t_type != LBrace || temp_token.t_type != RBrace) && idx != 0 {
            temp_tokens.push(temp_token.clone());
            idx -= 1;
            temp_token = self.tokens[idx].clone();
        }

        let mut out = temp_tokens
            .iter()
            .map(|t| {
                if t == &self.c_token {
                    format!(">{}<", t.value) // Surround with > and < if equal
                } else {
                    t.value.clone() // Otherwise, just clone the value
                }
            })
            .collect::<Vec<String>>();
        out.reverse();
        let out = out.join(" ");
        Err(out)
    }

    // Helper method to handle Ident statements
    fn parse_ident_statement(&mut self) -> Result<Option<Statement>, String> {
        let next_token = self.next_token()?;

        match next_token.t_type {
            TokenType::LParen => Ok(Some(self.parse_oper_call()?)),
            TokenType::Plus | TokenType::Minus | TokenType::Slash | TokenType::Star => {
                Ok(Some(*self.parse_expr()?))
            }
            TokenType::Equal => Ok(Some(self.parse_assignment_expr()?)),
            _ => {
                self.c_token = next_token;
                Ok(Some(Statement::IdentState(self.parse_id())))
            }
        }
    }

    fn parse_var(&mut self) -> Result<Variable, String> {
        self.advance()?;

        let id = self.parse_id();

        self.advance()?;

        self.expect(Equal)?;
        self.advance()?;

        if self.c_token.t_type == Semicolon {
            return Err("Variable must be assigned!".to_string());
        }

        let val = match self.c_token.t_type {
            RawStr => Statement::Val(self.parse_string()),
            TokenType::Ident => match self.next_token().unwrap().t_type {
                Plus | Minus | Slash | Star => *self.parse_expr().unwrap(),
                _ => Statement::IdentState(self.parse_id()),
            },
            IntNum | FloatNum => *self.parse_expr().unwrap(),
            _ => {
                return Err(format!(
                    "Unexpected token: {} In line: {}",
                    self.c_token.clone(),
                    self.tokens[self.index]
                ))
            }
        };

        let var_state = Variable::new(id, val);
        Ok(var_state)
    }

    fn parse_id(&self) -> Identifier {
        Identifier::new(self.c_token.clone())
    }

    fn parse_expr(&mut self) -> Result<Box<Statement>, String> {
        let left = self.parse_add_expr()?;
        Ok(left)
    }

    fn parse_primary_expr(&mut self) -> Result<Box<Statement>, String> {
        let token_t = self.c_token.clone().t_type;

        let ret: Box<Statement> = match token_t {
            TokenType::Ident => Box::new(Statement::IdentState(self.parse_id())),
            TokenType::IntNum => Box::new(Statement::Val(self.parse_int_num())),
            TokenType::FloatNum => Box::new(Statement::Val(self.parse_float_num())),
            TokenType::LParen => {
                self.advance()?; // open paren
                let expr = self.parse_expr()?;
                self.expect(TokenType::RParen)?;
                self.advance()?;
                expr
            }
            _ => return Err(format!("Unsupportet token: {:#?}", token_t)),
        };

        if self.c_token.t_type != Semicolon {
            self.advance()?;
        }
        Ok(ret)
    }

    fn parse_add_expr(&mut self) -> Result<Box<Statement>, String> {
        let mut left_part = self.parse_multiplicative_expr().unwrap();
        while self.c_token.t_type == TokenType::Plus || self.c_token.t_type == TokenType::Minus {
            let operator = self.c_token.clone().value;
            self.advance()?;
            let right = self.parse_multiplicative_expr()?;
            let left = left_part.clone();
            left_part = Box::new(Statement::BinaryState(Expression::new(
                ExpressionType::BinaryExpression {
                    left,
                    operator,
                    right,
                },
            )));
        }

        Ok(left_part)
    }

    fn parse_multiplicative_expr(&mut self) -> Result<Box<Statement>, String> {
        let mut left_part = self.parse_primary_expr().unwrap();
        while self.c_token.t_type == TokenType::Star || self.c_token.t_type == TokenType::Slash {
            let operator = self.c_token.clone().value;
            self.advance()?;
            let right = self.parse_primary_expr()?;
            let left = left_part.clone();
            left_part = Box::new(Statement::BinaryState(Expression::new(
                ExpressionType::BinaryExpression {
                    left,
                    operator,
                    right,
                },
            )));
        }

        Ok(left_part)
    }

    fn parse_assignment_expr(&mut self) -> Result<Statement, String> {
        let left = self.parse_id();
        self.advance()?; // skip id
        self.advance()?; // skip '=' token
        let right = self.parse_expr()?;
        let ret = Statement::AssignmentState(AssignmentExpr::new(left, right));
        Ok(ret)
    }

    fn parse_oper_call(&mut self) -> Result<Statement, String> {
        let name = self.c_token.clone().value;
        let is_native: bool = match name.as_str() {
            "print" | "println" => true,
            "something" => true,
            "aoeuaoe" => true,
            _ => false,
        };
        self.advance()?; // pass oper name

        self.expect(TokenType::LParen)?; // expect Lparen
        self.advance()?; // skip Lparen

        let mut args: Vec<Statement> = vec![];

        while self.c_token.t_type != TokenType::RParen {
            let arg = self.parse_statement().unwrap();
            args.push(arg.clone());
            if self.c_token.t_type != RParen {
                self.advance()?;
            }

            if self.c_token.t_type == TokenType::Comma {
                self.advance()?; // skip comma
            } else if self.c_token.t_type == TokenType::RParen {
                break; // if encounters with ) then quit loop
            } else {
                return Err(format!(
                    "Expected ',' or ')', found: {} In line: {}",
                    self.c_token,
                    self.tokens[self.index].value.clone()
                ));
            }
        }

        self.expect(TokenType::RParen)?; // expect Close paren
        self.advance()?; // Skip rparen

        Ok(Statement::OperCallState(OperCall::new(
            name,
            Some(args),
            is_native,
        )))
    }

    fn advance(&mut self) -> Result<(), String> {
        self.index += 1;
        if self.index < self.tokens.len() {
            self.c_token = self.tokens[self.index].clone();
            Ok(())
        } else {
            Err("Out of range".to_string())
        }
    }

    fn parse_return(&mut self) -> Result<Statement, String> {
        self.advance().unwrap(); // skip ret token itself

        if self.c_token.t_type == Semicolon {
            return Ok(Statement::RetState(RetStatement::new(None)));
        }

        let value = Some(Box::new(self.parse_statement().unwrap()));
        //self.advance()?;
        //println!("c token: {}", self.c_token.clone());
        let r = RetStatement::new(value);
        Ok(Statement::RetState(r))
    }

    fn parse_if(&mut self) -> Result<IfStatement, String> {
        self.advance()?; // skip "if" }
        let test: Option<Box<Expression>> = if self.c_token.t_type != LBrace {
            Some(Box::new(self.parse_bin_expr().unwrap()))
        } else {
            None
        };

        self.advance()?; // go to consequent part

        let mut consequent: Vec<Statement> = Vec::new();
        while self.c_token.t_type != RBrace {
            consequent.push(self.parse_statement().unwrap());
            if self.c_token.t_type != RBrace {
                self.advance()?;
            }
        }

        self.advance()?; // skip rbrace
        if self.c_token.t_type == TokenType::Else && self.next_token().unwrap().t_type == If {
            self.advance()?;
            let alter = self.parse_if().unwrap();
            return Ok(IfStatement::new(test, consequent, Some(Box::new(alter))));
        } else if self.c_token.t_type == Else && self.next_token().unwrap().t_type == LBrace {
            let alter = self.parse_if().unwrap();
            return Ok(IfStatement::new(None, consequent, Some(Box::new(alter))));
        }

        if test.is_some() {
            return Ok(IfStatement::new(test, consequent, None));
        }
        if self.c_token.t_type == RBrace {
            self.advance()?;
        }
        Ok(IfStatement::new(None, consequent, None))
    }

    fn parse_bin_primary_expr(&mut self) -> Result<Box<Statement>, String> {
        let token_t = self.c_token.clone().t_type;

        let ret: Box<Statement> = match token_t {
            TokenType::Ident => Box::new(Statement::IdentState(self.parse_id())),
            TokenType::IntNum => Box::new(Statement::Val(self.parse_int_num())),
            TokenType::FloatNum => Box::new(Statement::Val(self.parse_float_num())),
            _ => {
                return Err(format!(
                    "Unsupportet token: {:#?} in index: {}",
                    token_t, self.index
                ))
            }
        };

        Ok(ret)
    }

    fn make_bin(&mut self) -> Result<Expression, String> {
        let left = self.parse_bin_primary_expr().unwrap();
        self.advance()?;
        let operator = self.c_token.clone().value;
        self.advance()?;
        let right = self.parse_bin_primary_expr().unwrap();
        let ret = Expression::new(ExpressionType::BinaryExpression {
            left,
            operator,
            right,
        });
        Ok(ret)
    }

    fn parse_bin_expr(&mut self) -> Result<Expression, String> {
        let mut left_part = self.make_bin().unwrap();
        self.advance()?; // next token which is operator
        while self.c_token.t_type == TokenType::And || self.c_token.t_type == TokenType::Or {
            let operator = self.c_token.clone().value;
            self.advance()?;
            let right = self.make_bin().unwrap();
            self.advance()?;
            let left = left_part.clone();
            left_part = Expression::new(ExpressionType::BinaryExpression {
                left: Box::new(Statement::BinaryState(left)),
                operator,
                right: Box::new(Statement::BinaryState(right)),
            });
        }
        Ok(left_part)
    }

    fn parse_int_num(&self) -> VarValue {
        let val = self.c_token.clone().value.parse::<i64>().unwrap();
        VarValue::ExprVal(Expression::new(ExpressionType::IntNumber(val)))
    }

    fn parse_float_num(&self) -> VarValue {
        let val = self.c_token.clone().value.parse::<f64>().unwrap();
        VarValue::ExprVal(Expression::new(ExpressionType::FloatNumber(val)))
    }

    fn parse_string(&mut self) -> VarValue {
        VarValue::StringVal(self.c_token.clone().value.to_string())
    }

    fn expect(&self, exp_t: TokenType) -> Result<(), String> {
        if self.c_token.t_type != exp_t {
            return Err(format!(
                "Expecting {:#?}, found: {}",
                exp_t,
                self.c_token.clone()
            ));
        }
        Ok(())
    }

    // parsing operation call/definiton

    fn parse_operation(&mut self) -> Result<Option<Box<Statement>>, String> {
        self.advance().unwrap(); // skip oper token

        let oper_name = self.c_token.clone().value;
        self.advance().unwrap();

        self.expect(LParen)?;
        self.advance().unwrap(); // Skip lparen
        let mut args: Vec<Identifier> = vec![];

        while self.c_token.t_type != RParen {
            if self.c_token.t_type == Comma {
                self.advance()?;
            } else {
                args.push(self.parse_id());
                self.advance()?;
            }
        }

        self.expect(RParen)?;
        self.advance().unwrap(); // Skip rparen

        self.expect(LBrace)?;
        self.advance().unwrap(); // Skip lbrace

        let mut body: Vec<Statement> = vec![];

        while self.c_token.t_type != RBrace {
            body.push(self.parse_statement().unwrap());
            if self.c_token.t_type != RBrace {
                self.advance()?;
            }
        }

        let ret_statements: Vec<&RetStatement> = body
            .iter()
            .filter_map(|stmt| {
                if let Statement::RetState(ret_stmt) = stmt {
                    Some(ret_stmt)
                } else {
                    None
                }
            })
            .collect();

        if !ret_statements.is_empty() && ret_statements.len() != 1 {
            for c_r in 0..ret_statements.len() {
                if ret_statements[c_r].ret_type.is_some() {
                    for n_r in 1..ret_statements.len() {
                        if ret_statements[c_r].ret_type.as_ref().unwrap()
                            != ret_statements[n_r].ret_type.as_ref().unwrap()
                        {
                            return Err("Returns must have similar return type".to_string());
                        }
                    }
                } else {
                    return Err("Returns must have similar return type".to_string());
                }
            }
        }

        self.expect(RBrace)?;
        self.advance()?;

        if args.is_empty() {
            Ok(Some(Box::new(Statement::OperState(
                OperationDeclaration::new(oper_name.to_string(), None, Some(body)),
            ))))
        } else {
            Ok(Some(Box::new(Statement::OperState(
                OperationDeclaration::new(oper_name.to_string(), Some(args), Some(body)),
            ))))
        }
    }
}
