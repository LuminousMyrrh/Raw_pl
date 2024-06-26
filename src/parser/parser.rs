use crate::parser::ast::Statement::IfState;
use crate::parser::ast::Statement::Val;
use crate::parser::ast::Statement::VarState;
use crate::parser::ast::{Statement, *};
//use backtrace::Backtrace;

use crate::scan::token::{Token, TokenType, TokenType::*};

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    c_token: Token,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.clone(),
            index: 0,
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
            if let Ok(Some(s)) = self.parse_statement() {
                prog.push_statement(s);
            }
            if self.c_token.t_type == Semicolon {
                continue;
            } else if self.c_token.t_type == Eof {
                break;
            }
        }

        //println!("prog: {:#?}", prog);
        Ok(prog)
    }

    fn parse_statement(&mut self) -> Result<Option<Statement>, String> {
        let statement: Option<Statement>;
        match self.c_token.t_type {
            Var => {
                statement = Some(VarState(self.parse_var()?));
            }
            If => statement = Some(IfState(self.parse_if().unwrap())),
            Repay => statement = Some(self.parse_return()?),
            IntNum | FloatNum => {
                statement = Some(*self.parse_expr().unwrap());
            }
            TokenType::Ident => {
                if self.next_token().unwrap().t_type == LParen {
                    statement = Some(self.parse_oper_call().unwrap());
                } else if self.next_token().unwrap().t_type == Plus
                    || self.next_token().unwrap().t_type == Minus
                    || self.next_token().unwrap().t_type == Slash
                    || self.next_token().unwrap().t_type == Star
                {
                    statement = Some(*self.parse_expr().unwrap());
                } else if self.next_token().unwrap().t_type == Equal {
                    statement = Some(self.parse_assignment_expr().unwrap());
                } else {
                    statement = Some(Statement::IdentState(self.parse_id()));
                }
            }
            RawStr => statement = Some(Val(self.parse_string())),
            Oper => statement = Some(*self.parse_operation().unwrap().unwrap()),
            True => todo!(),
            False => todo!(),
            Semicolon => {
                self.advance()?;
                return Ok(None);
            }
            _ => {
                return Err(format!(
                    "Unexpected token:{} Next token: {}",
                    self.c_token, self.next_token().unwrap()
                ))
            }
        };
				//println!("{:#?}", statement);
        Ok(statement)
    }

    // Holy function
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
            let operator = self.c_token.clone();
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
            let operator = self.c_token.clone();
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
        //let location = Backtrace::new();

        //print!("assignment() called from {:?}", location);
        //println!("assigning {}\n", self.c_token.clone());
        let left = self.parse_id();
        self.advance()?; // skip id
        self.advance()?; // skip '=' token
        let right = self.parse_expr()?;
        let ret = Statement::AssignmentState(AssignmentExpr::new(left, right));
        Ok(ret)
    }

    fn parse_oper_call(&mut self) -> Result<Statement, String> {
        let name = self.c_token.clone().value.unwrap();
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
            let arg = self.parse_statement().unwrap().unwrap();
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
                    "Expected ',' or ')', found: {}In line: {}",
                    self.c_token,
                    self.tokens[self.index].value.clone().unwrap()
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
        // let location = Backtrace::new();

        // print!("advance() called from {:?}", location);
        // println!("advancing {}\n", self.c_token.clone());
        if self.index < self.tokens.len() - 1 {
            //println!("advancing: {}", self.c_token.clone());
            self.index += 1;
            self.c_token = self.tokens[self.index].clone();
            Ok(())
        } else {
            Err("Out of range".to_string())
        }
    }

    fn parse_return(&mut self) -> Result<Statement, String> {
        let ret_t = self.c_token.clone();
        self.advance().unwrap();

        if self.c_token.t_type == Semicolon {
            return Ok(Statement::RetState(RetStatement::new(ret_t, None)));
        }
        let value = Some(Box::new(self.parse_statement().unwrap().unwrap()));
        let r = RetStatement::new(ret_t, value);
        Ok(Statement::RetState(r))
    }

    fn parse_if(&mut self) -> Result<IfStatement, String> {
				self.advance()?; // skip "if" token
				let test = self.parse_bin_expr().unwrap();

				self.advance()?; // go to consequent part

				let mut consequent: Vec<Statement> = Vec::new();
				while self.c_token.t_type != RBrace {
						consequent.push(self.parse_statement().unwrap().unwrap());
						self.advance()?;
				};

				Ok(IfStatement::new(test, consequent, None))

    }

		fn parse_bin_primary_expr(&mut self) -> Result<Box<Statement>, String> {
        let token_t = self.c_token.clone().t_type;

        let ret: Box<Statement> = match token_t {
            TokenType::Ident => Box::new(Statement::IdentState(self.parse_id())),
            TokenType::IntNum => Box::new(Statement::Val(self.parse_int_num())),
            TokenType::FloatNum => Box::new(Statement::Val(self.parse_float_num())),
            _ => return Err(format!("Unsupportet token: {:#?}", token_t)),
        };

        Ok(ret)
		}

		fn make_bin(&mut self) -> Result<Box<Statement>, String> {
				let left = self.parse_bin_primary_expr().unwrap();
				self.advance()?;
				let operator = self.c_token.clone();
				self.advance()?;
				let right = self.parse_bin_primary_expr().unwrap();
				let ret = Box::new(Statement::BinaryState(Expression::new(ExpressionType::BinaryExpression{
						left,
						operator,
						right
				})));
				Ok(ret)
		}

		fn parse_bin_expr(&mut self) -> Result<Box<Statement>, String> {
				let mut left_part = self.make_bin().unwrap();
				self.advance()?; // next token which is operator
				while self.c_token.t_type == TokenType::And || self.c_token.t_type == TokenType::Or {
						let operator = self.c_token.clone();
						self.advance()?;
						let right = self.make_bin().unwrap();
						self.advance()?;
						let left = left_part.clone();
						left_part = Box::new(Statement::BinaryState(Expression::new(
								ExpressionType::BinaryExpression {
										left,
										operator,
										right,
								}
						)));
				}
				Ok(left_part)
		}

    fn parse_int_num(&self) -> VarValue {
        let val = self.c_token.clone().value.unwrap().parse::<i64>().unwrap();
        VarValue::ExprVal(Expression::new(ExpressionType::IntNumber(val)))
    }

    fn parse_float_num(&self) -> VarValue {
        let val = self.c_token.clone().value.unwrap().parse::<f64>().unwrap();
        VarValue::ExprVal(Expression::new(ExpressionType::FloatNumber(val)))
    }

    fn parse_string(&mut self) -> VarValue {
        VarValue::StringVal(self.c_token.clone().value.unwrap().to_string())
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
        let oper_name = self.c_token.clone().value.unwrap();

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
            if let Some(v) = self.parse_statement().unwrap() {
                body.push(v);
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
