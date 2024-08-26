use crate::lexing::ast::Statement::IfState;
use crate::lexing::ast::Statement::Val;
use crate::lexing::ast::Statement::VarState;
use crate::lexing::ast::{Statement, *};
use colored::*;

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

    pub fn parse(&mut self) -> Result<Ast, String> {
        let mut prog: Ast = Ast::new(Vec::new());

        while self.c_token.t_type != Eof {
            prog.push_statement(self.parse_statement().unwrap());
        }

        println!("prog: {:#?}", prog);
        Ok(prog)
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        let stmt = match self.c_token.t_type {
            TokenType::Var => VarState(self.parse_var()?),
            TokenType::If => IfState(self.parse_if()?),
            TokenType::Repay => self.parse_return()?,
            TokenType::IntNum | TokenType::FloatNum => *self.parse_expr()?,
            TokenType::Ident => {
                self.parse_ident_statement().unwrap()
            },
            TokenType::RawStr => Val(self.parse_string()),
            TokenType::Oper => *self.parse_operation()?.unwrap(),
            TokenType::True => todo!(),
            TokenType::False => todo!(),
            _ => {
                eprintln!("{} token in statement: {}", "Unexpected".red(), self.c_token.clone());
                return Err(self.report_error().err().unwrap())
            }
        };
        if self.c_token.t_type == Semicolon && self.c_token.t_type != Eof {
            self.advance().unwrap();
        }

        Ok(stmt)
    }

    fn parse_ident_statement(&mut self) -> Result<Statement, String> {
        let next_token = match self.next_token().unwrap().t_type {
            TokenType::LParen => self.parse_oper_call()?,
            TokenType::Plus | TokenType::Minus | TokenType::Slash | TokenType::Star => {
                *self.parse_expr()?
            }
            TokenType::Equal => self.parse_assignment_expr()?,
            _ => {
                //self.c_token = ;
                Statement::IdentState(self.parse_id())
            }
        };

        Ok(next_token)
    }

    fn parse_var(&mut self) -> Result<Variable, String> {
        self.advance()?;

        let id = self.parse_id();

        self.advance()?;

        if self.c_token.t_type == Semicolon {
            return Err("Variable must be assigned!".to_string());
        }

        self.expect(Equal)?;
        self.advance()?;

        let val = match self.c_token.t_type {
            RawStr => Statement::Val(self.parse_string()),
            TokenType::Ident => match self.next_token().unwrap().t_type {
                Plus | Minus | Slash | Star => *self.parse_expr().unwrap(),
                _ => Statement::IdentState(self.parse_id()),
            },
            IntNum | FloatNum => *self.parse_expr().unwrap(),
            _ => {
                eprintln!("{} token in var: {}", "Unexpected".red(), self.c_token.clone());
                return Err(self.report_error().err().unwrap());
            }
        };

        let var_state = Variable::new(id, val);
        Ok(var_state)
    }

    fn parse_id(&mut self) -> Identifier {
        let id = self.c_token.clone();
        Identifier::new(id)
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
            _ => {
                eprintln!("{} token in var: {}", "Unsupported".red(), self.c_token.clone());
                return Err(self.report_error().err().unwrap());
            }
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

    /// Parsing operation call/definiton

    fn parse_oper_call(&mut self) -> Result<Statement, String> {
        // Capture the operator name
        let name = self.c_token.value.clone();
        let is_native = matches!(name.as_str(), "print" | "println" | "something" | "aoeuaoe");

        self.advance()?;
        self.expect(TokenType::LParen)?;
        self.advance()?;

        let mut args: Vec<Statement> = Vec::new();

        loop {
            // Parse the next argument
            let arg = self.parse_statement().map_err(|e| format!("Failed to parse argument: {}", e))?;
            args.push(arg);

            if self.c_token.t_type != RParen {
                self.advance()?;
            }


            // Check for a comma or closing parenthesis
            match self.c_token.t_type {
                TokenType::Comma => {
                    self.advance()?; // Skip comma
                    continue;
                },
                TokenType::RParen => {
                    self.advance()?;
                    break;
                },
                _ => {
                    eprintln!("{} in oper call: {}", "Unexpected token".red(), self.c_token.clone());
                    return Err(self.report_error().err().unwrap());
                }
            }
        }

        // Check for unexpected tokens after the closing parenthesis
        if self.c_token.t_type != TokenType::Eof && self.c_token.t_type != TokenType::Semicolon {
            eprintln!("{} in oper call: {}", "Unexpected token".red(), self.c_token.clone());
            return Err(self.report_error().err().unwrap());
        }

        Ok(Statement::OperCallState(OperCall::new(name, Some(args), is_native)))
    }

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


    fn parse_return(&mut self) -> Result<Statement, String> {
        self.advance().unwrap(); // skip ret token itself

        if self.c_token.t_type == Semicolon {
            return Ok(Statement::RetState(RetStatement::new(None)));
        }

        let value = Some(Box::new(self.parse_statement().unwrap()));
        //self.advance()?;
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
        println!("test {}", test.clone().unwrap());

        if self.c_token.t_type == LBrace {
            self.advance()?; // go to consequent part
        }

        let mut consequent: Vec<Statement> = Vec::new();
        loop {
            consequent.push(self.parse_statement().unwrap());
            if self.c_token.t_type != RBrace {
                self.advance()?;
            } else {
                break;
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
                eprintln!("{} token in var: {}", "Unsupported".red(), self.c_token.clone());
                return Err(self.report_error().err().unwrap());
            }
        };

        self.advance()?;

        Ok(ret)
    }

    fn make_bin(&mut self) -> Result<Expression, String> {
        let left = self.parse_bin_primary_expr().unwrap();

        let operator = self.c_token.clone().value;
        self.advance()?; // skip oper

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
        self.advance()?; // skip to the next token which is operator
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

    fn parse_int_num(&mut self) -> VarValue {
        let val = self.c_token.clone().value.parse::<i64>().unwrap();
        VarValue::ExprVal(Expression::new(ExpressionType::IntNumber(val)))
    }

    fn parse_float_num(&mut self) -> VarValue {
        let val = self.c_token.clone().value.parse::<f64>().unwrap();
        VarValue::ExprVal(Expression::new(ExpressionType::FloatNumber(val)))
    }

    fn parse_string(&mut self) -> VarValue {
        VarValue::StringVal(self.c_token.clone().value.to_string())
    }

    /// Helper functions

    fn report_error(&self) -> Result<(), String> {
        let start_index = self.index.saturating_sub(3);
        let mut end_index = self.index;
        let mut temp_token = self.c_token.clone().t_type;

        while (temp_token != Semicolon && temp_token != Eof && temp_token != RBrace) && end_index != self.index + 5 {
            temp_token = self.tokens[end_index].clone().t_type;
            end_index += 1;
        }

        let tokens_to_report: Vec<Token> = self.tokens[start_index..end_index].to_vec();

        let output: Vec<String> = tokens_to_report
            .iter()
            .map(|token| {
                if token == &self.c_token {
                    format!("{}", token.value.red().underline())
                } else {
                    token.value.clone()
                }
            })
            .collect();

        let output_message = output.join(" ");

        eprintln!("\n{}\n \n\t{}\n", "Parsing error:".red().bold(), output_message);
        Err("Parsing error".to_string())
    }

    fn next_token(&self) -> Result<Token, String> {
        match self.tokens.get(self.index + 1) {
            Some(token) => Ok(token.clone()),
            None => Err("Out of range while trying to get next token".to_string()),
        }
    }

    fn advance(&mut self) -> Result<(), String> {
        self.index += 1;
        if self.index < self.tokens.len() {
            self.c_token = self.tokens[self.index].clone();
            Ok(())
        } else {
            Err(format!("Out of range {} of {}", self.index, self.tokens.len()))
        }
    }

    fn expect(&self, exp_t: TokenType) -> Result<(), String> {
        if self.c_token.t_type != exp_t {
            eprintln!("Expecting token {:#?}, found: {}", exp_t, self.c_token.clone());
            return Err(self.report_error().err().unwrap());
        }
        Ok(())
    }

}
