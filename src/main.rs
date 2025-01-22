use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LBrace,
    RBrace,
    Colon,
    Comma,
    Equals,
    Semicolon,
    Identifier(String),
    Number(i32),
    Function,
    If,
    Else,
    Let,
    Return,
    Plus,
    Greater,
    Less,
    LParen,
    RParen,
    Dot,
    Unknown(char),
}

#[derive(Debug, Clone, PartialEq)]
enum Expression {
    Number(i32),
    Variable(String),
    Object(HashMap<String, Expression>),
    ObjectAccess(Box<Expression>, String),
    BinaryOp(Box<Expression>, String, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    IfElse(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
}

#[derive(Debug, Clone, PartialEq)]
enum Statement {
    Let(String, Expression),
    FunctionDef(String, Vec<String>, Vec<Statement>),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Number(i32),
    Object(HashMap<String, Value>),
    Function(Vec<String>, Vec<Statement>),
}

struct Lexer {
    input: Vec<char>,
    position: usize,
}

impl Lexer {
    fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        while let Some(&ch) = self.input.get(self.position) {
            self.position += 1;
            return match ch {
                '{' => Some(Token::LBrace),
                '}' => Some(Token::RBrace),
                ':' => Some(Token::Colon),
                ',' => Some(Token::Comma),
                '=' => Some(Token::Equals),
                ';' => Some(Token::Semicolon),
                '(' => Some(Token::LParen),
                ')' => Some(Token::RParen),
                '+' => Some(Token::Plus),
                '>' => Some(Token::Greater),
                '<' => Some(Token::Less),
                '.' => Some(Token::Dot),
                'a'..='z' | 'A'..='Z' => {
                    let mut ident = ch.to_string();
                    while let Some(&next) = self.input.get(self.position) {
                        if next.is_alphanumeric() {
                            ident.push(next);
                            self.position += 1;
                        } else {
                            break;
                        }
                    }
                    match ident.as_str() {
                        "let" => Some(Token::Let),
                        "function" => Some(Token::Function),
                        "if" => Some(Token::If),
                        "else" => Some(Token::Else),
                        "return" => Some(Token::Return),
                        _ => Some(Token::Identifier(ident)),
                    }
                }
                '0'..='9' => {
                    let mut num = ch.to_string();
                    while let Some(&next) = self.input.get(self.position) {
                        if next.is_digit(10) {
                            num.push(next);
                            self.position += 1;
                        } else {
                            break;
                        }
                    }
                    Some(Token::Number(num.parse().unwrap()))
                }
                _ if ch.is_whitespace() => continue,
                _ => Some(Token::Unknown(ch)),
            };
        }
        None
    }
}

struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    fn parse_statements(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();
        while self.position < self.tokens.len() {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            } else {
                break;
            }
        }
        statements
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.tokens.get(self.position) {
            Some(Token::Let) => {
                self.position += 1;
                if let Some(Token::Identifier(name)) = self.tokens.get(self.position).cloned() {
                    self.position += 1;
                    if let Some(Token::Equals) = self.tokens.get(self.position) {
                        self.position += 1;
                        if let Some(expr) = self.parse_expression() {
                            if let Some(Token::Semicolon) = self.tokens.get(self.position) {
                                self.position += 1;
                                return Some(Statement::Let(name, expr));
                            }
                        }
                    }
                }
            }
            Some(Token::Function) => {
                self.position += 1;
                if let Some(Token::Identifier(name)) = self.tokens.get(self.position).cloned() {
                    self.position += 1;
                    if let Some(Token::LParen) = self.tokens.get(self.position) {
                        self.position += 1;
                        let mut params = Vec::new();
                        while let Some(Token::Identifier(param)) =
                            self.tokens.get(self.position).cloned()
                        {
                            params.push(param);
                            self.position += 1;
                            if let Some(Token::Comma) = self.tokens.get(self.position) {
                                self.position += 1;
                            } else {
                                break;
                            }
                        }
                        if let Some(Token::RParen) = self.tokens.get(self.position) {
                            self.position += 1;
                            let mut body = Vec::new();
                            if let Some(Token::LBrace) = self.tokens.get(self.position) {
                                self.position += 1;
                                while self.tokens.get(self.position) != Some(&Token::RBrace) {
                                    if let Some(stmt) = self.parse_statement() {
                                        body.push(stmt);
                                    }
                                }
                                self.position += 1; // Skip '}'
                                return Some(Statement::FunctionDef(name, params, body));
                            }
                        }
                    }
                }
            }
            _ => {
                if let Some(expr) = self.parse_expression() {
                    if let Some(Token::Semicolon) = self.tokens.get(self.position) {
                        self.position += 1;
                        return Some(Statement::Expression(expr));
                    }
                }
            }
        }
        None
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        let mut left = self.parse_term()?;
        while let Some(token) = self.tokens.get(self.position) {
            match token {
                Token::Plus => {
                    self.position += 1;
                    let right = self.parse_term()?;
                    left = Expression::BinaryOp(Box::new(left), "+".to_string(), Box::new(right));
                }
                _ => break,
            }
        }
        Some(left)
    }

    fn parse_term(&mut self) -> Option<Expression> {
        match self.tokens.get(self.position) {
            Some(Token::Number(num)) => {
                self.position += 1;
                Some(Expression::Number(*num))
            }
            Some(Token::Identifier(name)) => {
                self.position += 1;
                let mut expr = Expression::Variable(name.clone());
                while let Some(Token::Dot) = self.tokens.get(self.position) {
                    self.position += 1; // Skip '.'
                    if let Some(Token::Identifier(field)) = self.tokens.get(self.position).cloned()
                    {
                        self.position += 1;
                        expr = Expression::ObjectAccess(Box::new(expr), field);
                    }
                }
                if let Some(Token::LParen) = self.tokens.get(self.position) {
                    self.position += 1; // Skip '('
                    let mut args = Vec::new();
                    while self.tokens.get(self.position) != Some(&Token::RParen) {
                        if let Some(arg) = self.parse_expression() {
                            args.push(arg);
                            if let Some(Token::Comma) = self.tokens.get(self.position) {
                                self.position += 1; // Skip ','
                            }
                        }
                    }
                    self.position += 1; // Skip ')'
                    expr = Expression::FunctionCall(name.clone(), args);
                }
                Some(expr)
            }
            Some(Token::LBrace) => {
                self.position += 1; // Skip '{'
                let mut fields = HashMap::new();
                while self.tokens.get(self.position) != Some(&Token::RBrace) {
                    if let Some(Token::Identifier(key)) = self.tokens.get(self.position).cloned() {
                        self.position += 1;
                        if let Some(Token::Colon) = self.tokens.get(self.position) {
                            self.position += 1;
                            if let Some(value) = self.parse_expression() {
                                fields.insert(key, value);
                                if let Some(Token::Comma) = self.tokens.get(self.position) {
                                    self.position += 1;
                                }
                            }
                        }
                    }
                }
                self.position += 1; // Skip '}'
                Some(Expression::Object(fields))
            }
            _ => None,
        }
    }
}

struct Interpreter {
    variables: HashMap<String, Value>,
}

impl Interpreter {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn execute_statements(&mut self, statements: Vec<Statement>) {
        for statement in statements {
            self.execute_statement(statement);
        }
    }

    fn execute_statement(&mut self, statement: Statement) {
        println!("Executing: {:?}", statement);
        match statement {
            Statement::Let(name, expr) => {
                let value = self.evaluate_expression(expr);
                self.variables.insert(name, value);
            }
            Statement::FunctionDef(name, params, body) => {
                self.variables.insert(name, Value::Function(params, body));
            }
            Statement::Expression(expr) => {
                let result = self.evaluate_expression(expr);
                println!("Result: {:?}", result);
            }
        }
    }

    fn evaluate_expression(&self, expr: Expression) -> Value {
        match expr {
            Expression::Number(num) => Value::Number(num),
            Expression::Variable(name) => self
                .variables
                .get(&name)
                .cloned()
                .unwrap_or_else(|| panic!("Variable {} not found", name)),
            Expression::Object(fields) => {
                let mut object = HashMap::new();
                for (key, value_expr) in fields {
                    object.insert(key, self.evaluate_expression(value_expr));
                }
                Value::Object(object)
            }
            Expression::ObjectAccess(object_expr, key) => {
                if let Value::Object(map) = self.evaluate_expression(*object_expr) {
                    map.get(&key)
                        .cloned()
                        .unwrap_or_else(|| panic!("Field {} not found", key))
                } else {
                    panic!("Accessing a non-object value");
                }
            }
            Expression::BinaryOp(left, op, right) => {
                let left_value = self.evaluate_expression(*left);
                let right_value = self.evaluate_expression(*right);
                match (left_value, right_value) {
                    (Value::Number(left_num), Value::Number(right_num)) => {
                        let result = match op.as_str() {
                            "+" => left_num + right_num,
                            _ => panic!("Unsupported operator: {}", op),
                        };
                        Value::Number(result)
                    }
                    _ => panic!("Type error in binary operation"),
                }
            }
            Expression::FunctionCall(name, args) => {
                if let Value::Function(params, body) = self.variables.get(&name).cloned().unwrap() {
                    let mut local_context = self.variables.clone();
                    for (param, arg) in params.iter().zip(args) {
                        let arg_value = self.evaluate_expression(arg);
                        local_context.insert(param.clone(), arg_value);
                    }
                    let mut interpreter = Interpreter {
                        variables: local_context,
                    };
                    interpreter.execute_statements(body);
                    Value::Number(0) // For now, return 0 for function calls
                } else {
                    panic!("Function {} not found", name);
                }
            }
            _ => unimplemented!(),
        }
    }
}

fn main() {
    let code = r#"
        let obj = { x: 10, y: 20 };
        obj.x + obj.y;
        function myfun(x, y) {
            return x + y;
        }
        myfun(obj.x, obj.y);
    "#;

    let mut lexer = Lexer::new(code);
    let tokens: Vec<Token> = std::iter::from_fn(|| lexer.next_token()).collect();
    println!("Tokens: {:?}", tokens);

    let mut parser = Parser::new(tokens);
    let statements = parser.parse_statements();
    println!("Statements: {:?}", statements);

    let mut interpreter = Interpreter::new();
    interpreter.execute_statements(statements);
}
