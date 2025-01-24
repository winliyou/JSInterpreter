use log::info;
use std::collections::hash_map::DefaultHasher;
use std::env;
use std::fs;
use std::hash::{Hash, Hasher};
use std::io::{self, Write};
use std::path::Path;
use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq)]
struct Token {
    kind: TokenKind,
    line: usize,
    column: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum KeywordCatalog {
    Let,
    Function,
    If,
    Else,
    Return,
    While,
}

#[derive(Debug, Clone, PartialEq)]
enum BuiltInCatalog {
    Print,
}

#[derive(Debug, Clone, PartialEq)]
enum OperatorCatalog {
    Plus,
    Minus,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, PartialEq)]
enum TokenKind {
    LBrace,
    RBrace,
    Colon,
    Comma,
    Equals,
    Semicolon,
    Identifier(String),
    Number(i32),
    StringLiteral(String),
    Keyword(KeywordCatalog),
    BuiltIn(BuiltInCatalog),
    Operator(OperatorCatalog),
    LParen,
    RParen,
    Dot,
    Unknown(char),
}

#[derive(Debug, Clone, PartialEq)]
enum Expression {
    Number(i32),
    StringLiteral(String),
    Variable(String),
    Object(HashMap<String, Expression>),
    ObjectAccess(Box<Expression>, String),
    BinaryOp(Box<Expression>, OperatorCatalog, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    UnaryOp(String, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
enum Statement {
    Let(String, Expression),
    FunctionDef(String, Vec<String>, Vec<Statement>),
    Expression(Expression),
    Assignment(Expression, Expression),
    If(Expression, Vec<Statement>, Option<Vec<Statement>>),
    Return(Expression),
    While(Expression, Vec<Statement>),
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Number(i32),
    StringLiteral(String),
    Object(HashMap<String, Value>),
    Function(Vec<String>, Vec<Statement>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Value::Number(num) => write!(f, "{}", num),
            Value::StringLiteral(string) => write!(f, "\"{}\"", string), // 新增 StringLiteral 类型
            Value::Object(map) => {
                let fields: Vec<String> = map
                    .iter()
                    .map(|(key, value)| format!("{}: {:?}", key, value))
                    .collect();
                write!(f, "{{{}}}", fields.join(", "))
            }
            Value::Function(params, _) => write!(f, "fn({})", params.join(", ")),
        }
    }
}

struct Lexer {
    input: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        while let Some(&ch) = self.input.get(self.position) {
            let token_start_line = self.line;
            let token_start_column = self.column;
            self.position += 1;
            self.column += 1;
            return match ch {
                '{' => {
                    Some(self.create_token(TokenKind::LBrace, token_start_line, token_start_column))
                }
                '}' => {
                    Some(self.create_token(TokenKind::RBrace, token_start_line, token_start_column))
                }
                ':' => {
                    Some(self.create_token(TokenKind::Colon, token_start_line, token_start_column))
                }
                ',' => {
                    Some(self.create_token(TokenKind::Comma, token_start_line, token_start_column))
                }
                '=' => self.handle_equals(token_start_line, token_start_column),
                ';' => Some(self.create_token(
                    TokenKind::Semicolon,
                    token_start_line,
                    token_start_column,
                )),
                '(' => {
                    Some(self.create_token(TokenKind::LParen, token_start_line, token_start_column))
                }
                ')' => {
                    Some(self.create_token(TokenKind::RParen, token_start_line, token_start_column))
                }
                '+' => {
                    if let Some(&'+') = self.input.get(self.position) {
                        self.position += 1;
                        self.column += 1;
                        Some(self.create_token(
                            TokenKind::Operator(OperatorCatalog::Plus),
                            token_start_line,
                            token_start_column,
                        ))
                    } else {
                        Some(self.create_token(
                            TokenKind::Operator(OperatorCatalog::Plus),
                            token_start_line,
                            token_start_column,
                        ))
                    }
                }
                '-' => {
                    if let Some(&'-') = self.input.get(self.position) {
                        self.position += 1;
                        self.column += 1;
                        Some(self.create_token(
                            TokenKind::Operator(OperatorCatalog::Minus),
                            token_start_line,
                            token_start_column,
                        ))
                    } else {
                        Some(self.create_token(
                            TokenKind::Operator(OperatorCatalog::Minus),
                            token_start_line,
                            token_start_column,
                        ))
                    }
                }
                '*' => Some(self.create_token(
                    TokenKind::Operator(OperatorCatalog::Multiply),
                    token_start_line,
                    token_start_column,
                )),
                '/' => Some(self.create_token(
                    TokenKind::Operator(OperatorCatalog::Divide),
                    token_start_line,
                    token_start_column,
                )),
                '>' => self.handle_greater(token_start_line, token_start_column),
                '<' => self.handle_less(token_start_line, token_start_column),
                '.' => {
                    Some(self.create_token(TokenKind::Dot, token_start_line, token_start_column))
                }
                '!' => self.handle_not(token_start_line, token_start_column),
                '"' => self.handle_string(token_start_line, token_start_column),
                'a'..='z' | 'A'..='Z' => {
                    self.handle_identifier(ch, token_start_line, token_start_column)
                }
                '0'..='9' => self.handle_number(ch, token_start_line, token_start_column),
                '\n' => {
                    self.line += 1;
                    self.column = 1;
                    continue;
                }
                _ if ch.is_whitespace() => continue,
                _ => Some(self.create_token(
                    TokenKind::Unknown(ch),
                    token_start_line,
                    token_start_column,
                )),
            };
        }
        None
    }

    fn create_token(&self, kind: TokenKind, line: usize, column: usize) -> Token {
        Token { kind, line, column }
    }

    fn handle_equals(&mut self, line: usize, column: usize) -> Option<Token> {
        if let Some(&'=') = self.input.get(self.position) {
            self.position += 1;
            self.column += 1;
            Some(self.create_token(TokenKind::Operator(OperatorCatalog::Equal), line, column))
        } else {
            Some(self.create_token(TokenKind::Equals, line, column))
        }
    }

    fn handle_greater(&mut self, line: usize, column: usize) -> Option<Token> {
        if let Some(&'=') = self.input.get(self.position) {
            self.position += 1;
            self.column += 1;
            Some(self.create_token(
                TokenKind::Operator(OperatorCatalog::GreaterEqual),
                line,
                column,
            ))
        } else {
            Some(self.create_token(
                TokenKind::Operator(OperatorCatalog::GreaterThan),
                line,
                column,
            ))
        }
    }

    fn handle_less(&mut self, line: usize, column: usize) -> Option<Token> {
        if let Some(&'=') = self.input.get(self.position) {
            self.position += 1;
            self.column += 1;
            Some(self.create_token(
                TokenKind::Operator(OperatorCatalog::LessEqual),
                line,
                column,
            ))
        } else {
            Some(self.create_token(TokenKind::Operator(OperatorCatalog::LessThan), line, column))
        }
    }

    fn handle_not(&mut self, line: usize, column: usize) -> Option<Token> {
        if let Some(&'=') = self.input.get(self.position) {
            self.position += 1;
            self.column += 1;
            Some(self.create_token(TokenKind::Operator(OperatorCatalog::NotEqual), line, column))
        } else {
            Some(self.create_token(TokenKind::Unknown('!'), line, column))
        }
    }

    fn handle_identifier(&mut self, ch: char, line: usize, column: usize) -> Option<Token> {
        let mut ident = ch.to_string();
        while let Some(&next) = self.input.get(self.position) {
            if next.is_alphanumeric() {
                ident.push(next);
                self.position += 1;
                self.column += 1;
            } else {
                break;
            }
        }
        let kind = match ident.as_str() {
            "let" => TokenKind::Keyword(KeywordCatalog::Let),
            "function" => TokenKind::Keyword(KeywordCatalog::Function),
            "if" => TokenKind::Keyword(KeywordCatalog::If),
            "else" => TokenKind::Keyword(KeywordCatalog::Else),
            "while" => TokenKind::Keyword(KeywordCatalog::While),
            "return" => TokenKind::Keyword(KeywordCatalog::Return),
            "print" => TokenKind::BuiltIn(BuiltInCatalog::Print),
            _ => TokenKind::Identifier(ident),
        };
        Some(self.create_token(kind, line, column))
    }

    fn handle_number(&mut self, ch: char, line: usize, column: usize) -> Option<Token> {
        let mut num = ch.to_string();
        while let Some(&next) = self.input.get(self.position) {
            if next.is_ascii_digit() {
                num.push(next);
                self.position += 1;
                self.column += 1;
            } else {
                break;
            }
        }
        Some(self.create_token(TokenKind::Number(num.parse().unwrap()), line, column))
    }

    fn handle_string(&mut self, line: usize, column: usize) -> Option<Token> {
        let mut string = String::new();
        let mut escaped = false;

        while let Some(&ch) = self.input.get(self.position) {
            self.position += 1;
            self.column += 1;

            if escaped {
                match ch {
                    'n' => string.push('\n'),
                    't' => string.push('\t'),
                    'r' => string.push('\r'),
                    '\\' => string.push('\\'),
                    '"' => string.push('"'),
                    _ => string.push(ch),
                }
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == '"' {
                break;
            } else {
                if ch == '\n' {
                    self.line += 1;
                    self.column = 1;
                }
                string.push(ch);
            }
        }
        Some(self.create_token(TokenKind::StringLiteral(string), line, column))
    }

    fn get_line_content(&self, line: usize) -> String {
        let mut current_line = String::new();
        let mut current_line_number = 1;
        for &ch in &self.input {
            if current_line_number == line {
                if ch == '\n' {
                    break;
                }
                current_line.push(ch);
            } else if ch == '\n' {
                current_line_number += 1;
            }
        }
        current_line
    }
}

struct Parser {
    tokens: Rc<Vec<Token>>,
    position: usize,
    lexer: Lexer, // 新增 lexer 字段
}

impl Parser {
    fn new(tokens: Rc<Vec<Token>>, lexer: Lexer) -> Self {
        Self {
            tokens,
            position: 0,
            lexer,
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
        match self.tokens.clone().get(self.position).map(|t| &t.kind) {
            Some(TokenKind::Keyword(keyword)) => self.handle_keyword(keyword),
            Some(TokenKind::BuiltIn(name)) if name == &BuiltInCatalog::Print => {
                self.handle_builtin(name)
            }
            _ => self.handle_other(),
        }
    }

    fn handle_keyword(&mut self, keyword: &KeywordCatalog) -> Option<Statement> {
        match keyword {
            KeywordCatalog::Let => self.parse_let(),
            KeywordCatalog::Function => self.parse_function(),
            KeywordCatalog::Return => self.parse_return(),
            KeywordCatalog::If => self.parse_if(),
            KeywordCatalog::While => self.parse_while(),
            _ => {
                self.print_error("unexpected keyword");
                std::process::exit(1);
            }
        }
    }

    fn handle_builtin(&mut self, name: &BuiltInCatalog) -> Option<Statement> {
        self.position += 1;
        if let Some(Token {
            kind: TokenKind::LParen,
            ..
        }) = self.tokens.get(self.position)
        {
            self.position += 1;
            let mut args = Vec::new();
            while self.tokens.get(self.position).map(|t| &t.kind) != Some(&TokenKind::RParen) {
                if let Some(arg) = self.parse_expression() {
                    args.push(arg);
                    if let Some(Token {
                        kind: TokenKind::Comma,
                        ..
                    }) = self.tokens.get(self.position)
                    {
                        self.position += 1; // Skip ','
                    }
                }
            }
            if self.tokens.get(self.position).map(|t| &t.kind) == Some(&TokenKind::RParen) {
                self.position += 1; // Skip ')'
                if let Some(Token {
                    kind: TokenKind::Semicolon,
                    ..
                }) = self.tokens.get(self.position)
                {
                    self.position += 1; // Skip ';'
                    let builtin_name = match name {
                        BuiltInCatalog::Print => "print",
                        // 在这里添加更多的内置函数
                    };
                    return Some(Statement::Expression(Expression::FunctionCall(
                        builtin_name.to_string(),
                        args,
                    )));
                }
            } else {
                self.print_error("unmatched '('");
                std::process::exit(1);
            }
        }
        None
    }

    fn handle_other(&mut self) -> Option<Statement> {
        info!("current token is {:?}", self.tokens.get(self.position));
        if let Some(expr) = self.parse_expression() {
            match expr {
                Expression::UnaryOp(op, expr) if op == "++" || op == "--" => {
                    if let Some(Token {
                        kind: TokenKind::Semicolon,
                        ..
                    }) = self.tokens.get(self.position)
                    {
                        self.position += 1;
                        return Some(Statement::Expression(Expression::UnaryOp(op, expr)));
                    }
                }
                _ => {
                    if let Some(Token {
                        kind: TokenKind::Equals,
                        ..
                    }) = self.tokens.get(self.position)
                    {
                        self.position += 1;
                        if let Some(value_expr) = self.parse_expression() {
                            if let Some(Token {
                                kind: TokenKind::Semicolon,
                                ..
                            }) = self.tokens.get(self.position)
                            {
                                self.position += 1;
                                return Some(Statement::Assignment(expr, value_expr));
                            }
                        }
                    } else if let Some(Token {
                        kind: TokenKind::Semicolon,
                        ..
                    }) = self.tokens.get(self.position)
                    {
                        self.position += 1;
                        return Some(Statement::Expression(expr));
                    }
                }
            }
        }
        None
    }

    fn parse_let(&mut self) -> Option<Statement> {
        self.position += 1;
        if let Some(Token {
            kind: TokenKind::Identifier(name),
            ..
        }) = self.tokens.get(self.position).cloned()
        {
            self.position += 1;
            if let Some(Token {
                kind: TokenKind::Equals,
                ..
            }) = self.tokens.get(self.position)
            {
                self.position += 1;
                if let Some(expr) = self.parse_expression() {
                    if let Some(Token {
                        kind: TokenKind::Semicolon,
                        ..
                    }) = self.tokens.get(self.position)
                    {
                        self.position += 1;
                        return Some(Statement::Let(name, expr));
                    }
                }
            }
        }
        None
    }

    fn parse_function(&mut self) -> Option<Statement> {
        self.position += 1;
        if let Some(Token {
            kind: TokenKind::Identifier(name),
            ..
        }) = self.tokens.get(self.position).cloned()
        {
            self.position += 1;
            if let Some(Token {
                kind: TokenKind::LParen,
                ..
            }) = self.tokens.get(self.position)
            {
                self.position += 1;
                let mut params = Vec::new();
                while let Some(Token {
                    kind: TokenKind::Identifier(param),
                    ..
                }) = self.tokens.get(self.position).cloned()
                {
                    params.push(param);
                    self.position += 1;
                    if let Some(Token {
                        kind: TokenKind::Comma,
                        ..
                    }) = self.tokens.get(self.position)
                    {
                        self.position += 1;
                    } else {
                        break;
                    }
                }
                if let Some(Token {
                    kind: TokenKind::RParen,
                    ..
                }) = self.tokens.get(self.position)
                {
                    self.position += 1;
                    let mut body = Vec::new();
                    if let Some(Token {
                        kind: TokenKind::LBrace,
                        ..
                    }) = self.tokens.get(self.position)
                    {
                        self.position += 1;
                        while self.tokens.get(self.position).map(|t| &t.kind)
                            != Some(&TokenKind::RBrace)
                        {
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
        None
    }

    fn parse_return(&mut self) -> Option<Statement> {
        self.position += 1;
        if let Some(expr) = self.parse_expression() {
            if let Some(Token {
                kind: TokenKind::Semicolon,
                ..
            }) = self.tokens.get(self.position)
            {
                self.position += 1;
                return Some(Statement::Return(expr));
            }
        }
        None
    }

    fn parse_if(&mut self) -> Option<Statement> {
        self.position += 1;
        let condition = if let Some(Token {
            kind: TokenKind::LParen,
            ..
        }) = self.tokens.get(self.position)
        {
            self.position += 1;
            let cond = self.parse_expression();
            if let Some(Token {
                kind: TokenKind::RParen,
                ..
            }) = self.tokens.get(self.position)
            {
                self.position += 1;
            }
            cond
        } else {
            self.parse_expression()
        };

        if let Some(condition) = condition {
            // Parse the consequence block
            let consequence = self.parse_block()?;

            // Check for else
            let alternative = if let Some(Token {
                kind: TokenKind::Keyword(KeywordCatalog::Else),
                ..
            }) = self.tokens.get(self.position)
            {
                self.position += 1;
                // Check if it's an else-if
                if let Some(Token {
                    kind: TokenKind::Keyword(KeywordCatalog::If),
                    ..
                }) = self.tokens.get(self.position)
                {
                    // Parse the else-if as a nested if statement
                    if let Some(Statement::If(cond, cons, alt)) = self.parse_if() {
                        Some(vec![Statement::If(cond, cons, alt)])
                    } else {
                        None
                    }
                } else {
                    // Regular else block
                    self.parse_block()
                }
            } else {
                None
            };

            return Some(Statement::If(condition, consequence, alternative));
        }
        None
    }

    fn parse_while(&mut self) -> Option<Statement> {
        self.position += 1;
        let condition = if let Some(Token {
            kind: TokenKind::LParen,
            ..
        }) = self.tokens.get(self.position)
        {
            self.position += 1;
            let cond = self.parse_expression();
            if let Some(Token {
                kind: TokenKind::RParen,
                ..
            }) = self.tokens.get(self.position)
            {
                self.position += 1;
            }
            cond
        } else {
            self.parse_expression()
        };

        if let Some(condition) = condition {
            if let Some(block) = self.parse_block() {
                return Some(Statement::While(condition, block));
            }
        }
        None
    }

    fn parse_block(&mut self) -> Option<Vec<Statement>> {
        if let Some(Token {
            kind: TokenKind::LBrace,
            ..
        }) = self.tokens.get(self.position)
        {
            self.position += 1;
            let mut statements = Vec::new();
            while let Some(token) = self.tokens.get(self.position) {
                match &token.kind {
                    TokenKind::RBrace => {
                        self.position += 1;
                        return Some(statements);
                    }
                    _ => {
                        if let Some(stmt) = self.parse_statement() {
                            statements.push(stmt);
                        }
                    }
                }
            }
        }
        None
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        let mut left = self.parse_term()?;
        while let Some(Token {
            kind: TokenKind::Operator(op),
            ..
        }) = self.tokens.clone().get(self.position)
        {
            self.position += 1;
            let right = self.parse_term()?;
            left = Expression::BinaryOp(Box::new(left), op.clone(), Box::new(right));
        }
        Some(left)
    }

    fn parse_term(&mut self) -> Option<Expression> {
        match self.tokens.get(self.position).cloned() {
            Some(Token {
                kind: TokenKind::Operator(op),
                ..
            }) if op == OperatorCatalog::Plus || op == OperatorCatalog::Minus => {
                self.position += 1;
                let expr = self.parse_term()?;
                Some(Expression::UnaryOp(format!("{:?}", op), Box::new(expr)))
            }
            Some(Token {
                kind: TokenKind::Number(num),
                ..
            }) => {
                self.position += 1;
                Some(Expression::Number(num))
            }
            Some(Token {
                kind: TokenKind::StringLiteral(string),
                ..
            }) => {
                self.position += 1;
                Some(Expression::StringLiteral(string))
            }
            Some(Token {
                kind: TokenKind::Identifier(name),
                ..
            }) => {
                self.position += 1;
                let mut expr = Expression::Variable(name.clone());
                while let Some(token) = self.tokens.get(self.position).cloned() {
                    match token.kind {
                        TokenKind::Dot => {
                            self.position += 1; // Skip '.'
                            if let Some(Token {
                                kind: TokenKind::Identifier(field),
                                ..
                            }) = self.tokens.get(self.position).cloned()
                            {
                                self.position += 1;
                                expr = Expression::ObjectAccess(Box::new(expr), field);
                            }
                        }
                        TokenKind::LParen => {
                            self.position += 1; // Skip '('
                            let mut args = Vec::new();
                            while self.tokens.get(self.position).map(|t| &t.kind)
                                != Some(&TokenKind::RParen)
                            {
                                if let Some(arg) = self.parse_expression() {
                                    args.push(arg);
                                    if let Some(Token {
                                        kind: TokenKind::Comma,
                                        ..
                                    }) = self.tokens.get(self.position)
                                    {
                                        self.position += 1; // Skip ','
                                    }
                                }
                            }
                            if self.tokens.get(self.position).map(|t| &t.kind)
                                == Some(&TokenKind::RParen)
                            {
                                self.position += 1; // Skip ')'
                                expr = Expression::FunctionCall(name.clone(), args);
                            } else {
                                self.print_error("unmatched '('");
                                std::process::exit(1);
                            }
                        }
                        _ => break,
                    }
                }
                Some(expr)
            }
            Some(Token {
                kind: TokenKind::LBrace,
                ..
            }) => {
                self.position += 1; // Skip '{'
                let mut fields = HashMap::new();
                while self.tokens.get(self.position).map(|t| &t.kind) != Some(&TokenKind::RBrace) {
                    if let Some(Token {
                        kind: TokenKind::Identifier(key),
                        ..
                    }) = self.tokens.get(self.position).cloned()
                    {
                        self.position += 1;
                        if let Some(Token {
                            kind: TokenKind::Colon,
                            ..
                        }) = self.tokens.get(self.position)
                        {
                            self.position += 1;
                            if let Some(value) = self.parse_expression() {
                                fields.insert(key, value);
                                if let Some(Token {
                                    kind: TokenKind::Comma,
                                    ..
                                }) = self.tokens.get(self.position)
                                {
                                    self.position += 1;
                                }
                            }
                        }
                    }
                }
                if self.tokens.get(self.position).map(|t| &t.kind) == Some(&TokenKind::RBrace) {
                    self.position += 1; // Skip '}'
                    Some(Expression::Object(fields))
                } else {
                    self.print_error("unmatched '{'");
                    std::process::exit(1);
                }
            }
            Some(Token {
                kind: TokenKind::LParen,
                ..
            }) => {
                self.position += 1; // Skip '('
                let expr = self.parse_expression();
                if self.tokens.get(self.position).map(|t| &t.kind) == Some(&TokenKind::RParen) {
                    self.position += 1; // Skip ')'
                    expr
                } else {
                    self.print_error("unmatched '('");
                    std::process::exit(1);
                }
            }
            _ => {
                self.print_error("Error parsing expression");
                std::process::exit(1);
            }
        }
    }

    fn print_error(&self, message: &str) {
        let token = self.tokens.get(self.position).unwrap();
        let line = token.line;
        let column = token.column;
        let line_content = self.lexer.get_line_content(line);
        eprintln!(
            "Error: {} at line {}, column {}\n{}\n{}",
            message,
            line,
            column,
            line_content,
            " ".repeat(column - 1) + "^"
        );
    }
}

struct Interpreter {
    variables: HashMap<String, Value>,
    string_literals: HashMap<String, String>, // 新增 string_literals 字段
}

impl Interpreter {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            string_literals: HashMap::new(), // 初始化 string_literals
        }
    }

    fn execute_statements(&mut self, statements: Vec<Statement>) -> Option<Value> {
        let mut last_value = None;
        for statement in statements {
            last_value = self.execute_statement(statement);
            // 不要在这里直接返回，让所有语句都能执行
        }
        last_value
    }

    fn execute_statement(&mut self, statement: Statement) -> Option<Value> {
        info!("Executing: {:?}", statement);
        match statement {
            Statement::Let(name, expr) => {
                let value = self.evaluate_expression(expr);
                match &value {
                    Value::StringLiteral(ref unique_name) => {
                        // 存储字符串字面量到变量中
                        self.variables.insert(name, value.clone());
                        // 将实际字符串内容存储到 string_literals
                        if !self.string_literals.contains_key(unique_name) {
                            if let Some(str_value) = self.string_literals.get(unique_name) {
                                self.string_literals
                                    .insert(unique_name.clone(), str_value.clone());
                            }
                        }
                    }
                    _ => {
                        self.variables.insert(name, value);
                    }
                }
                None
            }
            Statement::FunctionDef(name, params, body) => {
                self.variables.insert(name, Value::Function(params, body));
                None
            }
            Statement::Expression(Expression::FunctionCall(name, args)) if name == "print" => {
                self.handle_builtin_print_call(args);
                None
            }
            Statement::Expression(expr) => {
                let value = self.evaluate_expression(expr.clone());
                if let Value::StringLiteral(ref unique_name) = value {
                    if let Expression::Variable(ref name) = expr {
                        self.variables
                            .insert(name.clone(), Value::StringLiteral(unique_name.to_string()));
                    }
                }
                None
            }
            Statement::Assignment(left, right) => {
                let value = self.evaluate_expression(right);
                match left {
                    Expression::Variable(name) => {
                        self.variables.insert(name, value);
                    }
                    Expression::ObjectAccess(object_expr, key) => {
                        if let Value::Object(mut map) = self.evaluate_expression(*object_expr) {
                            map.insert(key, value);
                        } else {
                            panic!("Assignment to a non-object value");
                        }
                    }
                    _ => panic!("Invalid assignment target"),
                }
                None
            }
            Statement::If(condition, consequence, alternative) => {
                if let Value::Number(cond) = self.evaluate_expression(condition) {
                    if cond != 0 {
                        self.execute_statements(consequence)
                    } else if let Some(alt) = alternative {
                        self.execute_statements(alt)
                    } else {
                        None
                    }
                } else {
                    panic!("Condition must be a number");
                }
            }
            Statement::Return(expr) => {
                let result = self.evaluate_expression(expr.clone());
                // 对于返回值是字符串的情况，确保在返回前字符串已经被正确保存
                if let Value::StringLiteral(ref unique_name) = result {
                    if let Expression::Variable(name) = expr {
                        self.variables
                            .insert(name, Value::StringLiteral(unique_name.to_string()));
                    }
                }
                Some(result)
            }
            Statement::While(condition, body) => {
                let mut last_value = None;
                while let Value::Number(cond) = self.evaluate_expression(condition.clone()) {
                    if cond == 0 {
                        break;
                    }
                    last_value = self.execute_statements(body.clone());
                }
                last_value
            }
        }
    }

    fn handle_builtin_print_call(&mut self, args: Vec<Expression>) -> Value {
        if args.is_empty() {
            println!();
            return Value::Number(0);
        }

        let mut values = Vec::new();
        for arg in args {
            let value = self.evaluate_expression(arg.clone());
            let str_value = match value {
                Value::Number(num) => num.to_string(),
                Value::StringLiteral(name) => self.string_literals.get(&name).unwrap().clone(),
                Value::Object(map) => {
                    let fields: Vec<String> = map
                        .iter()
                        .map(|(key, value)| format!("{}: {}", key, value))
                        .collect();
                    format!("{{{}}}", fields.join(", "))
                }
                Value::Function(params, _) => format!("function({})", params.join(", ")),
            };
            values.push(str_value);
        }

        // 如果第一个参数包含 {} 占位符，则按照格式化字符串处理
        if values[0].contains("{}") {
            let mut format_string = values[0].clone();
            for value in values.iter().skip(1) {
                if let Some(pos) = format_string.find("{}") {
                    format_string.replace_range(pos..pos + 2, value);
                }
            }
            println!("{}", format_string);
        } else {
            // 否则直接打印所有参数
            println!("{}", values.join(" "));
        }
        Value::Number(0)
    }

    fn generate_unique_name(&mut self, string: &str) -> String {
        let mut hasher = DefaultHasher::new();
        string.hash(&mut hasher);
        let hash = hasher.finish();
        let unique_name = format!("__str_{}__", hash);
        self.string_literals
            .entry(unique_name.clone())
            .or_insert(string.to_string());
        unique_name
    }

    fn evaluate_expression(&mut self, expr: Expression) -> Value {
        match expr {
            Expression::Number(num) => Value::Number(num),
            Expression::StringLiteral(string) => {
                info!("evaluate string: {}", string);
                let unique_name = self.generate_unique_name(&string);
                info!("unique_name: {}", unique_name);
                Value::StringLiteral(unique_name)
            }
            Expression::Variable(name) => {
                info!("evaluate variable: {}", name);
                self.variables
                    .get(&name)
                    .cloned()
                    .unwrap_or_else(|| panic!("Variable {} not found", name))
            }
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
                        let result = match op {
                            OperatorCatalog::Plus => left_num + right_num,
                            OperatorCatalog::Minus => left_num - right_num,
                            OperatorCatalog::Multiply => left_num * right_num,
                            OperatorCatalog::Divide => left_num / right_num,
                            OperatorCatalog::GreaterThan => (left_num > right_num) as i32,
                            OperatorCatalog::LessThan => (left_num < right_num) as i32,
                            OperatorCatalog::GreaterEqual => (left_num >= right_num) as i32,
                            OperatorCatalog::LessEqual => (left_num <= right_num) as i32,
                            OperatorCatalog::Equal => (left_num == right_num) as i32,
                            OperatorCatalog::NotEqual => (left_num != right_num) as i32,
                        };
                        Value::Number(result)
                    }
                    (Value::StringLiteral(left_str), Value::StringLiteral(right_str)) => {
                        if let OperatorCatalog::Plus = op {
                            let left_real_str = self.string_literals.get(&left_str).unwrap();
                            let right_real_str = self.string_literals.get(&right_str).unwrap();
                            let combined_str = format!("{}{}", left_real_str, right_real_str);
                            let unique_name = self.generate_unique_name(&combined_str);
                            Value::StringLiteral(unique_name)
                        } else {
                            panic!("Unsupported operator for strings: {:?}", op)
                        }
                    }
                    _ => panic!("Type error in binary operation"),
                }
            }
            Expression::FunctionCall(name, args) => {
                if name == "print" {
                    // 处理格式化字符串打印
                    if let Some(first_arg) = args.first() {
                        let format_value = self.evaluate_expression(first_arg.clone());
                        if let Value::StringLiteral(format_name) = format_value {
                            // 获取实际的格式化字符串
                            let mut format_string =
                                self.string_literals.get(&format_name).unwrap().clone();

                            // 处理剩余参数
                            for arg in args.iter().skip(1) {
                                let value = self.evaluate_expression(arg.clone());
                                let replacement = match value {
                                    Value::Number(num) => num.to_string(),
                                    Value::StringLiteral(name) => {
                                        self.string_literals.get(&name).unwrap().clone()
                                    }
                                    Value::Object(map) => {
                                        let fields: Vec<String> = map
                                            .iter()
                                            .map(|(key, value)| format!("{}: {}", key, value))
                                            .collect();
                                        format!("{{{}}}", fields.join(", "))
                                    }
                                    Value::Function(params, _) => {
                                        format!("function({})", params.join(", "))
                                    }
                                };

                                // 替换第一个 {} 占位符
                                if let Some(pos) = format_string.find("{}") {
                                    format_string.replace_range(pos..pos + 2, &replacement);
                                }
                            }
                            println!("{}", format_string);
                        } else {
                            // 如果第一个参数不是字符串,则按普通方式打印所有参数
                            for arg in args {
                                let value = self.evaluate_expression(arg);
                                match value {
                                    Value::Number(num) => print!("{} ", num),
                                    Value::StringLiteral(name) => {
                                        print!("{} ", self.string_literals.get(&name).unwrap());
                                    }
                                    Value::Object(map) => {
                                        let fields: Vec<String> = map
                                            .iter()
                                            .map(|(key, value)| format!("{}: {}", key, value))
                                            .collect();
                                        print!("{{{}}} ", fields.join(", "));
                                    }
                                    Value::Function(params, _) => {
                                        print!("function({}) ", params.join(", "));
                                    }
                                }
                            }
                            println!();
                        }
                    }
                    Value::Number(0)
                } else {
                    let func = self.variables.get(&name).cloned().unwrap();
                    if let Value::Function(params, body) = func {
                        let mut local_context = self.variables.clone();
                        for (param, arg) in params.iter().zip(args) {
                            let arg_value = self.evaluate_expression(arg);
                            local_context.insert(param.clone(), arg_value);
                        }
                        let mut interpreter = Interpreter {
                            variables: local_context,
                            string_literals: self.string_literals.clone(), // 传递 string_literals
                        };

                        // 获取函数返回值
                        let result = interpreter
                            .execute_statements(body)
                            .unwrap_or(Value::Number(0));

                        // 如果返回值是字符串，需要将其复制到当前解释器的 string_literals 中
                        if let Value::StringLiteral(unique_name) = &result {
                            if let Some(str_value) = interpreter.string_literals.get(unique_name) {
                                self.string_literals
                                    .insert(unique_name.clone(), str_value.clone());
                            }
                        }

                        result
                    } else {
                        panic!("Function {} not found", name);
                    }
                }
            }
            Expression::UnaryOp(op, expr) => match &*expr {
                Expression::Variable(name) => {
                    if let Some(Value::Number(num)) = self.variables.get(name) {
                        let new_value = match op.as_str() {
                            "++" => num + 1,
                            "--" => num - 1,
                            _ => panic!("Unsupported unary operator: {}", op),
                        };
                        self.variables
                            .insert(name.clone(), Value::Number(new_value));
                        Value::Number(new_value)
                    } else {
                        panic!("Cannot apply {} to non-numeric variable {}", op, name)
                    }
                }
                _ => panic!("Cannot apply {} to non-variable expression", op),
            },
        }
    }
}

fn read_file(path: &Path) -> io::Result<String> {
    fs::read_to_string(path)
}

fn is_balanced(input: &str) -> bool {
    let mut stack = Vec::new();
    let mut in_string = false;

    for c in input.chars() {
        match c {
            '"' => in_string = !in_string,
            '{' | '(' if !in_string => stack.push(c),
            '}' if !in_string => {
                if stack.pop() != Some('{') {
                    return false;
                }
            }
            ')' if !in_string => {
                if stack.pop() != Some('(') {
                    return false;
                }
            }
            _ => {}
        }
    }
    !in_string && stack.is_empty()
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Interactive mode. Enter code or 'exit()' to quit.");
        println!("Use Ctrl+D to exit. Multi-line input is supported.");
        let mut interpreter = Interpreter::new();

        'repl: loop {
            let mut input = String::new();
            let mut is_first_line = true;

            loop {
                if is_first_line {
                    print!("> ");
                } else {
                    print!("... ");
                }
                io::stdout().flush().unwrap();

                let mut line = String::new();
                match io::stdin().read_line(&mut line) {
                    Ok(0) => {
                        println!("\nGoodbye!"); // Ctrl+D was pressed
                        break 'repl;
                    }
                    Ok(_) => {
                        if line.trim() == "exit()" {
                            println!("Goodbye!");
                            break 'repl;
                        }

                        // 只在第一行是空白行时特殊处理
                        if is_first_line && line.trim().is_empty() {
                            break;
                        }

                        input.push_str(&line);

                        // 如果输入平衡了，就执行代码
                        if is_balanced(&input) {
                            // 确保输入不是空的
                            if !input.trim().is_empty() {
                                let mut lexer = Lexer::new(&input);
                                let tokens: Rc<Vec<Token>> =
                                    Rc::new(std::iter::from_fn(|| lexer.next_token()).collect());
                                let mut parser = Parser::new(tokens, lexer);
                                let statements = parser.parse_statements();
                                interpreter.execute_statements(statements);
                            }
                            break;
                        }

                        is_first_line = false;
                    }
                    Err(e) => {
                        eprintln!("Error reading input: {}", e);
                        break;
                    }
                }
            }
        }
        return;
    }

    // 从文件读取代码
    let file_path = Path::new(&args[1]);
    match read_file(file_path) {
        Ok(code) => {
            let mut lexer = Lexer::new(&code);
            let tokens: Rc<Vec<Token>> =
                Rc::new(std::iter::from_fn(|| lexer.next_token()).collect());
            let mut parser = Parser::new(tokens, lexer);
            let statements = parser.parse_statements();
            let mut interpreter = Interpreter::new();
            interpreter.execute_statements(statements);
        }
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            std::process::exit(1);
        }
    }
}
