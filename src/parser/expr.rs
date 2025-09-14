use crate::sym::num_bigint::{BigInt, BigUint, Sign};
use crate::sym::num_traits::One;

use crate::scanner::tok::{Tok, TokType};
use crate::sym::*;

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Tok),
    Arr(Vec<Expr>),
    Group(Box<Expr>),
    Binary(Binary),
    Concat(Concat),
    Negate(Negate),
    Call(Call),
    Exp(Exp),
    Comp(Comp),
    Or(Or),
    And(And),
    Index(Index),
    Sym(Tok, Box<Expr>)
}

impl Expr {
    pub fn to_string(&self) -> String {
        use Expr::*;
        match self {
            Literal(l) => l.lexeme.clone(),
            Group(e) => format!("({})", (*e).to_string()),
            Binary(b) => {
                b.operands
                    .iter().enumerate()
                    .filter(|(i, _)| *i != 0)
                    .fold(b.operands.first().unwrap().to_string(), |acc, (i, v)| {
                        format!("{} {} {}", acc, b.operators[i - 1].lexeme, v.to_string())
                    })
            },
            Concat(c) => format!("{} {} {}", c.l.to_string(), c.op.lexeme, c.r.to_string()),
            Negate(n) => format!("-{}", n.value.to_string()),
            Call(c) => {
                let args = c.args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", c.identifier.lexeme, args)
            }
            Exp(e) => format!("{}^{}", e.base.to_string(), e.power.to_string()),
            Comp(c) => {
                c.operands
                    .iter().enumerate()
                    .filter(|(i, _)| *i != 0)
                    .fold(c.operands.first().unwrap().to_string(), |acc, (i, v)| {
                        format!("{} {} {}", acc, c.operators[i - 1].lexeme, v.to_string())
                    })
            }
            And(a) => format!("{} and {}", a.right.to_string(), a.left.to_string()),
            Or(o) => format!("{} or {}", o.right.to_string(), o.left.to_string()),
            Arr(a) => format!("[{}]", a.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", ")),
            Index(i) => i.expr.to_string() + &i.index.to_string(),
            Sym(_, e) => e.to_string(),
        }
    }
    pub fn kind_name(&self) -> String {
        use TokType::*;
        match self {
            Expr::Literal(_) => String::from("literal"),
            Expr::Arr(_) => String::from("array"),
            Expr::Group(_) => String::from("group"),
            Expr::Concat(_) => String::from("string concatenation"),
            Expr::Negate(_) => String::from("negation"),
            Expr::Call(_) => String::from("call"),
            Expr::Exp(_) => String::from("exponent"),
            Expr::Comp(_) => String::from("(in)equality"),
            Expr::Or(_) => String::from("logical or"),
            Expr::And(_) => String::from("logical and"),
            Expr::Index(_) => String::from("index"),
            Expr::Binary(b) => match b.operators[0].t {
                Plus => String::from("addition"),
                Star => String::from("multiplication"),
                Slash => String::from("division"),
                Minus => String::from("subtraction"),
                _ => format!("binary ('{}')", b.operators[0].lexeme),
            },
            Expr::Sym(_, e) => e.kind_name(),
        }
    }
    pub fn to_sym(&self) -> Result<SymExpr, String> {
        use crate::scanner::tok::TokType;

        match self {
            Expr::Literal(tok) => match tok.t {
                TokType::Int => Ok(SymExpr::z_from_string(&tok.lexeme)),
                TokType::Float => todo!(), // TODO Convert to Q
                TokType::Identifier => Ok(SymExpr::Symbol(tok.lexeme.clone())),
                _ => todo!(), // TODO: Print error message
            }
            Expr::Binary(b) => b.to_sym(),
            Expr::Group(g) => g.to_sym(),
            Expr::Sym(_, s) => s.to_sym(),
            Expr::Negate(n) => n.to_sym(),
            _ => Err(format!("Cannot convert {} expression to a symbolic expression.", self.kind_name()))
 // TODO
        }
    }
}

#[derive(Debug, Clone)]
pub struct Exp {
    pub base: Box<Expr>,
    pub power: Box<Expr>,
    pub op: Tok,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operators: Vec<Tok>,
    pub operands: Vec<Expr>,
}

impl Binary {
    pub fn new(operators: Vec<Tok>, operands: Vec<Expr>) -> Self {
        Self { operators, operands }
    }
    fn sum_to_sym(&self) -> Result<SymExpr, String> {
        let mut terms = Vec::new();
        let op = self.operators[0].t;
        for (i,operand) in self.operands.iter().enumerate() {
            let expr = if op == TokType::Minus && i > 0 {
                Negate::negate_sym_expr(operand.to_sym()?)
            } else {
                operand.to_sym()?
            };
            terms.push(expr); 
        }
        Ok(SymExpr::Sum(Sum { terms }))
    }
    fn prod_to_sym(&self) -> Result<SymExpr, String> {
        let mut factors = Vec::new();
        for operand in self.operands.iter() {
            factors.push(operand.to_sym()?); 
        }
        Ok(SymExpr::Product(Product { factors }))
    }
    pub fn to_sym(&self) -> Result<SymExpr, String> {
        use crate::scanner::tok::TokType;
        let opt = self.operators[0].t;
        match opt {
            TokType::Plus | TokType::Minus => self.sum_to_sym(),
            TokType::Star => self.prod_to_sym(),
            TokType::Slash => todo!(),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Negate {
    pub minus: Tok,
    pub value: Box<Expr>
}

impl Negate {
    pub fn negate_sym_expr(symexpr: SymExpr) -> SymExpr {
        let neg_one = SymExpr::Z(BigInt::from_biguint(Sign::Minus, BigUint::one()));
        SymExpr::mul(neg_one, symexpr)
    }
    pub fn to_sym(&self) -> Result<SymExpr, String> {
        self.value.to_sym().map(|v| Negate::negate_sym_expr(v))
    }
}

#[derive(Debug, Clone)]
pub struct Concat {
    pub l: Box<Expr>,
    pub op: Tok,
    pub r: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub identifier: Tok,
    pub args: Vec<Expr>,
    pub rparen: Tok,
}

#[derive(Debug, Clone)]
pub struct Comp {
    pub operators: Vec<Tok>,
    pub operands: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Or {
    pub op: Tok,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct And {
    pub op: Tok,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Index {
    pub expr: Box<Expr>,
    pub lb: Tok,
    pub index: Box<Expr>,
    pub rb: Tok,
}

