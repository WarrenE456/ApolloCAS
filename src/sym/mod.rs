pub extern crate num_bigint;

use num_bigint::{BigInt, Sign};

use std::str::FromStr;

#[derive(Clone, Debug)]
pub enum SymExpr {
    Z(BigInt),
    Sum(Sum),
    Product(Product),
    Pow(Pow),
    Symbol(String),
}

impl SymExpr {
    pub fn z_from_i64(n: i64) -> SymExpr {
        let (sign, n) = if n >= 0 {
            (Sign::Plus, n as u64)
        } else {
            (Sign::Minus, -n as u64)
        };
        SymExpr::Z(BigInt::new(sign, decompose_u64(n)))
    }
    pub fn z_from_string(s: &String) -> SymExpr {
        SymExpr::Z(BigInt::from_str(s.as_str()).unwrap())
    }
    pub fn simplify(self) -> SymExpr {
        match self {
            SymExpr::Z(_) => self,
            SymExpr::Symbol(_) => self,
            SymExpr::Sum(s) => s.simplify(),
            SymExpr::Product(t) => t.simplify(),
            SymExpr::Pow(_) => todo!(),
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            SymExpr::Symbol(s) => s.clone(),
            SymExpr::Z(n) => n.to_string(),
            SymExpr::Sum(s) => s.to_string(),
            SymExpr::Product(t) => t.to_string(),
            SymExpr::Pow(_) => todo!(),
        }
    }
    pub fn kind_name(&self) -> String {
        match self {
            SymExpr::Symbol(_) => String::from("symbol"),
            SymExpr::Z(_) => String::from("Z"),
            SymExpr::Sum(_) => String::from("sum of expressions"),
            SymExpr::Product(_) => String::from("product of expressions"),
            SymExpr::Pow(_)  => String::from("exponential"),
        }
    }
    pub fn distribute(self, sum: Sum) -> Sum {
        let mut terms = Vec::new();
        for term in sum.terms.iter() {
            let factors = vec![self.clone(), term.clone()];
            let term = SymExpr::Product(Product{ factors }.flatten());
            terms.push(term);
        }
        Sum { terms }
    }
}

#[derive(Clone, Debug)]
pub struct Sum {
    pub terms: Vec<SymExpr>,
}

impl Sum {
    pub fn simplify(self) -> SymExpr {
        let mut n = BigInt::ZERO;
        let mut exprs = Vec::new();
        for expr in self.terms {
            match expr.simplify() {
                SymExpr::Z(i) => n += i,
                other => exprs.push(other),
            }
        }
        return if exprs.len() == 0 {
            SymExpr::Z(n)
        } else {
            exprs.push(SymExpr::Z(n));
            SymExpr::Sum(Sum{ terms: exprs })
        };
    }
    pub fn to_string(&self) -> String {
        self.terms
            .iter().map(|expr| expr.to_string())
            .collect::<Vec<_>>()
            .join(&String::from(" + "))
    }
    pub fn distribute(self, other: Sum) -> Sum {
        let mut terms = Vec::new();
        for a in self.terms.iter() {
            let term = a.clone().distribute(other.clone());
            terms.push(SymExpr::Sum(term));
        }
        Sum{ terms }
    }
}

#[derive(Clone, Debug)]
pub struct Product {
    pub factors: Vec<SymExpr>,
}

impl Product {
    fn to_string(&self) -> String {
        self.factors
            .iter().map(|f| f.to_string())
            .collect::<Vec<String>>()
            .join(&String::from(" "))
    }
    fn distribute_aux(mut self) -> SymExpr {
        assert!(self.factors.len() != 0);
        self.factors.reverse();
        let mut expr = self.factors.pop().unwrap();
        while let Some(next_expr) = self.factors.pop() {
            expr = match (expr, next_expr) {
                (SymExpr::Sum(s), SymExpr::Sum(ss)) => {
                    SymExpr::Sum(s.distribute(ss))
                }
                (SymExpr::Sum(s), other) |
                (other, SymExpr::Sum(s)) => {
                    SymExpr::Sum(other.distribute(s))
                }
                (other1, other2) => {
                    let factors = vec![other1, other2];
                    SymExpr::Product(Product{ factors }.flatten())
                }
            }
        };
        expr
    }
    fn distribute(self) -> SymExpr {
        return if self.factors.len() < 2 {
            SymExpr::Product(self)
        } else {
            self.distribute_aux()
        };
    }
    fn flatten(self) -> Product {
        let mut flattened = Vec::new();
        for factor in self.factors {
            match factor {
                SymExpr::Product(t) => {
                    for factor in t.factors {
                        flattened.push(factor);
                    }
                }
                _ => flattened.push(factor),
            }
        }
        Product { factors: flattened }
    }
    fn collect_factors(self) -> Product {
        self // TODO
    }
    fn simplify_each(self) -> Product {
        let factors = self.factors.into_iter()
            .map(|expr| expr.simplify())
            .collect::<Vec<SymExpr>>();
        Product{ factors }
    }
    pub fn simplify(self) -> SymExpr {
        match self
            .simplify_each()
            .flatten()
            .distribute()
        {
            SymExpr::Product(t) => SymExpr::Product(t.collect_factors()),
            SymExpr::Sum(s) => {
                let terms = s.terms
                    .into_iter()
                    .map(|term| match  term {
                        SymExpr::Product(t) => SymExpr::Product(t.collect_factors()),
                        SymExpr::Sum(s) => SymExpr::Sum(s), // TODO remove
                        other => panic!("!{}", other.kind_name()), // TODO unreachable
                    })
                    .collect::<Vec<SymExpr>>();
                SymExpr::Sum(Sum { terms })
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Pow {
    pub base: Box<SymExpr>,
    pub exp: Box<SymExpr>,
}

impl Pow {
}

fn decompose_u64(n: u64) -> Vec<u32> {
    let mut n = n;
    let mut decomp = vec![];
    let mask: u64 = 0xffffffff;
    while n > 0 {
        decomp.push((n & mask) as u32);
        n >>= 32;
    }
    decomp
}
