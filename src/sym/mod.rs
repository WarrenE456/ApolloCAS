pub extern crate num_bigint;

use num_bigint::{BigInt, Sign};

use std::str::FromStr;
use std::collections::HashMap;
use std::cmp::Ordering;

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
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
    pub fn seperate_coef(self) -> (BigInt, SymExpr) {
        match self {
            SymExpr::Product(p) => p.seperate_coef(),
            SymExpr::Z(z) => (z, SymExpr::Z(BigInt::ZERO + 1)),
            other => (BigInt::ZERO + 1, other),
        }
    }
    fn remove_coef(&self) -> SymExpr {
        // if let SymExpr::Product(p) = self {
        //     let (_, non_coef) = p.seperate_coef();
        // }
        todo!()
    }
    /*
    Takes two symbolic expressions, both of which must be simplified
    Returns Ordering::Less of a comes before b,
            Order::Equal if a and b are the same order
            and Order::Greater if a comes after b,
    Rules:
        Expression kinds ordered from first to last,
            Z < Symbol < Sum < Product = Pow
        If expressions are of the same kind...
        Z:
            z_i comes before z_k if z_i < z_k
            this only really matters for ordering pow
        Sym:
            symbol a comes before symbol b if len(a) < len(b)
            if len(a) = len(b), then ordering lexographically
        Sum:
            if len(sum_i) < len(sum_k), sum1 comes first
            otherwise, based off of order of last term.
            If these terms are equal we look at the next term,
            then then the next and so on
        Product & Pow:
            a comes before b if sum_deg(a) comes after sum_deg(b)
            otherwise a comes before b if len(a.products) > len(b.products)
            otherwise we combare the base of each factor
    */
    fn order_product_power(pro: &Product, pow: &Pow) -> Ordering {
        todo!()
    }
    pub fn order (&self, other: &SymExpr) -> Ordering {
        match self {
            SymExpr::Z(z) => match other {
                SymExpr::Z(zz) => z.cmp(zz),
                _ => Ordering::Less,
            }
            SymExpr::Symbol(s) => match other {
                SymExpr::Z(_) => Ordering::Greater,
                SymExpr::Symbol(ss) => string_order(&s, &ss),
                _ => Ordering::Less,
            },
            SymExpr::Sum(s) => match other {
                SymExpr::Z(_) | SymExpr::Symbol(_) => Ordering::Greater,
                SymExpr::Sum(ss) => s.order(ss),
                _ => Ordering::Less,
            }
            SymExpr::Product(p) => match other {
                SymExpr::Z(_) | SymExpr::Symbol(_) | SymExpr::Sum(_) => Ordering::Greater,
                SymExpr::Product(pp) => todo!(),
                SymExpr::Pow(pp) => Self::order_product_power(p, pp),
            }
            SymExpr::Pow(p) => match other {
                SymExpr::Z(_) | SymExpr::Symbol(_) | SymExpr::Sum(_) => Ordering::Greater,
                SymExpr::Product(pp) => Self::order_product_power(pp, p),
                SymExpr::Pow(pp) => p.order(pp),
            }
        }
    }
}

fn string_order(s: &String, ss: &String) -> Ordering {
    if s.len() == ss.len() {
        s.cmp(&ss)
    }
    else if s.len() < ss.len() {
        Ordering::Less
    } else {
        Ordering::Greater
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Sum {
    pub terms: Vec<SymExpr>,
}

impl Sum {
    fn simplify_each(self) -> Sum {
        let terms = self.terms
            .into_iter().map(|expr| expr.simplify())
            .collect::<Vec<SymExpr>>(); 
        Sum { terms }
    }
    fn flatten(self) -> Sum {
        let mut terms = Vec::new();
        for term in self.terms {
            match term {
                SymExpr::Sum(s) => for term in s.terms {
                    terms.push(term)
                }
                _ => terms.push(term)
            }
        }
        Sum { terms }
    }
    fn order_terms(self) -> Sum {
        let mut terms = self.terms;
        terms.sort_by(|a, b| b.order(a));
        Sum { terms }
    }
    fn combine_like_terms(self) -> SymExpr {
        let mut mp = HashMap::new();
        for term in self.terms {
            let (coef, term) = term.seperate_coef();
            *mp.entry(term).or_insert(BigInt::ZERO) += coef;
        }
        let mut terms = Vec::new();
        for (term, coef) in mp {
            if coef == BigInt::ZERO {
                continue;
            }
            let term = if coef == BigInt::from(1) {
                term
            } else if let SymExpr::Z(z) = &term {
                if *z == BigInt::from(1) {
                    SymExpr::Z(coef)
                } else {
                    let factors = vec![SymExpr::Z(coef), term];
                    SymExpr::Product(Product{ factors }.flatten())
                }
            }else {
                let factors = vec![SymExpr::Z(coef), term];
                SymExpr::Product(Product{ factors }.flatten())
            };
            terms.push(term);
        }
        if terms.len() > 1 {
            SymExpr::Sum(Sum{ terms })
        } else {
            terms[0].clone()
        }
    }
    pub fn simplify(self) -> SymExpr {
        self
            .simplify_each()
            .flatten()
            .order_terms()
            .combine_like_terms()
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
    pub fn order(&self, other: &Sum) -> Ordering {
        if self.terms.len() < other.terms.len() {
            return Ordering::Less;
        }
        else if other.terms.len() < self.terms.len() {
            return Ordering::Greater;
        }

        for (t1, t2) in Iterator::zip(self.terms.iter().rev(), other.terms.iter().rev()) {
            if t1 == t2 {
                continue;
            } else {
                return t1.order(t2)
            }
        }

        Ordering::Equal
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
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
    fn order_factors(self) -> Product {
        let mut factors = self.factors;
        factors.sort_by(|a, b| a.order(b));
        Product { factors }
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
            SymExpr::Product(t) => {
                SymExpr::Product(
                    t.collect_factors()
                        .order_factors()
                )
            }
            SymExpr::Sum(s) => {
                let terms = s.terms
                    .into_iter()
                    .map(|term| match  term {
                        SymExpr::Product(t) => SymExpr::Product(t.collect_factors().order_factors()),
                        SymExpr::Sum(s) => SymExpr::Sum(s), // TODO remove
                        other => panic!("!{}", other.kind_name()), // TODO unreachable
                    })
                    .collect::<Vec<SymExpr>>();
                SymExpr::Sum(Sum { terms })
            }
            _ => unreachable!(),
        }
    }
    pub fn seperate_coef(self) -> (BigInt, SymExpr) {
        let mut coef = BigInt::ZERO + 1;
        let mut factors = Vec::new();
        for factor in self.factors {
            match factor {
                SymExpr::Z(n) => coef *= n,
                other => factors.push(other),
            }
        }
        if factors.len() == 1 {
            (coef, factors[0].clone())
        } else {
            (coef, SymExpr::Product(Product{ factors }))
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Pow {
    pub base: Box<SymExpr>,
    pub exp: Box<SymExpr>,
}

impl Pow {
    pub fn order(&self, other: &Pow) -> Ordering {
        match self.exp.order(&other.exp) {
            Ordering::Equal => self.base.order(&other.base),
            order => order,
        }
    }
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
