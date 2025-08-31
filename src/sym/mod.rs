pub extern crate num_bigint;
extern crate num_traits;

use num_bigint::{BigInt, Sign};
use num_traits::One;

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
            SymExpr::Product(p) => p.simplify(),
            SymExpr::Pow(_) => todo!(),
        }
    }
    // TODO parenthesis when child op has lower precedence than parent op
    pub fn to_string(&self) -> String {
        match self {
            SymExpr::Symbol(s) => s.clone(),
            SymExpr::Z(n) => n.to_string(),
            SymExpr::Sum(s) => s.to_string(),
            SymExpr::Product(t) => t.to_string(),
            SymExpr::Pow(p) => p.to_string(),
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
            let term = self.clone().mul(term.clone());
            terms.push(term);
        }
        Sum { terms }
    }
    pub fn seperate_coef(self) -> (BigInt, SymExpr) {
        match self {
            SymExpr::Product(p) => p.seperate_coef(),
            SymExpr::Z(z) => (z, SymExpr::Z(BigInt::one())),
            other => (BigInt::one(), other),
        }
    }
    pub fn add(self, other: SymExpr) -> SymExpr {
        match (self, other) {
            (SymExpr::Z(a), SymExpr::Z(b)) => SymExpr::Z(a + b),
            (a, b) => SymExpr::Sum(Sum { terms: vec![a, b] }.flatten()),
        }
    }
    pub fn mul(self, other: SymExpr) -> SymExpr {
        match (self, other) {
            (SymExpr::Z(a), SymExpr::Z(b)) => SymExpr::Z(a * b),
            (a, b) => SymExpr::Product(Product { factors: vec![a, b] }.flatten()),
        }
    }
    /*
    Takes two symbolic expressions, both of which must be simplified
    Based of of ordering in product.
    Ordering should be reversed in sums.
    Returns Ordering::Less of a comes before b,
            and Order::Greater if a comes after b,
            Order::Equal if a and b are the same order
            Order::Equal should only be used for a, b in Z because otherwise order could be ambiguous
    Rules:
        Certain expression kinds are always less than others:
            Z < Symbol =? Product =? Sum =? Pow
            and Symbol < (Pow, Sum)
        If expressions are of the same kind...
        both Z:
            z_i comes before z_k if z_i < z_k
            this only really matters for ordering pow
        both Sym:
            symbol a comes before symbol b if len(a) < len(b)
            if len(a) = len(b), then ordering lexographically
        a, b:
            a comes before b if sum_deg(a) comes after sum_deg(b), implying Symbol < Pow
            otherwise a comes before b if len(a) > len(b), len is the number of terms, len(sym) < len(product | sum)
            otherwise compare first arguements, then the next. At this point a and b must both be sums.
    */
    pub fn order (&self, other: &SymExpr) -> Ordering {
        // TODO double check work
        match (self, other) {
            (SymExpr::Z(z), SymExpr::Z(zz)) => z.cmp(zz),
            (SymExpr::Z(_), _) => Ordering::Less,

            (SymExpr::Symbol(_), SymExpr::Z(_)) => Ordering::Greater,
            (SymExpr::Symbol(s), SymExpr::Symbol(ss)) => string_order(&s, &ss),
            (SymExpr::Symbol(s), SymExpr::Pow(_) | SymExpr::Sum(_)) => Ordering::Less,

            
            (SymExpr::Sum(_), SymExpr::Z(_) | SymExpr::Symbol(_)) => Ordering::Greater,
            (SymExpr::Sum(s), SymExpr::Sum(ss)) => s.order(ss),

            (SymExpr::Product(_), SymExpr::Z(_)) => Ordering::Greater,
            (SymExpr::Product(p), SymExpr::Product(pp)) => panic!("{:?} {:?}", p, pp), // TODO

            (SymExpr::Pow(_), SymExpr::Z(_) | SymExpr::Symbol(_)) => Ordering::Greater,
            (SymExpr::Pow(p), SymExpr::Pow(pp)) => p.order(pp),

            (a, b) => Self::order_aux(a, b),
        }
    }
    fn order_aux(a: &SymExpr, b: &SymExpr) -> Ordering {
        let a_deg = a.sum_deg().order(&b.sum_deg());
        if a_deg.is_gt() {
            return Ordering::Greater;
        }
        else if a_deg.is_lt() {
            return Ordering::Less;
        }

        let a_n = a.num_terms();
        let b_n = a.num_terms();

        if a_n < b_n {
            return Ordering::Less;
        }
        else if a_n > b_n {
            return Ordering::Greater;
        }

        match (a, b) {
            (SymExpr::Sum(a), SymExpr::Sum(b)) => a.order_eq_len_sum(b),
            _ => unreachable!() // TODO wait this isn't actually unreachable? $2 * b + a got it
        }
    }
    pub fn sum_deg(&self) -> SymExpr {
        match self {
            SymExpr::Z(_) => SymExpr::Z(BigInt::ZERO),
            SymExpr::Symbol(_) => SymExpr::Z(BigInt::one()),
            SymExpr::Pow(p) => *p.exp.clone(),
            SymExpr::Product(p) => p.factors.iter()
                .fold(SymExpr::Z(BigInt::ZERO), |acc, x| acc.add(x.sum_deg())),
            SymExpr::Sum(s) => {
                if s.terms.len() == 0 {
                    unreachable!()
                } else {
                    s.terms[0].sum_deg() // assumes s is simplified
                }
            }
        }
    }
    pub fn num_terms(&self) -> usize {
        match self {
            SymExpr::Sum(s) => s.terms.len(),
            _ => 0,
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
        if terms.len() == 1 {
            terms.pop().unwrap()
        } else {
            SymExpr::Sum(Sum{ terms })
        }
    }
    pub fn simplify(self) -> SymExpr {
        match self
            .simplify_each()
            .flatten()
            .combine_like_terms()
        {
            SymExpr::Sum(s) => SymExpr::Sum(s.order_terms()),
            other => other,
        }
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
    fn order_eq_len_sum(&self, other: &Sum) -> Ordering {
        assert_eq!(self.terms.len(), other.terms.len());
        for (a, b) in Iterator::zip(self.terms.iter(), other.terms.iter()) {
            match a.order(b) {
                Ordering::Less => return Ordering::Less,
                Ordering::Greater => return Ordering::Greater,
                _ => {}
            }
        }
        Ordering::Equal // Should be unreachable
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
    fn add_a_to_entry(a: SymExpr, entry: &mut SymExpr) {
        match (entry, a) {
            (SymExpr::Z(n), SymExpr::Z(nn)) => *n = n.clone() + nn,
            (a, b) => {
                *a = a.clone().add(b);
            }
        }
    }
    fn collect_factors(self) -> SymExpr {
        let mut mp = HashMap::new();
        let one = SymExpr::Z(BigInt::one());
        let mut numeric = BigInt::one();
        for factor in self.factors {
            if let SymExpr::Pow(p) = factor {
                let exp = mp.entry(*p.base)
                    .or_insert(SymExpr::Z(BigInt::ZERO));
                Self::add_a_to_entry(*p.exp, exp);
            }
            else if let SymExpr::Z(z) = factor {
                if z == BigInt::ZERO {
                    return SymExpr::Z(BigInt::ZERO);
                }
                numeric *= z;
            }
            else {
                let exp = mp.entry(factor)
                    .or_insert(SymExpr::Z(BigInt::ZERO));
                Self::add_a_to_entry(one.clone(), exp);
            }
        }
        let mut factors = Vec::new();
        for (base, exp) in mp.into_iter() {
            let exp = exp.simplify();
            match &exp {
                SymExpr::Z(z) => 
                if *z == BigInt::ZERO {
                    numeric += 1;
                }
                else if *z == BigInt::one() {
                    factors.push(base);
                } else {
                    factors.push(SymExpr::Pow(Pow { base: Box::new(base), exp: Box::new(exp) } ));
                }
                _ => factors.push(SymExpr::Pow(Pow { base: Box::new(base), exp: Box::new(exp) } )),
            }
        }

        if numeric != BigInt::one() {
            factors.push(SymExpr::Z(numeric));
        }

        if factors.len() == 1 {
            factors.pop().unwrap()
        } else {
            SymExpr::Product(Product { factors })
        }
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
            SymExpr::Product(t) => match t.collect_factors() {
                SymExpr::Product(p) => SymExpr::Product(p.order_factors()),
                other => other
            }
            SymExpr::Sum(s) => s.simplify(),
            _ => unreachable!(),
        }
    }
    pub fn seperate_coef(self) -> (BigInt, SymExpr) {
        let mut coef = BigInt::one();
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
    pub fn to_string(&self) -> String {
        format!("{}^{}", self.base.to_string(), self.exp.to_string())
    }
    pub fn order(&self, other: &Pow) -> Ordering {
        match self.exp.order(&other.exp) {
            Ordering::Equal => self.base.order(&other.base),
            order => order,
        }
    }
}

fn decompose_u64(n: u64) -> Vec<u32> {
    let mut n = n;
    let mut decomp = Vec::new();
    let mask: u64 = 0xffffffff;
    while n > 0 {
        decomp.push((n & mask) as u32);
        n >>= 32;
    }
    decomp
}
