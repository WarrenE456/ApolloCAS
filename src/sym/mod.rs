pub extern crate num_bigint;
pub extern crate num_traits;

use num_bigint::BigInt;
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
    Polynomial(Polynomial),
}

impl SymExpr {
    pub fn z_from_string(s: &String) -> SymExpr {
        SymExpr::Z(BigInt::from_str(s.as_str()).unwrap())
    }
    pub fn set_simplified(self, simplified: bool) -> SymExpr {
        match self {
            SymExpr::Z(_) => self,
            SymExpr::Symbol(_) => self,
            SymExpr::Sum(s) => SymExpr::Sum(s.set_simplified(simplified)),
            SymExpr::Product(p) => SymExpr::Product(p.set_simplified(true)),
            SymExpr::Pow(p) => SymExpr::Pow(p.set_simplified(true)),
            SymExpr::Polynomial(_) => self,
        }
    }
    pub fn simplify(self) -> SymExpr {
        match self {
            SymExpr::Z(_) => self,
            SymExpr::Symbol(_) => self,
            SymExpr::Sum(s) => s.simplify(),
            SymExpr::Product(p) => p.simplify(),
            SymExpr::Pow(p) => p.simplify(),
            SymExpr::Polynomial(p) => SymExpr::Polynomial(p),
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
            SymExpr::Polynomial(p) => p.to_string(),
        }
    }
    pub fn kind_name(&self) -> String {
        match self {
            SymExpr::Symbol(_) => String::from("symbol"),
            SymExpr::Z(_) => String::from("Z"),
            SymExpr::Sum(_) => String::from("sum of expressions"),
            SymExpr::Product(_) => String::from("product of expressions"),
            SymExpr::Pow(_)  => String::from("exponential"),
            SymExpr::Polynomial(_) => String::from("polynomial"),
        }
    }
    pub fn distribute(self, sum: Sum) -> Sum {
        let mut terms = Vec::new();
        for term in sum.terms.iter() {
            let term = self.clone().mul(term.clone());
            terms.push(term);
        }
        Sum::new(terms)
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
            (a, b) => SymExpr::Sum(Sum::new(vec![a, b]).flatten()),
        }
    }
    pub fn mul(self, other: SymExpr) -> SymExpr {
        match (self, other) {
            (SymExpr::Z(a), SymExpr::Z(b)) => SymExpr::Z(a * b),
            (a, b) => SymExpr::Product(Product::new(vec![a, b]).flatten()),
        }
    }
    // Value for ordering differient kinds
    pub fn kind_value(&self) -> usize {
        match self {
            SymExpr::Z(_) => 0,
            SymExpr::Symbol(_) => 1,
            SymExpr::Pow(_) => 2,
            SymExpr::Product(_) => 3,
            SymExpr::Sum(_) => 4,
            SymExpr::Polynomial(_) => unreachable!(),
        }
    }
    /*
    *
    Ordring function for general expressions
    Nessessary for cannonical representations
    e.g. 1 + x -> x + 1, x + 1 = x + 1 is true
    
    Start my comparing expression kinds:
    Z < Symbol < Power < Product < Sum
    
    Then compare amoung like-kinds:
    Z:
    a < b -> a comes before b

    Symbol:
    lexographic ordering

    Power:
    compare exponents then bases

    Product | Sum:
    compare elementwise
    then len(a) < len(b) -> a comes before b

    */
    pub fn order(&self, other: &SymExpr) -> Ordering {

        let self_value = self.kind_value();
        let other_value = other.kind_value();

        // Ensures kind(a) = kind(b)
        if self_value < other_value {
            return Ordering::Less;
        } else if self_value > other_value {
            return Ordering::Greater;
        }

        match (self, other) {
            (SymExpr::Z(a), SymExpr::Z(b)) => {
                a.cmp(b)
            }
            (SymExpr::Symbol(a), SymExpr::Symbol(b)) => {
                a.cmp(b)
            }
            (SymExpr::Pow(a), SymExpr::Pow(b)) => {
                a.order(b)
            }
            (SymExpr::Product(a), SymExpr::Product(b)) => {
                Self::elementwise_order(&a.factors, &b.factors)
            }
            (SymExpr::Sum(a), SymExpr::Sum(b)) => {
                Self::elementwise_order(&a.terms, &b.terms)
            }
            _ => unreachable!(),
        }
    }
    fn elementwise_order(args1: &Vec<SymExpr>, args2: &Vec<SymExpr>) -> Ordering {
        use std::iter::zip;
        for (a, b) in zip(args1.iter().rev(), args2.iter().rev()) {
            match a.order(b) {
                Ordering::Equal => {}
                other => return other,
            }
        }

        args1.len().cmp(&args2.len())
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
            SymExpr::Polynomial(p) => {
                let sum = p.terms.iter().fold(BigInt::ZERO, |acc, t| acc + t.deg.clone());
                SymExpr::Z(sum)
            }
        }
    }
    pub fn num_terms(&self) -> usize {
        match self {
            SymExpr::Sum(s) => s.terms.len(),
            _ => 0,
        }
    }
    pub fn is_one(&self) -> bool {
        match self {
            SymExpr::Z(z) => z.is_one(),
            _ => false
        }
    }
    pub fn is_zero(&self) -> bool {
        match self {
            SymExpr::Z(z) => *z == BigInt::ZERO,
            _ => false
        }
    }
    pub fn gcd(&self, other: &SymExpr) -> Result<SymExpr, String> {
        use crate::runtime::val::bin_gcd;
        match (self, other) {
            (SymExpr::Z(a), SymExpr::Z(b)) => {
                Ok(SymExpr::Z(bin_gcd(&a.clone(), &b.clone())))
            }
            _ => todo!()
        }
    }
    pub fn to_term(self, var: &String) -> Result<Term, String> {
        match self {
            SymExpr::Z(z) => Ok(Term::new(SymExpr::Z(z), BigInt::ZERO)),
            SymExpr::Symbol(s) => if s == *var {
                Ok(Term::new(SymExpr::Z(BigInt::one()), BigInt::one()))
            } else {
                Ok(Term::new(SymExpr::Symbol(s), BigInt::ZERO))
            }
            SymExpr::Product(p) => p.to_term(var),
            SymExpr::Pow(p) => p.to_term(var),
            SymExpr::Sum(s) => Err(format!("Attempt to turn sum '{}' into a single term.", s.to_string())),
            SymExpr::Polynomial(_) => unreachable!(),
        }
    }
    pub fn to_polynomial(self, var: &String) -> Result<Polynomial, String> {
        match self.simplify() {
            SymExpr::Sum(s) => Ok(s.to_polynomial(var)?.simplify()),
            SymExpr::Polynomial(p) => if *var == p.var {
                Ok(p)
            } else {
                Err(format!(
                    "Polynomial '{}' in {} cannot be converted to a polynomial in {}.",
                    p.to_string(), p.var, var
                ))
            }
            other => Ok(other.to_term(var)?.to_monomial(var.to_owned()).simplify()),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Sum {
    pub terms: Vec<SymExpr>,
    pub simplified: bool,
}

impl Sum {
    pub fn new(terms: Vec<SymExpr>) -> Sum {
        Sum { terms, simplified: false }
    }
    fn simplify_each(self) -> Sum {
        if self.simplified {
            return self;
        }

        let terms = self.terms
            .into_iter().map(|expr| expr.simplify())
            .collect::<Vec<SymExpr>>(); 
        Sum::new(terms)
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
        Sum::new(terms)
    }
    fn order_terms(self) -> Sum {
        let mut terms = self.terms;
        terms.sort_by(|a, b| a.order(b));
        Sum::new(terms)
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
                    SymExpr::Product(Product::new(factors).flatten())
                }
            }else {
                let factors = vec![SymExpr::Z(coef), term];
                SymExpr::Product(Product::new(factors).flatten())
            };
            terms.push(term);
        }
        if terms.len() == 1 {
            terms.pop().unwrap()
        }
        else if terms.len() == 0 {
            SymExpr::Z(BigInt::ZERO)
        } else {
            SymExpr::Sum(Sum::new(terms))
        }
    }
    pub fn distribute(self, other: Sum) -> Sum {
        let mut terms = Vec::new();
        for a in self.terms.iter() {
            let term = a.clone().distribute(other.clone());
            terms.push(SymExpr::Sum(term));
        }
        Sum::new(terms)
    }
    fn set_simplified(self, simplified: bool) -> Sum {
        Sum { terms: self.terms, simplified }
    }
    pub fn simplify(self) -> SymExpr {
        if self.simplified {
            return SymExpr::Sum(self);
        }

        match self
            .simplify_each()
            .flatten()
            .combine_like_terms()
        {
            SymExpr::Sum(s) => SymExpr::Sum(s.order_terms().set_simplified(true)),
            other => other.set_simplified(true),
        }
    }
    pub fn to_string(&self) -> String {
        self.terms
            .iter().map(|expr| expr.to_string())
            .collect::<Vec<_>>()
            .join(&String::from(" + "))
    }
    pub fn to_polynomial(self, var: &String) -> Result<Polynomial, String> {
        let mut terms = Vec::new();
        for term in self.terms {
            terms.push(term.to_term(var)?);
        }
        Ok(Polynomial::new(var.to_owned(), terms))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Product {
    pub factors: Vec<SymExpr>,
    pub simplified: bool,
}

impl Product {
    pub fn new(factors: Vec<SymExpr>) -> Product {
        Product { factors, simplified: false }
    }
    pub fn set_simplified(self, simplified: bool) -> Product {
        Product { factors: self.factors, simplified }
    }
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
                    SymExpr::Product(Product::new(factors).flatten())
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
        Product::new(flattened)
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
                    factors.push(SymExpr::Pow(Pow::new(Box::new(base), Box::new(exp))));
                }
                _ => factors.push(SymExpr::Pow(Pow::new(Box::new(base), Box::new(exp)))),
            }
        }

        if numeric != BigInt::one() {
            factors.push(SymExpr::Z(numeric));
        }

        if factors.len() == 1 {
            factors.pop().unwrap()
        } else {
            SymExpr::Product(Product::new(factors))
        }
    }
    fn order_factors(self) -> Product {
        let mut factors = self.factors;
        factors.sort_by(|a, b| a.order(b));
        Product::new(factors)
    }
    fn simplify_each(self) -> Product {
        let factors = self.factors.into_iter()
            .map(|expr| expr.simplify())
            .collect::<Vec<SymExpr>>();
        Product::new(factors)
    }
    pub fn simplify(self) -> SymExpr {
        if self.simplified {
            return SymExpr::Product(self);
        }

        match self
            .simplify_each()
            .flatten()
            .distribute()
        {
            SymExpr::Product(t) => match t.collect_factors() {
                SymExpr::Product(p) => SymExpr::Product(p.order_factors().set_simplified(true)),
                other => other.set_simplified(true)
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
            (coef, SymExpr::Product(Product::new(factors)))
        }
    }
    pub fn to_term(self, var: &String) -> Result<Term, String> {
        let mut term = Term::new(SymExpr::Z(BigInt::one()), BigInt::ZERO);
        for factor in self.factors {
            term = term.mul(factor.to_term(var)?);
        }
        Ok(term)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Pow {
    pub base: Box<SymExpr>,
    pub exp: Box<SymExpr>,
    pub simplified: bool,
}

impl Pow {
    pub fn new(base: Box<SymExpr>, exp: Box<SymExpr>) -> Pow {
        Pow { base, exp, simplified: false }
    }
    pub fn set_simplified(self, simplified: bool) -> Pow {
        Pow { base: self.base, exp: self.exp, simplified }
    }
    pub fn to_string(&self) -> String {
        format!("{}^{}", self.base.to_string(), self.exp.to_string())
    }
    pub fn order(&self, other: &Pow) -> Ordering {
        match self.exp.order(&other.exp) {
            Ordering::Equal => self.base.order(&other.base),
            order => order,
        }
    }
    fn simplify_children(self) -> Pow {
        let base = Box::new(self.base.simplify());
        let exp = Box::new(self.exp.simplify());
        Pow::new(base, exp)
    }
    fn flatten(self) -> Pow {
        match *self.base {
            SymExpr::Pow(p) => {
                let base = Box::new(p.base.simplify());
                let exp = Box::new(self.exp.mul(*p.exp).simplify());
                Pow::new(base, exp)
            }
            base => Pow::new(Box::new(base), self.exp),
        }
    }
    fn expand_or_eval(self) -> SymExpr {
        match (&*self.base, &*self.exp) {
            (SymExpr::Z(a), SymExpr::Z(b)) => {
                let exp = b.try_into().expect("Exponent too large.");
                SymExpr::Z(a.pow(exp))
            }
            (expr, SymExpr::Z(exp)) => {
                let exp = exp.try_into().expect("Exponent too large.");
                let factors = vec![expr.clone(); exp];
                Product::new(factors).simplify()
            }
            _ => SymExpr::Pow(self)
        }
    }
    pub fn simplify(self) -> SymExpr {
        if self.simplified {
            return SymExpr::Pow(self);
        }

        let pow = self
            .simplify_children()
            .flatten()
            .set_simplified(true);

        if pow.base.is_one() {
            return SymExpr::Z(BigInt::one());
        }
        else if pow.exp.is_zero() {
            return SymExpr::Z(BigInt::one());
        } else if pow.exp.is_one() {
            return *pow.base;
        } else {
            pow.expand_or_eval()
        }
    }
    pub fn to_term(self, var: &String) -> Result<Term, String> {
        match self.base.to_term(var)?{
            Term { coef, deg } => {
                if deg == BigInt::ZERO {
                    let coef = SymExpr::Pow(Pow::new(Box::new(coef), self.exp));
                    Ok(Term::new(coef, deg))
                }
                else {
                    if let SymExpr::Z(n) = *self.exp {
                        if n > BigInt::ZERO {
                            Ok(Term::new(coef, deg * n))
                        }
                        else {
                            Err(format!(
                                "Attempt to raise '{}' to negative integer '{}' in symexpr to polynomial conversion.",
                                var, n
                            ))
                        }
                    }
                    else {
                        Err(format!(
                            "Attempt to raise '{}' to non-integer exponent '{}' in symexpr to polynomial conversion.",
                            var, self.exp.to_string()
                        ))
                    }
                }
            }
        }
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Term {
    pub coef: SymExpr,
    pub deg: BigInt,
}

impl Term {
    pub fn new(coef: SymExpr, deg: BigInt) -> Term {
        Term { coef, deg }
    }
    pub fn mul(self, other: Term) -> Term {
        Term::new(self.coef.mul(other.coef), self.deg + other.deg)
    }
    pub fn to_string(&self, var: &String) -> String {
        format!("{} {}^{}", self.coef.to_string(), var, self.deg)
    }
    pub fn to_monomial(self, var: String) -> Polynomial {
        Polynomial::new(var, vec![self])
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Polynomial {
    pub var: String,
    pub terms: Vec<Term>,
}

impl Polynomial {
    pub fn new(var: String, terms: Vec<Term>) -> Polynomial {
        Polynomial { var, terms }
    }
    fn order_terms(mut self) -> Polynomial {
        self.terms.sort_by(|a, b| b.deg.cmp(&a.deg));
        self
    }
    fn simplify_coef(self) -> Polynomial {
        let terms = self.terms
            .into_iter()
            .map(|t| Term::new(t.coef.simplify(), t.deg))
            .collect::<Vec<_>>();
        Polynomial::new(self.var, terms)
    }
    pub fn simplify(self) -> Polynomial {
        self
            .simplify_coef()
            .order_terms()
    }
    pub fn to_string(&self) -> String {
        self.terms
            .iter()
            .map(|t| format!("{} {}^{}", t.coef.to_string(), self.var, t.deg))
            .collect::<Vec<_>>()
            .join(" + ")
    }
}
