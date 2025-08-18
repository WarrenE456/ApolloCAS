pub extern crate num_bigint;

use num_bigint::{BigInt, Sign};
use std::str::FromStr;

#[derive(Clone, Debug)]
pub enum SymExpr {
    Z(BigInt),
    Sum(Sum),
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
            SymExpr::Sum(s) => s.simplify(),
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            SymExpr::Z(n) => n.to_string(),
            SymExpr::Sum(s) => s.to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Sum {
    pub exprs: Vec<SymExpr>,
}

impl Sum {
    pub fn simplify(self) -> SymExpr {
        let mut n = BigInt::ZERO;
        let mut exprs = Vec::new();
        for expr in self.exprs {
            match expr.simplify() {
                SymExpr::Z(i) => n += i,
                other => exprs.push(other),
            }
        }
        return if exprs.len() == 0 {
            SymExpr::Z(n)
        } else {
            exprs.push(SymExpr::Z(n));
            SymExpr::Sum(Sum{ exprs })
        };
    }
    pub fn to_string(&self) -> String {
        self.exprs
            .iter().map(|expr| expr.to_string())
            .collect::<Vec<_>>()
            .join(&String::from(" + "))
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
