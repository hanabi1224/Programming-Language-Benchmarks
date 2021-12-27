// Algorithm ported from 1.js

use ibig::{ibig, IBig};
use lazy_static::lazy_static;
use num_traits::{Signed, Zero};
use std::{borrow::Borrow, ops::ShrAssign};

mod curve {
    use super::*;

    lazy_static! {
        pub static ref P: IBig = ibig!(2).pow(256) - ibig!(2).pow(32) - ibig!(977);
        // pub static ref N: IBig = ibig!(2).pow(256) - ibig!("432420386565659656852420866394968145599" base 10);
    }
}

lazy_static! {
    static ref PRIVATE_KEY: IBig = IBig::from_str_radix(
        "20775598474904240222758871485654738649026525153462921990999819694398496339603",
        10
    )
    .unwrap();
    static ref ZERO: Point = Point::zero();
}

#[derive(Debug, Clone)]
struct Point {
    pub x: IBig,
    pub y: IBig,
}

impl Point {
    pub fn new(x: IBig, y: IBig) -> Self {
        Self { x, y }
    }

    pub fn zero() -> Self {
        Self::new(ibig!(0), ibig!(0))
    }

    pub fn generator() -> Self {
        Self::new(
            IBig::from_str_radix(
                "55066263022277343669578718895168534326250603453777594175500187360389116729240",
                10,
            )
            .unwrap(),
            IBig::from_str_radix(
                "32670510020758816978083085130507043184471273380659243275938904335757337482424",
                10,
            )
            .unwrap(),
        )
    }

    fn double(&self) -> Self {
        let x1 = &self.x;
        let y1 = &self.y;
        let lam =
            Self::rem((ibig!(3) * x1.pow(2) * Self::invert((ibig!(2) * y1).borrow())).borrow());
        let x3 = Self::rem((&lam * &lam - ibig!(2) * x1).borrow());
        let y3 = Self::rem((&lam * (x1 - &x3) - y1).borrow());
        Self::new(x3, y3)
    }

    fn add(&self, other: &Self) -> Self {
        let a = self;
        let b = other;
        if a.x.borrow().is_zero() || a.y.borrow().is_zero() {
            b.clone()
        } else if b.x.borrow().is_zero() || b.y.borrow().is_zero() {
            self.clone()
        } else if a.x.borrow() == b.x.borrow() && a.y.borrow() == b.y.borrow() {
            self.double()
        } else if a.x.borrow() == b.x.borrow() && a.y.borrow() == (-(b.y.borrow())).borrow() {
            Self::zero()
        } else {
            let lam = Self::rem(((&b.y - &a.y) * Self::invert((&b.x - &a.x).borrow())).borrow());
            let x3 = Self::rem((&lam * &lam - &a.x - &b.x).borrow());
            let y3 = Self::rem((&lam * (&a.x - &x3) - &a.y).borrow());
            Self::new(x3, y3)
        }
    }

    pub fn mul(&self, scalar: &IBig) -> Self {
        let mut n = scalar.clone();
        let mut p = Self::zero();
        let mut d = self.clone();
        while n.is_positive() {
            if !(&n & 1_u8).is_zero() {
                p = p.add(&d);
            }
            d = d.double();
            n.shr_assign(1);
        }
        p
    }

    fn rem(a: &IBig) -> IBig {
        let b: &IBig = &curve::P;
        let r = a % b;
        if r.is_negative() {
            b + r
        } else {
            r
        }
    }

    fn invert(number: &IBig) -> IBig {
        let modulo: &IBig = &curve::P;
        let mut a = Self::rem(number);
        let mut b = modulo.clone();
        let mut x = ibig!(0);
        let mut y = ibig!(1);
        let mut u = ibig!(1);
        let mut v = ibig!(0);
        while !a.is_zero() {
            let q = &b / &a;
            let r = &b % &a;
            let m = &x - &u * &q;
            let n = &y - &v * &q;
            b = a.clone();
            a = r;
            x = u;
            y = v;
            u = m;
            v = n;
        }
        Self::rem(&x)
    }
}

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|s| s.parse().ok())
        .unwrap_or(1);

    let mut point = Point::generator();
    for _i in 0..n {
        point = point.mul(&PRIVATE_KEY);
    }
    println!("{:x},{:x}", point.x, point.y);
}
