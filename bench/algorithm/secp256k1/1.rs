// Algorithm ported from 1.ts

use ibig::{ibig, IBig};
use lazy_static::lazy_static;
use num_traits::{Signed, Zero};
use std::{borrow::Borrow, ops::ShrAssign};

mod curve {
    use super::*;

    lazy_static! {
        pub static ref P: IBig = ibig!(2).pow(256) - ibig!(2).pow(32) - ibig!(977);
        pub static ref N: IBig = ibig!(2).pow(256)
            - IBig::from_str_radix("432420386565659656852420866394968145599", 10).unwrap();
        pub static ref BETA: IBig = IBig::from_str_radix(
            "7ae96a2b657c07106e64479eac3434e99cf0497512f58995c1396c28719501ee",
            16
        )
        .unwrap();
    }
}

lazy_static! {
    static ref PRIVATE_KEY: IBig = IBig::from_str_radix(
        "20775598474904240222758871485654738649026525153462921990999819694398496339603",
        10
    )
    .unwrap();
    static ref POW_2_128: IBig = ibig!(2).pow(128);
    static ref ZERO: Point = Point::zero();
    static ref ZERO_J: JacobianPoint = JacobianPoint::zero();
    static ref A1: IBig = IBig::from_str_radix("3086d221a7d46bcde86c90e49284eb15", 16).unwrap();
    static ref B1: IBig = IBig::from_str_radix("-e4437ed6010e88286f547fa90abfe4c3", 16).unwrap();
    static ref A2: IBig = IBig::from_str_radix("114ca50f7a8e2f3f657c1108d9d44cfd8", 16).unwrap();
}

#[derive(Debug, Clone)]
struct JacobianPoint {
    pub x: IBig,
    pub y: IBig,
    pub z: IBig,
}

impl JacobianPoint {
    pub fn new(x: IBig, y: IBig, z: IBig) -> Self {
        Self { x, y, z }
    }

    pub fn zero() -> Self {
        Self {
            x: ibig!(0),
            y: ibig!(1),
            z: ibig!(0),
        }
    }

    pub fn from_affine(p: Point) -> Self {
        Self::new(p.x, p.y, ibig!(1))
    }

    pub fn to_affine(&self) -> Point {
        let inv_z = invert(&self.z);
        let inv_z_pow = inv_z.pow(2);
        let x = rem((&self.x * &inv_z_pow).borrow());
        let y = rem((&self.y * &inv_z * &inv_z_pow).borrow());
        Point::new(x, y)
    }

    pub fn negate(self) -> Self {
        Self::new(self.x, rem(&-self.y), self.z)
    }

    pub fn double(&self) -> Self {
        let a = rem(&self.x.pow(2));
        let b = rem(&self.y.pow(2));
        let c = rem(&b.pow(2));
        let d = rem(
            (ibig!(2) * rem((rem((&self.x + &b).pow(2).borrow()) - &a - &c).borrow())).borrow(),
        );
        let e = rem((ibig!(3) * &a).borrow());
        let f = rem(&e.pow(2).borrow());
        let x3 = rem(&(f - &d * ibig!(2)));
        let y3 = rem(&(&e * (&d - &x3) - &c * ibig!(8)));
        let z3 = rem(&(&self.y * &self.z * ibig!(2)));
        Self::new(x3, y3, z3)
    }

    pub fn add(&self, other: &Self) -> Self {
        if other.x.is_zero() || other.y.is_zero() {
            self.clone()
        } else if self.x.is_zero() || self.y.is_zero() {
            other.clone()
        } else {
            let z1z1 = self.z.pow(2);
            let z2z2 = other.z.pow(2);
            let u1 = rem(&(&self.x * &z2z2));
            let u2 = rem(&(&other.x * &z1z1));
            let s1 = rem(&(&self.y * &other.z * &z2z2));
            let s2 = rem(&(rem(&(&other.y * &self.z)) * &z1z1));
            let h = rem(&(&u2 - &u1));
            let r = rem(&(&s2 - &s1));
            if h.is_zero() {
                if r.is_zero() {
                    self.double()
                } else {
                    Self::zero()
                }
            } else {
                let hh = rem(&h.pow(2).borrow());
                let hhh = rem(&(&h * &hh));
                let v = rem(&(&u1 * &hh));
                let x3 = rem(&(&r.pow(2) - &hhh - &v * ibig!(2)));
                let y3 = rem(&(&r * (&v - &x3) - &s1 * &hhh));
                let z3 = rem(&(&self.z * &other.z * &h));
                Self::new(x3, y3, z3)
            }
        }
    }

    pub fn mul_unsafe(&self, scalar: &IBig) -> Self {
        let n = scalar.clone();

        // let mut p = Self::zero();
        // let mut d = self.clone();
        // while n.is_positive() {
        //     if !(&n & 1_u8).is_zero() {
        //         p = p.add(&d);
        //     }
        //     d = d.double();
        //     n.shr_assign(1);
        // }
        // p

        let (k1neg, mut k1, k2neg, mut k2) = split_scalar_endo(&n);
        let mut k1p = Self::zero();
        let mut k2p = Self::zero();
        let mut d = self.clone();
        while k1.is_positive() || k2.is_positive() {
            if !(&k1 & 1_u8).is_zero() {
                k1p = k1p.add(&d);
            }
            if !(&k2 & 1_u8).is_zero() {
                k2p = k2p.add(&d);
            }
            d = d.double();
            k1.shr_assign(1);
            k2.shr_assign(1);
        }
        if k1neg {
            k1p = k1p.negate();
        }
        if k2neg {
            k2p = k2p.negate();
        }
        let beta: &IBig = &curve::BETA;
        k2p = Self::new(rem(&(&k2p.x * beta)), k2p.y.clone(), k2p.z.clone());
        k1p.add(&k2p)
    }
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

    pub fn mul(&self, scalar: &IBig) -> Self {
        let pj = JacobianPoint::from_affine(self.clone());
        pj.mul_unsafe(scalar).to_affine()
    }
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

fn rem_n(a: &IBig) -> IBig {
    let b: &IBig = &curve::N;
    let r = a % b;
    if r.is_negative() {
        b + r
    } else {
        r
    }
}

fn invert(number: &IBig) -> IBig {
    let modulo: &IBig = &curve::P;
    let mut a = rem(number);
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
    rem(&x)
}

fn div_nearest(a: &IBig, b: &IBig) -> IBig {
    (a + b / ibig!(2)) / b
}

fn split_scalar_endo(k: &IBig) -> (bool, IBig, bool, IBig) {
    let n: &IBig = &curve::N;
    let a1: &IBig = &A1;
    let b1: &IBig = &B1;
    let a2: &IBig = &A2;
    let b2 = a1;
    let c1 = div_nearest(&(b2 * k), n);
    let c2 = div_nearest(&(-b1 * k), n);
    let mut k1 = rem_n(&(k - &c1 * a1 - &c2 * a2));
    let mut k2 = rem_n(&(-&c1 * b1 - &c2 * b2));
    let k1neg = &k1 > &POW_2_128;
    let k2neg = &k2 > &POW_2_128;
    if k1neg {
        k1 = n - &k1;
    }
    if k2neg {
        k2 = n - &k2;
    }
    (k1neg, k1, k2neg, k2)
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
