// Ported from 1.ts

use BigInteger;

config const n = 1;
const P = "115792089237316195423570985008687907853269984665640564039457584007908834671663" : bigint;
const N = "115792089237316195423570985008687907852837564279074904382605163141518161494337" : bigint;
const GX = "55066263022277343669578718895168534326250603453777594175500187360389116729240" : bigint;
const GY = "32670510020758816978083085130507043184471273380659243275938904335757337482424" : bigint;
const BETA = "55594575648329892869085402983802832744385952214688224221778511981742606582254" : bigint;
const POW_2_128 = "340282366920938463463374607431768211456" : bigint;
var ZERO_J = new_zero_j();
var BASE = new_base();

proc new_zero_j() {
  return new JacobianPoint(0:bigint, 1:bigint, 0:bigint);
}

proc new_base_j() {
  return new JacobianPoint(GX, GY, 1:bigint);
}

class JacobianPoint {
  var x, y, z: bigint;
  proc init(x: bigint, y: bigint, z: bigint) {
    this.x = x;
    this.y = y;
    this.z = z;
  }

  proc clone() {
    return new JacobianPoint(this.x, this.y, this.z);
  }

  proc to_p() {
    var inv_z = invert(this.z);
    var inv_z_pow = inv_z ** 2;
    var x = mod(this.x * inv_z_pow);
    var y = mod(this.y * inv_z * inv_z_pow);
    return new Point(x, y);
  }

  proc negate() {
    return new JacobianPoint(this.x, mod(-this.y), this.z);
  }

  proc negate_mut() {
    this.y = mod(-this.y);
  }

  proc double() {
    var x1 = this.x;
    var y1 = this.y;
    var z1 = this.z;
    var a = mod(x1 ** 2);
    var b = mod(y1 ** 2);
    var c = mod(b ** 2);
    var d = mod(2 * (mod(mod((x1 + b) ** 2)) - a - c));
    var e = mod(3 * a);
    var f = mod(e ** 2);
    var x3 = mod(f - 2 * d);
    var y3 = mod(e * (d - x3) - 8 * c);
    var z3 = mod(2 * y1 * z1);
    return new JacobianPoint(x3, y3, z3);
  }

  proc double_mut() {
    var x1 = this.x;
    var y1 = this.y;
    var z1 = this.z;
    var a = mod(x1 ** 2);
    var b = mod(y1 ** 2);
    var c = mod(b ** 2);
    var d = mod(2 * (mod(mod((x1 + b) ** 2)) - a - c));
    var e = mod(3 * a);
    var f = mod(e ** 2);
    var x3 = mod(f - 2 * d);
    var y3 = mod(e * (d - x3) - 8 * c);
    var z3 = mod(2 * y1 * z1);
    this.x = x3;
    this.y = y3;
    this.z = z3;
  }

  proc add(other: JacobianPoint) {
    var x1 = this.x,
        y1 = this.y,
        z1 = this.z;
    var x2 = other.x,
        y2 = other.y,
        z2 = other.z; 
    if x2 == 0 || y2 == 0 then return this.clone();
    if x1 == 0 || y1 == 0 then return other.clone();
    var z1z1 = mod(z1 ** 2),
        z2z2 = mod(z2 ** 2),
        u1 = mod(x1 * z2z2),
        u2 = mod(x2 * z1z1),
        s1 = mod(y1 * z2 * z2z2),
        s2 = mod(mod(y2 * z1) * z1z1),
        h = mod(u2 - u1),
        r = mod(s2 - s1);
    if h == 0 {
      if r == 0 then return this.double();
      return ZERO_J;
    }
    var hh = mod(h ** 2),
        hhh = mod(h * hh),
        v = mod(u1 * hh),
        x3 = mod(r ** 2 - hhh - 2 * v),
        y3 = mod(r * (v - x3) - s1 * hhh),
        z3 = mod(z1 * z2 * h);
    return new JacobianPoint(x3, y3, z3);
  }

  proc add_mut(other: JacobianPoint) {
    var x1 = this.x,
        y1 = this.y,
        z1 = this.z;
    var x2 = other.x,
        y2 = other.y,
        z2 = other.z; 
    if x2 == 0 || y2 == 0 then return;
    if x1 == 0 || y1 == 0 {
      this.x = other.x;
      this.y = other.y;
      this.z = other.z;
      return;
    }
    var z1z1 = mod(z1 ** 2),
        z2z2 = mod(z2 ** 2),
        u1 = mod(x1 * z2z2),
        u2 = mod(x2 * z1z1),
        s1 = mod(y1 * z2 * z2z2),
        s2 = mod(mod(y2 * z1) * z1z1),
        h = mod(u2 - u1),
        r = mod(s2 - s1);
    if h == 0 {
      if r == 0 {
        this.double_mut();
        return;
      }
      this.x = ZERO_J.x;
      this.y = ZERO_J.y;
      this.z = ZERO_J.z;
      return ;
    }
    var hh = mod(h ** 2),
        hhh = mod(h * hh),
        v = mod(u1 * hh),
        x3 = mod(r ** 2 - hhh - 2 * v),
        y3 = mod(r * (v - x3) - s1 * hhh),
        z3 = mod(z1 * z2 * h);
    this.x = x3;
    this.y = y3;
    this.z = z3;
  }

  proc multiply_unsafe(k: bigint) {
    var (k1neg, k1, k2neg, k2) = split_scalar_endo(k);
    var k1p = new_zero_j(),
        k2p = new_zero_j(),
        d = this.clone();
    while k1 > 0 || k2 > 0 {
      if k1.isOdd() then k1p.add_mut(d);
      if k2.isOdd() then k2p.add_mut(d);
      d.double_mut();
      k1 >>= 1;
      k2 >>= 1;
    }
    if k1neg then k1p.negate_mut();
    if k2neg then k2p.negate_mut();
    var tmp = new JacobianPoint(mod(k2p.x * BETA), k2p.y, k2p.z);
    k1p.add_mut(tmp);
    return k1p;
  }
}

proc new_zero() {
  return new Point(0:bigint, 0:bigint);
}

proc new_base() {
  return new Point(GX, GY);
}

class Point {
  var x, y: bigint;
  proc init(x: bigint, y: bigint) {
    this.x = x;
    this.y = y;
  }

  proc to_j() {
    return new JacobianPoint(this.x, this.y, 1:bigint);
  }

  proc multiply(scalar : bigint) {
    return this.to_j().multiply_unsafe(scalar).to_p();
  }
}

proc mod(a : bigint, b = P) {
  var r = a % b;
  if r < 0 {
    return r + b;
  }
  else {
    return r;
  }
}

proc invert(number : bigint, modulo = P) {
  var a = mod(number, modulo);
  var b = modulo;
  var x = 0 : bigint;
  var y = 1 : bigint;
  var u = 1 : bigint;
  var v = 0 : bigint;
  while a != 0 {
    var q = b / a;
    var r = b % a;
    var m = x - u * q;
    var n = y - v * q;
    b = a;
    a = r;
    x = u;
    y = v;
    u = m;
    v = n;
  }
  return mod(x, modulo);
}

proc div_nearest(a : bigint, b : bigint) {
  return (a + b / 2) / b;
}

proc split_scalar_endo(k : bigint) {
  const a1 = "0x3086d221a7d46bcde86c90e49284eb15" : bigint;
  const b1 = "-0xe4437ed6010e88286f547fa90abfe4c3" : bigint;
  const a2 = "0x114ca50f7a8e2f3f657c1108d9d44cfd8" : bigint;
  var b2 = a1;
  var c1 = div_nearest(b2 * k, N);
  var c2 = div_nearest(-b1 * k, N);
  var k1 = mod(k - c1 * a1 - c2 * a2, N);
  var k2 = mod(-c1 * b1 - c2 * b2, N);
  var k1neg = k1 > POW_2_128;
  var k2neg = k2 > POW_2_128;
  if k1neg then k1 = N - k1;
  if k2neg then k2 = N - k2;
  return (k1neg, k1, k2neg, k2);
}

proc main() {
  const private_key = "0x2DEE927079283C3C4FCA3EF970FF4D38B64592E3FE0AB0DAD9132D70B5BC7693" : bigint;
  ref point = BASE;
  for 1..n {
    point = point.multiply(private_key);
  } 
  writef("%s,%s\n", point.x.get_str(16), point.y.get_str(16));
}
