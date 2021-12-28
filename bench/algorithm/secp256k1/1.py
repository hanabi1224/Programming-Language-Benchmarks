# ported from 1.ts

import sys

P = 2 ** 256 - 2 ** 32 - 977
N = 2 ** 256 - 432420386565659656852420866394968145599
GX = 55066263022277343669578718895168534326250603453777594175500187360389116729240
GY = 32670510020758816978083085130507043184471273380659243275938904335757337482424
BETA = 0x7ae96a2b657c07106e64479eac3434e99cf0497512f58995c1396c28719501ee
POW_2_128 = 2 ** 128


class JacobianPoint(object):
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

    def zero():
        return JacobianPoint(0, 1, 0)

    def base():
        return JacobianPoint(GX, GY, 1)

    def from_affine(p):
        return JacobianPoint(p.x, p.y, 1)

    def to_affine(self):
        inv_z = invert(self.z)
        inv_z_pow = inv_z ** 2
        x = mod(self.x * inv_z_pow)
        y = mod(self.y * inv_z * inv_z_pow)
        return Point(x, y)

    def negate(self):
        return JacobianPoint(self.x, mod(-self.y), self.z)

    def double(self):
        x1 = self.x
        y1 = self.y
        z1 = self.z
        a = mod(x1 ** 2)
        b = mod(y1 ** 2)
        c = mod(b ** 2)
        d = mod(2 * (mod(mod((x1 + b) ** 2)) - a - c))
        e = mod(3 * a)
        f = mod(e ** 2)
        x3 = mod(f - 2 * d)
        y3 = mod(e * (d - x3) - 8 * c)
        z3 = mod(2 * y1 * z1)
        return JacobianPoint(x3, y3, z3)

    def add(self, other):
        x1 = self.x
        y1 = self.y
        z1 = self.z
        x2 = other.x
        y2 = other.y
        z2 = other.z
        if x2 == 0 or y2 == 0:
            return self
        if x1 == 0 or y1 == 0:
            return other
        z1z1 = mod(z1 ** 2)
        z2z2 = mod(z2 ** 2)
        u1 = mod(x1 * z2z2)
        u2 = mod(x2 * z1z1)
        s1 = mod(y1 * z2 * z2z2)
        s2 = mod(mod(y2 * z1) * z1z1)
        h = mod(u2 - u1)
        r = mod(s2 - s1)
        if h == 0:
            if r == 0:
                return self.double()
            else:
                return JacobianPoint.zero()
        hh = mod(h ** 2)
        hhh = mod(h * hh)
        v = mod(u1 * hh)
        x3 = mod(r ** 2 - hhh - 2 * v)
        y3 = mod(r * (v - x3) - s1 * hhh)
        z3 = mod(z1 * z2 * h)
        return JacobianPoint(x3, y3, z3)

    def multiply_unsafe(self, n):
        (k1neg, k1, k2neg, k2) = split_scalar_endo(n)
        k1p = JacobianPoint.zero()
        k2p = JacobianPoint.zero()
        d = self
        while k1 > 0 or k2 > 0:
            if k1 & 1:
                k1p = k1p.add(d)
            if k2 & 1:
                k2p = k2p.add(d)
            d = d.double()
            k1 >>= 1
            k2 >>= 1
        if k1neg:
            k1p = k1p.negate()
        if k2neg:
            k2p = k2p.negate()
        k2p = JacobianPoint(mod(k2p.x * BETA), k2p.y, k2p.z)
        return k1p.add(k2p)


class Point(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def zero():
        return Point(0, 0)

    def base():
        return Point(GX, GY)

    def multiply(self, scalar):
        return JacobianPoint.from_affine(self).multiply_unsafe(scalar).to_affine()


def mod(a, b=P):
    r = a % b
    if r < 0:
        return r + b
    return r


def invert(number, modulo=P):
    a = mod(number, modulo)
    b = modulo
    x, y, u, v = 0, 1, 1, 0
    while a != 0:
        q = b // a
        r = b % a
        m = x - u * q
        n = y - v * q
        b, a = a, r
        x, y = u, v
        u, v = m, n
    return mod(x, modulo)


def div_nearest(a, b):
    return (a + b // 2) // b


def split_scalar_endo(k):
    n = N
    a1 = 0x3086d221a7d46bcde86c90e49284eb15
    b1 = -0xe4437ed6010e88286f547fa90abfe4c3
    a2 = 0x114ca50f7a8e2f3f657c1108d9d44cfd8
    b2 = a1
    c1 = div_nearest(b2 * k, n)
    c2 = div_nearest(-b1 * k, n)
    k1 = mod(k - c1 * a1 - c2 * a2, n)
    k2 = mod(-c1 * b1 - c2 * b2, n)
    k1neg = k1 > POW_2_128
    k2neg = k2 > POW_2_128
    if k1neg:
        k1 = n - k1
    if k2neg:
        k2 = n - k2
    return (k1neg, k1, k2neg, k2)


def main():
    n = 1 if len(sys.argv) < 2 else int(sys.argv[1])
    private_key = 0x2DEE927079283C3C4FCA3EF970FF4D38B64592E3FE0AB0DAD9132D70B5BC7693
    point = Point.base()
    for i in range(0, n):
        point = point.multiply(private_key)
    print(f"{point.x:x},{point.y:x}")


if __name__ == '__main__':
    main()
