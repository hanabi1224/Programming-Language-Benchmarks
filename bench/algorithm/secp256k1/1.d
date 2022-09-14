@safe:

import std;

static immutable ZERO = BigInt(0);
static immutable ONE = BigInt(1);
static immutable TWO = BigInt(2);
static immutable THREE = BigInt(3);
static immutable EIGHT = BigInt(8);
static immutable P = BigInt("115792089237316195423570985008687907853269984665640564039457584007908834671663");
static immutable N = BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337");
static immutable GX = BigInt("55066263022277343669578718895168534326250603453777594175500187360389116729240");
static immutable GY = BigInt("32670510020758816978083085130507043184471273380659243275938904335757337482424");
static immutable BETA = BigInt("55594575648329892869085402983802832744385952214688224221778511981742606582254");
static immutable POW_2_128 = BigInt("340282366920938463463374607431768211456");
static immutable A1 = BigInt("64502973549206556628585045361533709077");
static immutable B1 = BigInt("-303414439467246543595250775667605759171");
static immutable A2 = BigInt("367917413016453100223835821029139468248");
static immutable PRIVATE_KEY = BigInt("20775598474904240222758871485654738649026525153462921990999819694398496339603");

void main(string[] args)
{
    auto n = args.length > 1 ? args[1].to!int : 1;
    auto point = point.BASE_P;
    foreach(_; 0..n)
        point = point.multiply(PRIVATE_KEY);
    writefln(point.x, point.y);
}

BigInt mod(BigInt ma, BigInt mb = P) {
    auto r = ma % mb;
    return r < ZERO ? r + mb : r;
}

BigInt invert(BigInt number, BigInt modulo = P) {
    auto a = mod(number, modulo);
    auto b = modulo;
    auto x = ZERO;
    auto y = ONE;
    auto u = ONE;
    auto v = ZERO;
    while (a != ZERO) {
        auto q = b / a;
        auto r = b % a;
        auto m = x - u * q;
        auto n = y - v * q;
        b = a;
        a = r;
        x = u;
        y = v;
        u = m;
        v = n;
    }
    return mod(x, modulo);
}

BigInt divNearest(BigInt a, BigInt b) pure @nogc @nothrow {
    return (a + b / TWO) / b;
}

Tuple!(bool,BigInt,bool,BigInt) splitScalarEndo (BigInt k) pure @nogc @nothrow {
    auto b2 = A1;
    auto c1 = divNearest(b2 * k, N);
    auto c2 = divNearest(-B1 * k, N);
    auto k1 = mod(k - c1 * A1 - c2 * A2, N);
    auto k2 = mod(-c1 * B1 - c2 * b2, N);
    bool k1neg = k1 > POW_2_128;
    bool k2neg = k2 > POW_2_128;
    if (k1neg)
        k1 = N - k1;
    if (k2neg)
        k2 = N - k2;
    return tuple(k1neg, k1, k2neg, k2);
}

class JacobianPoint(BigInt x, BigInt y, BigInt z) {
    pure @nogc @nothrow
    Point toAffine() {
        auto invZ = invert(z);
        auto invZ2 = powmod(invZ, TWO, ZERO);
        auto x = mod(x * invZ2);
        auto y = mod(y * invZ * invZ2);
        return Point(x, y);
    }

    pure @nogc @nothrow
    JacobianPoint negate() {
        return JacobianPoint(x, mod(-y), z);
    }

    pure @nogc @nothrow
    JacobianPoint double_mult() {
        auto X1 = this.x;
        auto Y1 = this.y;
        auto Z1 = this.z;
        auto A = mod(powmod(X1, TWO, ZERO));
        auto B = mod(powmod(Y1, TWO, ZERO));
        auto C = mod(powmod(B, TWO, ZERO));
        auto D = mod(TWO * (mod(mod(powmod(X1+B,TWO,ZERO))) - A - C));
        auto E = mod(THREE * A);
        auto F = mod(powmod(E, TWO, ZERO));
        auto X3 = mod(F - TWO * D);
        auto Y3 = mod(E * (D - X3) - EIGHT * C);
        auto Z3 = mod(TWO * Y1 * Z1);
        return JacobianPoint(X3, Y3, Z3);
    }

    pure @nogc @nothrow
    JacobianPoint add(JacobianPoint other) {
        auto X1 = this.x;
        auto Y1 = this.y;
        auto Z1 = this.z;
        auto X2 = other.x;
        auto Y2 = other.y;
        auto Z2 = other.z;
        if (X2 == ZERO || Y2 == ZERO)
            return this;
        if (X1 == ZERO || Y1 == ZERO)
            return other;
        auto Z1Z1 = powmod(Z1, TWO, P);
        auto Z2Z2 = powmod(Z2, TWO, P);
        auto U1 = mod(X1 * Z2Z2);
        auto U2 = mod(X2 * Z1Z1);
        auto S1 = mod(Y1 * Z2 * Z2Z2);
        auto S2 = mod(mod(Y2 * Z1) * Z1Z1);
        auto H = mod(U2 - U1);
        auto r = mod(S2 - S1);
        if (H == ZERO) {
            if (r == ZERO)
                return this.double_mult();
            else
                return JacobianPoint.ZERO_J;
        }
        auto HH = powmod(H, TWO, P);
        auto HHH = mod(H * HH);
        auto V = mod(U1 * HH);
        auto X3 = mod(powmod(r,TWO,ZERO) - HHH - TWO * V);
        auto Y3 = mod(r * (V - X3) - S1 * HHH);
        auto Z3 = mod(Z1 * Z2 * H);
        return JacobianPoint(X3, Y3, Z3);
    }

    pure @nogc @nothrow
    JacobianPoint multiplyUnsafe(BigInt n) {
        auto t = splitScalarEndo(n);
        auto k1p = JacobianPoint.ZERO_J;
        auto k2p = JacobianPoint.ZERO_J;
        auto d = this;
        while (t[1] > ZERO || t[3] > ZERO) {
            if (t[1].testBit(0))
                k1p = k1p.add(d);
            if (t[3].testBit(0))
                k2p = k2p.add(d);
            d = d.double_mult();
            t[1] = t[1].shr(1);
            t[3] = t[3].shr(1);
        }
        if (t[0])
            k1p = k1p.negate();
        if (t[2])
            k2p = k2p.negate();
        k2p = JacobianPoint(mod(k2p.x * BETA), k2p.y, k2p.z);
        return k1p.add(k2p);
    }
}

static ZERO_J = JacobianPoint(ZERO, ONE, ZERO);
static BASE_J = JacobianPoint(GX, GY, ONE);

class Point(BigInt x, BigInt y) {
    JacobianPoint toPrejective() {
        return JacobianPoint(x, y, ONE);
    }

    Point multiply(BigInt scalar) {
        return this.toProjective().multiplyUnsafe(scalar).toAffine();
    }
}

static ZERO_P = Point(ZERO, ZERO);
static BASE_P = Point(GX, GY);
