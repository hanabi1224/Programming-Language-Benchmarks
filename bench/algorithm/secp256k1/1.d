@safe:

import std;

static immutable ZERO = BigInt(0);
static immutable ONE = BigInt(1);
static immutable TWO = BigInt(2);
static immutable THREE = BigInt(3);
static immutable EIGHT = BigInt(8);
static immutable P = BigInt("115792089237316195423570985008687907853269984665640564039457584007908834671663");
static immutable N = BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337");
static immutable BETA = BigInt("55594575648329892869085402983802832744385952214688224221778511981742606582254");
static immutable POW_2_128 = BigInt("340282366920938463463374607431768211456");
static immutable A1 = BigInt("64502973549206556628585045361533709077");
static immutable B1 = BigInt("-303414439467246543595250775667605759171");
static immutable A2 = BigInt("367917413016453100223835821029139468248");
static immutable PRIVATE_KEY = BigInt("20775598474904240222758871485654738649026525153462921990999819694398496339603");
static ZERO_J = JacobianPoint(BigInt(0), BigInt(1), BigInt(0));
static point = Point(BigInt("55066263022277343669578718895168534326250603453777594175500187360389116729240"),
                     BigInt("32670510020758816978083085130507043184471273380659243275938904335757337482424"));

void main(string[] args)
{
    auto n = args.length > 1 ? args[1].to!int : 1;
    foreach(_; 0..n)
        point = point * PRIVATE_KEY;
    writefln("%s,%s",replace(toLower(point.x.toHex()),"_",""), replace(toLower(point.y.toHex()),"_",""));
}

pure
BigInt mod(BigInt a, BigInt b = P) {
    auto r = a % b;
    return r < ZERO ? r + b : r;
}

pure
BigInt invert(BigInt number, BigInt modulo = P) {
    BigInt a = mod(number, modulo);
    BigInt b = modulo;
    BigInt x = ZERO;
    BigInt y = ONE;
    BigInt u = ONE;
    BigInt v = ZERO;
    BigInt q,r,m,n;
    while (a != ZERO) {
        q = b / a;
        r = b % a;
        m = x - u * q;
        n = y - v * q;
        b = a;
        a = r;
        x = u;
        y = v;
        u = m;
        v = n;
    }
    return mod(x, modulo);
}

pure
BigInt divNearest(BigInt a, BigInt b) {
    return (a + b / TWO) / b;
}

pure
auto splitScalarEndo (BigInt k) {
    BigInt b2 = A1;
    BigInt c1 = divNearest(b2 * k, N);
    BigInt c2 = divNearest(-B1 * k, N);
    BigInt k1 = mod(k - c1 * A1 - c2 * A2, N);
    BigInt k2 = mod(-c1 * B1 - c2 * b2, N);
    bool k1neg = k1 > POW_2_128;
    bool k2neg = k2 > POW_2_128;
    if (k1neg)
        k1 = N - k1;
    if (k2neg)
        k2 = N - k2;
    return tuple(k1neg, k1, k2neg, k2);
}

struct JacobianPoint {
    BigInt x,y,z;

    this(BigInt X, BigInt Y, BigInt Z) {
        this.x = X;
        this.y = Y;
        this.z = Z;
    }

    JacobianPoint fromAffine(Point p) {
        return JacobianPoint(p.x, p.y, ONE);
    }

    Point toAffine() {
        auto invZ = invert(z);
        auto invZ2 = invZ ^^ 2;
        auto x = mod(x * invZ2);
        auto y = mod(y * invZ * invZ2);
        return Point(x, y);
    }

    JacobianPoint negate() {
        return JacobianPoint(x, mod(-y), z);
    }

    JacobianPoint double_mul() {
        BigInt A = mod(this.x ^^ 2);
        BigInt B = mod(this.y ^^ 2);
        BigInt C = mod(B ^^ 2);
        BigInt D = mod(TWO * (mod(mod((this.x+B) ^^ 2)) - A - C));
        BigInt E = mod(THREE * A);
        BigInt F = mod(E ^^ 2);
        BigInt X3 = mod(F - TWO * D);
        BigInt Y3 = mod(E * (D - X3) - EIGHT * C);
        BigInt Z3 = mod(TWO * this.y * this.z);
        return JacobianPoint(X3, Y3, Z3);
    }

    JacobianPoint add(JacobianPoint other) {
        if (other.x == ZERO || other.y == ZERO)
            return this;
        if (this.x == ZERO || this.y == ZERO)
            return other;
        BigInt Z1Z1 = mod(this.z ^^ 2);
        BigInt Z2Z2 = mod(other.z ^^ 2);
        BigInt U1 = mod(this.x * Z2Z2);
        BigInt U2 = mod(other.x * Z1Z1);
        BigInt S1 = mod(this.y * other.z * Z2Z2);
        BigInt S2 = mod(mod(other.y * this.z) * Z1Z1);
        BigInt H = mod(U2 - U1);
        BigInt r = mod(S2 - S1);
        // H = 0 meaning it's the same point
        if (H == ZERO) {
            if (r == ZERO)
                return this.double_mul();
            else
                return ZERO_J;
        }
        BigInt HH = mod(H ^^ 2);
        BigInt HHH = mod(H * HH);
        BigInt V = mod(U1 * HH);
        BigInt X3 = mod(r ^^ 2 - HHH - TWO * V);
        BigInt Y3 = mod(r * (V - X3) - S1 * HHH);
        BigInt Z3 = mod(this.z * other.z * H);
        return JacobianPoint(X3, Y3, Z3);
    }

    JacobianPoint multiplyUnsafe(BigInt n) {
        bool k1neg, k2neg;
        BigInt k1, k2;
        AliasSeq!(k1neg,k1,k2neg,k2)  = splitScalarEndo(n);
        auto k1p = ZERO_J;
        auto k2p = ZERO_J;
        auto d = this;
        while (k1 > ZERO || k2 > ZERO) {
            if (k1 & 1)
                k1p = k1p.add(d);
            if (k2 & 1)
                k2p = k2p.add(d);
            d = d.double_mul();
            k1 >>= 1;
            k2 >>= 1;
        }
        if (k1neg)
            k1p = k1p.negate();
        if (k2neg)
            k2p = k2p.negate();
        k2p = JacobianPoint(mod(k2p.x * BETA), k2p.y, k2p.z);
        return k1p.add(k2p);
    }
}

struct Point {
    BigInt x,y;

    this(BigInt X, BigInt Y) {
        this.x = X;
        this.y = Y;
    }

    JacobianPoint toPrejective() {
        return JacobianPoint(this.x, this.y, ONE);
    }

    Point opBinary(string op="*")(BigInt scalar) {
        JacobianPoint tmp;
        return tmp.fromAffine(this).multiplyUnsafe(scalar).toAffine();
    }
}

