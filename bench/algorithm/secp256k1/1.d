@safe:

import std;

static ZERO = BigInt(0);
static ONE = BigInt(1);
static TWO = BigInt(2);
static THREE = BigInt(3);
static EIGHT = BigInt(8);
static P = BigInt("115792089237316195423570985008687907853269984665640564039457584007908834671663");
static N = BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337");
static GX = BigInt("55066263022277343669578718895168534326250603453777594175500187360389116729240");
static GY = BigInt("32670510020758816978083085130507043184471273380659243275938904335757337482424");
static BETA = BigInt("55594575648329892869085402983802832744385952214688224221778511981742606582254");
static POW_2_128 = BigInt("340282366920938463463374607431768211456");
static A1 = BigInt("64502973549206556628585045361533709077");
static B1 = BigInt("-303414439467246543595250775667605759171");
static A2 = BigInt("367917413016453100223835821029139468248");
static PRIVATE_KEY = BigInt("20775598474904240222758871485654738649026525153462921990999819694398496339603");

void main(string[] args)
{

    auto n = args.length > 1 ? args[1].to!int : 1;
    Point p;
    auto point = p.BASE_P();
    foreach(_; 0..n)
        point = point.multiply(PRIVATE_KEY);
    writefln("%s,%s",replace(toLower(point.x.toHex()),"_",""), replace(toLower(point.y.toHex()),"_",""));
}

//Point ZERO_P = Point(ZERO, ZERO);
//Point BASE_P = Point(GX, GY);

//JacobianPoint ZERO_J = JacobianPoint(ZERO, ONE, ZERO);
//JacobianPoint BASE_J = JacobianPoint(GX, GY, ONE);

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

BigInt divNearest(BigInt a, BigInt b) {
    return (a + b / TWO) / b;
}

auto splitScalarEndo (BigInt k) {
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


struct JacobianPoint {
    BigInt x,y,z;

    this(BigInt X, BigInt Y, BigInt Z) {
        this.x = X;
        this.y = Y;
        this.z = Z;
    }

    JacobianPoint ZERO_J() {
        return JacobianPoint(ZERO, ONE, ZERO);
    }

    JacobianPoint BASE_J() {
        return JacobianPoint(GX, GY, ONE);
    }

    JacobianPoint fromAffine(Point p) {
        return JacobianPoint(p.x, p.y, ONE);
    }

    Point toAffine() {
        auto invZ = invert(z);
        auto invZ2 = invZ * invZ;
        auto x = mod(x * invZ2);
        auto y = mod(y * invZ * invZ2);
        return Point(x, y);
    }

    JacobianPoint negate() {
        return JacobianPoint(x, mod(-y), z);
    }

    JacobianPoint double_mul() {
        auto X1 = this.x;
        auto Y1 = this.y;
        auto Z1 = this.z;
        auto A = mod(X1 * X1);
        auto B = mod(Y1 * Y1);
        auto C = mod(B * B);
        auto D = mod(TWO * (mod(mod((X1+B) ^^ 2)) - A - C));
        auto E = mod(THREE * A);
        auto F = mod(E ^^ 2);
        auto X3 = mod(F - TWO * D);
        auto Y3 = mod(E * (D - X3) - EIGHT * C);
        auto Z3 = mod(TWO * Y1 * Z1);
        return JacobianPoint(X3, Y3, Z3);
    }

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
        auto Z1Z1 = mod(Z1 ^^ 2);
        auto Z2Z2 = mod(Z2 ^^ 2);
        auto U1 = mod(X1 * Z2Z2);
        auto U2 = mod(X2 * Z1Z1);
        auto S1 = mod(Y1 * Z2 * Z2Z2);
        auto S2 = mod(mod(Y2 * Z1) * Z1Z1);
        auto H = mod(U2 - U1);
        auto r = mod(S2 - S1);
        // H = 0 meaning it's the same point
        if (H == ZERO) {
            if (r == ZERO)
                return this.double_mul();
            else
                return JacobianPoint.ZERO_J();
        }
        auto HH = mod(H ^^ 2);
        auto HHH = mod(H * HH);
        auto V = mod(U1 * HH);
        auto X3 = mod(r ^^ 2 - HHH - TWO * V);
        auto Y3 = mod(r * (V - X3) - S1 * HHH);
        auto Z3 = mod(Z1 * Z2 * H);
        return JacobianPoint(X3, Y3, Z3);
    }

    JacobianPoint multiplyUnsafe(BigInt n) {
        bool k1neg, k2neg;
        BigInt k1, k2;
        AliasSeq!(k1neg,k1,k2neg,k2)  = splitScalarEndo(n);
        auto k1p = JacobianPoint.ZERO_J();
        auto k2p = JacobianPoint.ZERO_J();
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

    Point ZERO_P() {
        return Point(ZERO, ZERO);
    }

    Point BASE_P() {
        return Point(GX, GY);
    }

    JacobianPoint toPrejective() {
        return JacobianPoint(this.x, this.y, ONE);
    }

    Point multiply(BigInt scalar) {
        JacobianPoint tmp;
        return tmp.fromAffine(this).multiplyUnsafe(scalar).toAffine();
    }
}

