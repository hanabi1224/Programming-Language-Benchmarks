// Ported from 1.ts

using System;
using System.Numerics;

public static class Program
{
    static readonly BigInteger ZERO = BigInteger.Zero;
    static readonly BigInteger ONE = BigInteger.One;
    static readonly BigInteger TWO = new BigInteger(2);
    static readonly BigInteger THREE = new BigInteger(3);
    static readonly BigInteger EIGHT = new BigInteger(8);
    static readonly BigInteger P = BigInteger.Parse("115792089237316195423570985008687907853269984665640564039457584007908834671663");
    static readonly BigInteger N = BigInteger.Parse("115792089237316195423570985008687907852837564279074904382605163141518161494337");
    static readonly BigInteger GX = BigInteger.Parse("55066263022277343669578718895168534326250603453777594175500187360389116729240");
    static readonly BigInteger GY = BigInteger.Parse("32670510020758816978083085130507043184471273380659243275938904335757337482424");
    static readonly BigInteger BETA = BigInteger.Parse("55594575648329892869085402983802832744385952214688224221778511981742606582254");
    static readonly BigInteger POW_2_128 = BigInteger.Parse("340282366920938463463374607431768211456");
    static readonly BigInteger A1 = BigInteger.Parse("64502973549206556628585045361533709077");
    static readonly BigInteger B1 = BigInteger.Parse("-303414439467246543595250775667605759171");
    static readonly BigInteger A2 = BigInteger.Parse("367917413016453100223835821029139468248");
    static readonly BigInteger PRIVATE_KEY = BigInteger.Parse("20775598474904240222758871485654738649026525153462921990999819694398496339603");

    public static void Main(string[] args)
    {
        var n = args.Length > 0 ? int.Parse(args[0]) : 1;
        var point = Point.BASE;
        for (var i = 0; i < n; i++)
        {
            point = point.multiply(PRIVATE_KEY);
        }
        Console.WriteLine($"{point.X.ToHexString()},{point.Y.ToHexString()}");
    }

    static BigInteger mod(BigInteger a, BigInteger b)
    {
        var r = a % b;
        return r < 0 ? r + b : r;
    }

    static BigInteger mod(BigInteger a)
    {
        return mod(a, P);
    }

    static BigInteger invert(BigInteger number, BigInteger modulo)
    {
        var a = mod(number, modulo);
        var b = modulo;
        var (x, y, u, v) = (ZERO, ONE, ONE, ZERO);
        while (a != ZERO)
        {
            var q = b / a;
            var r = b % a;
            var m = x - u * q;
            var n = y - v * q;
            (b, a) = (a, r);
            (x, y) = (u, v);
            (u, v) = (m, n);
        }
        return mod(x, modulo);
    }

    static BigInteger divNearest(BigInteger a, BigInteger b)
    {
        return (a + b / 2) / b;
    }

    static (bool, BigInteger, bool, BigInteger) splitScalarEndo(BigInteger k)
    {
        var b2 = A1;
        var c1 = divNearest(b2 * k, N);
        var c2 = divNearest(-B1 * k, N);
        var k1 = mod(k - c1 * A1 - c2 * A2, N);
        var k2 = mod(-c1 * B1 - c2 * b2, N);
        var k1neg = k1 > POW_2_128;
        var k2neg = k2 > POW_2_128;
        if (k1neg) k1 = N - k1;
        if (k2neg) k2 = N - k2;
        return (k1neg, k1, k2neg, k2);
    }

    static BigInteger invert(BigInteger number)
    {
        return invert(number, P);
    }

    struct JacobianPoint
    {
        public static readonly JacobianPoint ZERO_J = new JacobianPoint(ZERO, ONE, ZERO);
        public static readonly JacobianPoint BASE_J = new JacobianPoint(GX, GY, ONE);

        public BigInteger X { get; private set; }
        public BigInteger Y { get; private set; }
        public BigInteger Z { get; private set; }

        public JacobianPoint(BigInteger x, BigInteger y, BigInteger z)
        {
            X = x; Y = y; Z = z;
        }

        public static JacobianPoint FromAffine(Point p)
        {
            return new JacobianPoint(p.X, p.Y, ONE);
        }

        public Point ToAffine()
        {
            var inv_z = invert(Z);
            var inv_z_pow = inv_z.Pow(2);
            var x = mod(X * inv_z_pow);
            var y = mod(Y * inv_z * inv_z_pow);
            return new Point(x, y);
        }

        public JacobianPoint negate()
        {
            return new JacobianPoint(X, mod(-Y), Z);
        }

        public JacobianPoint dbl()
        {
            var X1 = X;
            var Y1 = Y;
            var Z1 = Z;
            var A = mod(X1.Pow(2));
            var B = mod(Y1.Pow(2));
            var C = mod(B.Pow(2));
            var D = mod(TWO * (mod(mod((X1 + B).Pow(2))) - A - C));
            var E = mod(THREE * A);
            var F = mod(E.Pow(2));
            var X3 = mod(F - TWO * D);
            var Y3 = mod(E * (D - X3) - EIGHT * C);
            var Z3 = mod(TWO * Y1 * Z1);
            return new JacobianPoint(X3, Y3, Z3);
        }

        JacobianPoint add(JacobianPoint other)
        {
            var X1 = this.X;
            var Y1 = this.Y;
            var Z1 = this.Z;
            var X2 = other.X;
            var Y2 = other.Y;
            var Z2 = other.Z;
            if (X2 == ZERO || Y2 == ZERO) return this;
            if (X1 == ZERO || Y1 == ZERO) return other;
            var Z1Z1 = mod(Z1.Pow(2));
            var Z2Z2 = mod(Z2.Pow(2));
            var U1 = mod(X1 * Z2Z2);
            var U2 = mod(X2 * Z1Z1);
            var S1 = mod(Y1 * Z2 * Z2Z2);
            var S2 = mod(mod(Y2 * Z1) * Z1Z1);
            var H = mod(U2 - U1);
            var r = mod(S2 - S1);
            // H = 0 meaning it's the same point.
            if (H == ZERO)
            {
                if (r == ZERO)
                {
                    return this.dbl();
                }
                else
                {
                    return JacobianPoint.ZERO_J;
                }
            }
            var HH = mod(H.Pow(2));
            var HHH = mod(H * HH);
            var V = mod(U1 * HH);
            var X3 = mod(r.Pow(2) - HHH - TWO * V);
            var Y3 = mod(r * (V - X3) - S1 * HHH);
            var Z3 = mod(Z1 * Z2 * H);
            return new JacobianPoint(X3, Y3, Z3);
        }

        public JacobianPoint multiplyUnsafe(BigInteger n)
        {
            var (k1neg, k1, k2neg, k2) = splitScalarEndo(n);
            var k1p = JacobianPoint.ZERO_J;
            var k2p = JacobianPoint.ZERO_J;
            var d = this;
            while (k1 > ZERO || k2 > ZERO)
            {
                if (!k1.IsEven) k1p = k1p.add(d);
                if (!k2.IsEven) k2p = k2p.add(d);
                d = d.dbl();
                k1 >>= 1;
                k2 >>= 1;
            }
            if (k1neg) k1p = k1p.negate();
            if (k2neg) k2p = k2p.negate();
            k2p = new JacobianPoint(mod(k2p.X * BETA), k2p.Y, k2p.Z);
            return k1p.add(k2p);
        }
    }

    struct Point
    {
        public static readonly Point ZERO = new Point(0, 0);
        public static readonly Point BASE = new Point(GX, GY);
        public BigInteger X { get; private set; }
        public BigInteger Y { get; private set; }

        public Point(BigInteger x, BigInteger y)
        {
            X = x; Y = y;
        }
        public Point multiply(BigInteger scalar)
        {
            return JacobianPoint.FromAffine(this).multiplyUnsafe(scalar).ToAffine();
        }
    }
}

static class Extensions
{
    public static BigInteger Pow(this BigInteger i, int exp)
    {
        return BigInteger.Pow(i, exp);
    }

    public static string ToHexString(this BigInteger i)
    {
        var s = i.ToString("x");
        return s.Length > 64 ? s.Substring(s.Length - 64) : s;
    }
}
