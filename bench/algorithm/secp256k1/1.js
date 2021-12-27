// Algrithms come from https://paulmillr.com/posts/noble-secp256k1-fast-ecc/

const CURVE = {
    P: 2n ** 256n - 2n ** 32n - 977n,
    n: 2n ** 256n - 432420386565659656852420866394968145599n
};
class Point {
    static ZERO = new Point(0n, 0n); // Point at infinity aka identity point aka zero
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
    // Adds point to itself. http://hyperelliptic.org/EFD/g1p/auto-shortw.html
    double() {
        const X1 = this.x;
        const Y1 = this.y;
        const lam = mod(3n * X1 ** 2n * invert(2n * Y1, CURVE.P));
        const X3 = mod(lam * lam - 2n * X1);
        const Y3 = mod(lam * (X1 - X3) - Y1);
        return new Point(X3, Y3);
    }
    // Adds point to other point. http://hyperelliptic.org/EFD/g1p/auto-shortw.html
    add(other) {
        const [a, b] = [this, other];
        const [X1, Y1, X2, Y2] = [a.x, a.y, b.x, b.y];
        if (X1 === 0n || Y1 === 0n) return b;
        if (X2 === 0n || Y2 === 0n) return a;
        if (X1 === X2 && Y1 === Y2) return this.double();
        if (X1 === X2 && Y1 === -Y2) return Point.ZERO;
        const lam = mod((Y2 - Y1) * invert(X2 - X1, CURVE.P));
        const X3 = mod(lam * lam - X1 - X2);
        const Y3 = mod(lam * (X1 - X3) - Y1);
        return new Point(X3, Y3);
    }

    multiplyDA(n) {
        let p = Point.ZERO;
        let d = this;
        while (n > 0n) {
            if (n & 1n) p = p.add(d);
            d = d.double();
            n >>= 1n;
        }
        return p;
    }
}
function mod(a, b = CURVE.P) {
    const result = a % b;
    return result >= 0 ? result : b + result;
}
// Inverses number over modulo
function invert(number, modulo = CURVE.P) {
    if (number === 0n || modulo <= 0n) {
        throw new Error(`invert: expected positive integers, got n=${number} mod=${modulo}`);
    }
    // Eucledian GCD https://brilliant.org/wiki/extended-euclidean-algorithm/
    let a = mod(number, modulo);
    let b = modulo;
    let [x, y, u, v] = [0n, 1n, 1n, 0n];
    while (a !== 0n) {
        const q = b / a;
        const r = b % a;
        const m = x - u * q;
        const n = y - v * q;
        [b, a] = [a, r];
        [x, y] = [u, v];
        [u, v] = [m, n];
    }
    const gcd = b;
    if (gcd !== 1n) throw new Error('invert: does not exist');
    return mod(x, modulo);
}

// G x, y values taken from official secp256k1 document
CURVE.Gx = 55066263022277343669578718895168534326250603453777594175500187360389116729240n;
CURVE.Gy = 32670510020758816978083085130507043184471273380659243275938904335757337482424n;

const G = new Point(CURVE.Gx, CURVE.Gy);
const PRIVATE_KEY = 0x2DEE927079283C3C4FCA3EF970FF4D38B64592E3FE0AB0DAD9132D70B5BC7693n;

function main() {
    const n = +process.argv[2] || 1;
    let point = G;
    for (let i = 0; i < n; i++) {
        point = point.multiplyDA(PRIVATE_KEY);
    }
    console.log(`${point.x.toString(16)},${point.y.toString(16)}`);
}

main();
