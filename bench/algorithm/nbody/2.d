@safe:
import std;

const Solarmass = 4 * PI * PI;
const DaysPeryear = 365.24;

Body[5] bodies = [
    // Sun
    new Body(
        0,
        0,
        0,
        0,
        0,
        0,
        Solarmass),
    // Jupiter
    new Body(
        4.84143144246472090e+00,
        -1.16032004402742839e+00,
        -1.03622044471123109e-01,
        1.66007664274403694e-03 * DaysPeryear,
        7.69901118419740425e-03 * DaysPeryear,
        -6.90460016972063023e-05 * DaysPeryear,
        9.54791938424326609e-04 * Solarmass),
    // Saturn
    new Body(
        8.34336671824457987e+00,
        4.12479856412430479e+00,
        -4.03523417114321381e-01,
        -2.76742510726862411e-03 * DaysPeryear,
        4.99852801234917238e-03 * DaysPeryear,
        2.30417297573763929e-05 * DaysPeryear,
        2.85885980666130812e-04 * Solarmass),
    // Uranus
    new Body(
        1.28943695621391310e+01,
        -1.51111514016986312e+01,
        -2.23307578892655734e-01,
        2.96460137564761618e-03 * DaysPeryear,
        2.37847173959480950e-03 * DaysPeryear,
        -2.96589568540237556e-05 * DaysPeryear,
        4.36624404335156298e-05 * Solarmass),
    // Neptune
    new Body(
        1.53796971148509165e+01,
        -2.59193146099879641e+01,
        1.79258772950371181e-01,
        2.68067772490389322e-03 * DaysPeryear,
        1.62824170038242295e-03 * DaysPeryear,
        -9.51592254519715870e-05 * DaysPeryear,
        5.15138902046611451e-05 * Solarmass),
].staticArray;

class Body
{
    double[3] pos;
    double[3] velocity;
    double mass;

    this(double x, double y, double z, double vx, double vy, double vz, double mass)
    {
        this.pos = [x, y, z].staticArray;
        this.velocity = [vx, vy, vz].staticArray;
        this.mass = mass;
    }
}

void addMut(ref double[3] a, ref double[3] b)
{
    foreach (int i; 0 .. 3)
    {
        a[i] += b[i];
    }
}

void subMut(ref double[3] a, ref double[3] b)
{
    foreach (int i; 0 .. 3)
    {
        a[i] -= b[i];
    }
}

double[3] sub(ref double[3] a, ref double[3] b)
{
    double[3] r;
    foreach (int i; 0 .. 3)
    {
        r[i] = a[i] - b[i];
    }
    return r;
}

double[3] mulS(double[3] a, double b)
{
    foreach (int i; 0 .. 3)
    {
        a[i] *= b;
    }
    return a;
}

double dot(ref double[3] a, ref double[3] b)
{
    double r = 0.0;
    foreach (int i; 0 .. 3)
    {
        r += a[i] * b[i];
    }
    return r;
}

void advance(ref Body[5] bodies, double dt)
{
    foreach (int i; 0 .. bodies.length)
    {
        auto bi = bodies[i];
        foreach (int j; (i + 1) .. bodies.length)
        {
            auto bj = bodies[j];
            auto dpos = sub(bi.pos, bj.pos);
            auto d2 = dot(dpos, dpos);
            auto mag = dt / (d2 * std.math.sqrt(d2));
            auto tmp = mulS(dpos, mag * bj.mass);
            subMut(bi.velocity, tmp);
            tmp = mulS(dpos, mag * bi.mass);
            addMut(bj.velocity, tmp);
        }
        auto tmp = mulS(bi.velocity, dt);
        addMut(bi.pos, tmp);
    }
}

void offsetMomentum(ref Body[5] bodies)
{
    double[3] p = [0.0, 0.0, 0.0].staticArray;
    foreach (Body b; bodies)
    {
        auto tmp = mulS(b.velocity, b.mass);
        subMut(p, tmp);
    }
    auto sol = bodies[0];
    sol.velocity = mulS(p, 1.0 / Solarmass);
}

double energy(ref Body[5] bodies)
{
    auto e = 0.0;
    foreach (int i; 0 .. bodies.length)
    {
        auto bi = bodies[i];
        e += 0.5 * bi.mass * dot(bi.velocity, bi.velocity);
        foreach (int j; (i + 1) .. bodies.length)
        {
            auto bj = bodies[j];
            auto dpos = sub(bi.pos, bj.pos);
            e -= (bi.mass * bj.mass) / std.math.sqrt(dot(dpos, dpos));
        }
    }
    return e;
}

void main(string[] args)
{
    auto n = args.length > 1 ? args[1].to!int() : 1000;
    offsetMomentum(bodies);
    std.writefln("%.9f", energy(bodies));
    foreach (int i; 0 .. n)
    {
        advance(bodies, 0.01);
    }
    std.writefln("%.9f", energy(bodies));
}
