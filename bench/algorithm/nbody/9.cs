namespace nbody
{
    /*  The Computer Language Benchmarks Game
        https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

        contributed by Isaac Gouy
        modified by Robert F. Tobler
        modified by Eric P. Nusbaum
        modified by hanabi1224 to use simd-powered Vector
    */

    using System;
    using System.Numerics;

    public class NBody
    {
        public static void Main(String[] args)
        {
            var n = args.Length > 0 ? int.Parse(args[0]) : 10000;
            NBodySystem sys = new NBodySystem();
            sys.OffsetMomentum();
            Console.WriteLine("{0:f9}", sys.Energy());
            for (var i = 0; i < n; i++)
            {
                sys.Advance(0.01);
            }
            Console.WriteLine("{0:f9}", sys.Energy());
        }
    }

    public class Body
    {
        public Vector<double> Pos { get; set; }
        public Vector<double> Velocity { get; set; }
        public double Mass { get; }

        public Body(double x, double y, double z, double vx, double vy, double vz, double mass)
        {
            Pos = new Vector<double>(new[] { x, y, z, 0 });
            Velocity = new Vector<double>(new[] { vx, vy, vz, 0 });
            Mass = mass;
        }
    }


    public class NBodySystem
    {
        private Body[] _bodies;

        const byte bodyCount = 5;

        const double Pi = 3.141592653589793;
        const double Solarmass = 4 * Pi * Pi;
        const double Solarmass_inv = 1 / Solarmass;
        const double DaysPeryear = 365.24;

        public NBodySystem()
        {
            _bodies = new[]
            {
                // Sun
                new Body(
                    x : 0,
                    y : 0,
                    z : 0,
                    vx : 0,
                    vy :0,
                    vz : 0,
                    mass:Solarmass
                ),
                // Jupiter
                new Body(
                    x : 4.84143144246472090e+00,
                    y : -1.16032004402742839e+00,
                    z : -1.03622044471123109e-01,
                    vx : 1.66007664274403694e-03*DaysPeryear,
                    vy : 7.69901118419740425e-03*DaysPeryear,
                    vz : -6.90460016972063023e-05*DaysPeryear,
                    mass : 9.54791938424326609e-04*Solarmass
                ),
                // Saturn
                new Body(
                    x : 8.34336671824457987e+00,
                    y : 4.12479856412430479e+00,
                    z : -4.03523417114321381e-01,
                    vx : -2.76742510726862411e-03*DaysPeryear,
                    vy : 4.99852801234917238e-03*DaysPeryear,
                    vz : 2.30417297573763929e-05*DaysPeryear,
                    mass : 2.85885980666130812e-04*Solarmass
                ),
                // Uranus
                new Body(
                    x : 1.28943695621391310e+01,
                    y : -1.51111514016986312e+01,
                    z : -2.23307578892655734e-01,
                    vx : 2.96460137564761618e-03*DaysPeryear,
                    vy : 2.37847173959480950e-03*DaysPeryear,
                    vz : -2.96589568540237556e-05*DaysPeryear,
                    mass : 4.36624404335156298e-05*Solarmass
                ),
                // Neptune
                new Body(
                    x : 1.53796971148509165e+01,
                    y : -2.59193146099879641e+01,
                    z : 1.79258772950371181e-01,
                    vx : 2.68067772490389322e-03*DaysPeryear,
                    vy : 1.62824170038242295e-03*DaysPeryear,
                    vz : -9.51592254519715870e-05*DaysPeryear,
                    mass : 5.15138902046611451e-05*Solarmass
                ),
            };
        }

        public void OffsetMomentum()
        {
            var p = new Vector<double>(new[] { 0.0, 0.0, 0.0, 0.0 });
            foreach (var b in _bodies)
            {
                p -= b.Velocity * b.Mass;
            }
            _bodies[0].Velocity = p * Solarmass_inv;
        }

        public void Advance(double dt)
        {
            for (var i = 0; i < bodyCount - 1; i++)
            {
                var bi = _bodies[i];
                var v = bi.Velocity;
                for (var j = i + 1; j < bodyCount; j++)
                {
                    var bj = _bodies[j];
                    var dpos = bi.Pos - bj.Pos;
                    double d2 = Vector.Dot(dpos, dpos);
                    double mag = dt / (d2 * Math.Sqrt(d2));
                    v -= dpos * bj.Mass * mag;
                    bj.Velocity += dpos * bi.Mass * mag;
                }
                bi.Velocity = v;
            }
            foreach (var b in _bodies)
            {
                b.Pos += b.Velocity * dt;
            }
        }

        public double Energy()
        {
            double e = 0.0;
            for (int i = 0; i < bodyCount; i++)
            {
                var bi = _bodies[i];
                e += 0.5 * bi.Mass * Vector.Dot(bi.Velocity, bi.Velocity);
                for (int j = i + 1; j < bodyCount; j++)
                {
                    var bj = _bodies[j];
                    var dpos = bi.Pos - bj.Pos;
                    e -= (bi.Mass * bj.Mass) / Math.Sqrt(Vector.Dot(dpos, dpos));
                }
            }
            return e;
        }
    }
}
