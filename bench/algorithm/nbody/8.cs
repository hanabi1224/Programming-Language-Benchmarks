namespace nbody
{
    /*  The Computer Language Benchmarks Game
        https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

        contributed by Isaac Gouy
        modified by Robert F. Tobler
        modified by Eric P. Nusbaum
    */

    using System;

    public class NBody
    {
        public static void Main(String[] args)
        {
                int n = args.Length > 0 ? Int32.Parse(args[0]) : 10000;
                NBodySystem bodies = new NBodySystem();
                Console.WriteLine("{0:f9}", bodies.Energy());
                for (int i = 0; i < n; i++) bodies.Advance(0.01);
                Console.WriteLine("{0:f9}", bodies.Energy());
        }
    }

    public class Body { public double x, y, z, vx, vy, vz, mass; }
    

    public class NBodySystem
    {
        private Body[] _bodies;
        private Body[] _pairL;
        private Body[] _pairR;
        private byte bodyCount = 5;

        const double Pi = 3.141592653589793;
        const double Solarmass = 4 * Pi * Pi;
        const double DaysPeryear = 365.24;

        public NBodySystem()
        {
            _bodies = new[]
            {
                new Body()
                {
                    // Sun
                    mass = Solarmass,
                },
                new Body()
                {
                    // Jupiter
                    x = 4.84143144246472090e+00,
                    y = -1.16032004402742839e+00,
                    z = -1.03622044471123109e-01,
                    vx = 1.66007664274403694e-03*DaysPeryear,
                    vy = 7.69901118419740425e-03*DaysPeryear,
                    vz = -6.90460016972063023e-05*DaysPeryear,
                    mass = 9.54791938424326609e-04*Solarmass,
                },
                new Body()
                {
                    // Saturn
                    x = 8.34336671824457987e+00,
                    y = 4.12479856412430479e+00,
                    z = -4.03523417114321381e-01,
                    vx = -2.76742510726862411e-03*DaysPeryear,
                    vy = 4.99852801234917238e-03*DaysPeryear,
                    vz = 2.30417297573763929e-05*DaysPeryear,
                    mass = 2.85885980666130812e-04*Solarmass,
                },
                new Body()
                {
                    // Uranus
                    x = 1.28943695621391310e+01,
                    y = -1.51111514016986312e+01,
                    z = -2.23307578892655734e-01,
                    vx = 2.96460137564761618e-03*DaysPeryear,
                    vy = 2.37847173959480950e-03*DaysPeryear,
                    vz = -2.96589568540237556e-05*DaysPeryear,
                    mass = 4.36624404335156298e-05*Solarmass,
                },
                new Body()
                {
                    // Neptune
                    x = 1.53796971148509165e+01,
                    y = -2.59193146099879641e+01,
                    z = 1.79258772950371181e-01,
                    vx = 2.68067772490389322e-03*DaysPeryear,
                    vy = 1.62824170038242295e-03*DaysPeryear,
                    vz = -9.51592254519715870e-05*DaysPeryear,
                    mass = 5.15138902046611451e-05*Solarmass,
                },
            };

            _pairL = new Body[(bodyCount * (bodyCount - 1) / 2)];
            _pairR = new Body[(bodyCount * (bodyCount - 1) / 2)];
            var pi = 0;
            for (var i = 0; i < bodyCount - 1; i++)
                for (var j = i + 1; j < bodyCount; j++)
                {
                    _pairL[pi] = _bodies[i];
                    _pairR[pi] = _bodies[j];
                    pi++;
                }

        double px = 0.0, py = 0.0, pz = 0.0;
            foreach (var b in _bodies)
            {
                px += b.vx * b.mass; py += b.vy * b.mass; pz += b.vz * b.mass;
            }
            var sol = _bodies[0];
            sol.vx = -px / Solarmass; sol.vy = -py / Solarmass; sol.vz = -pz / Solarmass;
        }

        public void Advance(double dt)
        {
            var length = _pairL.Length;
            for (int i = 0; i < length; i++)
            {
                Body bi =  _pairL[i], bj = _pairR[i];
                double dx = bi.x - bj.x, dy = bi.y - bj.y, dz = bi.z - bj.z;
                double d2 = dx * dx + dy * dy + dz * dz;
                double mag = dt / (d2 * Math.Sqrt(d2));
                bi.vx -= dx * bj.mass * mag; bj.vx += dx * bi.mass * mag;
                bi.vy -= dy * bj.mass * mag; bj.vy += dy * bi.mass * mag;
                bi.vz -= dz * bj.mass * mag; bj.vz += dz * bi.mass * mag;
            }
            foreach (var b in _bodies)
            {
                b.x += dt * b.vx; b.y += dt * b.vy; b.z += dt * b.vz;
            }
        }

        public double Energy()
        {
            double e = 0.0;
            for (int i = 0; i < bodyCount; i++)
            {
                var bi = _bodies[i];
                e += 0.5 * bi.mass * (bi.vx * bi.vx + bi.vy * bi.vy + bi.vz * bi.vz);
                for (int j = i + 1; j < bodyCount; j++)
                {
                    var bj = _bodies[j];
                    double dx = bi.x - bj.x, dy = bi.y - bj.y, dz = bi.z - bj.z;
                    e -= (bi.mass * bj.mass) / Math.Sqrt(dx * dx + dy * dy + dz * dz);
                }
            }
            return e;
        }
    }
}
