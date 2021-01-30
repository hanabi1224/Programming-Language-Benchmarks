namespace nbody {
   /**
    * The Computer Language Benchmarks Game
    * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
    *
    * Contributed by Derek Ziemba
    *
    * Optimized using:
    * - Unmanaged stack allocated structs (zero memory allocation)
    * - AVX Vector Intrinsics targeting IvyBridge ix-3xxx series cpus
    */
    using System;
    using System.Runtime.CompilerServices;
    using System.Runtime.Intrinsics;
    using System.Runtime.Intrinsics.X86;

    using static System.Runtime.CompilerServices.MethodImplOptions;

    using V256d = System.Runtime.Intrinsics.Vector256<double>;
    using V128d = System.Runtime.Intrinsics.Vector128<double>;

    public static unsafe class NBody {
        const MethodImplOptions AllOptimizations =
            AggressiveInlining | AggressiveOptimization;

        struct Body {
            public V256d Position;
            public V256d Velocity;
            public V256d Mass;

            public Body(double x, double y, double z,
                        double vx, double vy, double vz, double mass) {
                this.Position = Vector256.Create(x, y, z, 0d);
                this.Velocity = Vector256.Create(vx, vy, vz, 0d);
                this.Mass = Vector256.Create(mass, mass, mass, 0d);
            }
        }

        public static void Main(string[] args) {
            unchecked {
                const double SOLAR_MASS = 4 * Math.PI * Math.PI;
                const double DAYS_PER_YEAR = 365.24;

                Body* system = stackalloc Body[5];
                Body* last = system + 4;

                /**** SUN ****/
                system[0] =
                    new Body(x: 0,
                             y: 0,
                             z: 0,
                             vx: 0,
                             vy: 0,
                             vz: 0,
                             mass: SOLAR_MASS);

                /**** JUPITER ****/
                system[1] =
                    new Body(x: 4.84143144246472090e+00,
                             y: -1.16032004402742839e+00,
                             z: -1.03622044471123109e-01,
                             vx: 1.66007664274403694e-03 * DAYS_PER_YEAR,
                             vy: 7.69901118419740425e-03 * DAYS_PER_YEAR,
                             vz: -6.90460016972063023e-05 * DAYS_PER_YEAR,
                             mass: 9.54791938424326609e-04 * SOLAR_MASS);

                /**** SATURN ****/
                system[2] =
                    new Body(x: 8.34336671824457987e+00,
                             y: 4.12479856412430479e+00,
                             z: -4.03523417114321381e-01,
                             vx: -2.76742510726862411e-03 * DAYS_PER_YEAR,
                             vy: 4.99852801234917238e-03 * DAYS_PER_YEAR,
                             vz: 2.30417297573763929e-05 * DAYS_PER_YEAR,
                             mass: 2.85885980666130812e-04 * SOLAR_MASS);

                /**** URANUS ****/
                system[3] =
                    new Body(x: 1.28943695621391310e+01,
                             y: -1.51111514016986312e+01,
                             z: -2.23307578892655734e-01,
                             vx: 2.96460137564761618e-03 * DAYS_PER_YEAR,
                             vy: 2.37847173959480950e-03 * DAYS_PER_YEAR,
                             vz: -2.96589568540237556e-05 * DAYS_PER_YEAR,
                             mass: 4.36624404335156298e-05 * SOLAR_MASS);

                /**** NEPTUNE ****/
                system[4] =
                    new Body(x: 1.53796971148509165e+01,
                             y: -2.59193146099879641e+01,
                             z: 1.79258772950371181e-01,
                             vx: 2.68067772490389322e-03 * DAYS_PER_YEAR,
                             vy: 1.62824170038242295e-03 * DAYS_PER_YEAR,
                             vz: -9.51592254519715870e-05 * DAYS_PER_YEAR,
                             mass: 5.15138902046611451e-05 * SOLAR_MASS);

                OffsetMomentumAVX(system, last);

                Console.WriteLine(EnergyAVX(system, last).ToString("F9"));

                RunSimulation(args.Length > 0 ? Int32.Parse(args[0]) : 10000,
                              0.01d,
                              system,
                              last);

                Console.WriteLine(EnergyAVX(system, last).ToString("F9"));
            }//END unchecked
        }//END Method Main

        private static void
        OffsetMomentumAVX(Body* system, Body* last) {
            unchecked {
                V256d velocity = system->Velocity;
                for (Body* bi = system + 1; bi <= last; ++bi) {
                    velocity =
                        Avx.Add(velocity,
                                Avx.Multiply(bi->Velocity,
                                             bi->Mass));
                }//END of i loop
                system->Velocity =
                    Avx.Divide(velocity,
                        Avx.Multiply(Vector256.Create(-1d),
                                     system->Mass)).WithElement(3, 0d);
            }//END unchecked
        }//END Method OffsetMomentumAVX

        private static double
        EnergyAVX(Body* bi, Body* last) {
            unchecked {
                double e = 0.0;
                for (; bi <= last; ++bi) {
                    V256d iPos = bi->Position;
                    double iVel = bi->Velocity.Square().Sum();
                    double iMass = bi->Mass.ToScalar();
                    e = e + 0.5 * iMass * iVel;

                    for (Body* bj = bi + 1; bj <= last; ++bj) {
                        e = e
                            - iMass
                            * bj->Mass.ToScalar()
                            / Math.Sqrt(
                                Avx.Subtract(iPos,
                                             bj->Position)
                                .Square().Sum());
                    }//END of j loop
                }//END of i loop
                return e;
            }//END unchecked
        }//END Method EnergyAVX


        [MethodImpl(AggressiveOptimization)]
        private static void
        RunSimulation(int iterations, double step, Body* system, Body* last) {
            unchecked {
                V256d stepV = Vector256.Create(step, step, step, 0d);
                while (iterations-- > 0) {
                    AdvanceAVX(stepV, system, last);
                }
            }//END unchecked
        }//END Method RunSimulation

        [MethodImpl(AllOptimizations)]
        private static void
        AdvanceAVX(V256d stepV, Body* bi, Body* last) {
            for (; bi < last; ++bi) {
                // It's slightly more performant to
                // copy values locally instead of
                // constantly going through the pointer
                V256d iPos = bi->Position;
                V256d iVel = bi->Velocity;
                V256d iMass = bi->Mass;

                for (Body* bj = bi + 1; bj <= last; ++bj) {
                    V256d dx = Avx.Subtract(bj->Position, iPos);
                    double dp = dx.Square().Sum();
                    V256d mag =
                        Avx.Multiply(dx,
                            Vector256.Create(
                                stepV.ToScalar() / (dp * Math.Sqrt(dp))));

                    bj->Velocity =
                        Avx.Subtract(bj->Velocity,
                                        Avx.Multiply(iMass, mag));
                    iVel =
                        Avx.Add(iVel,
                                Avx.Multiply(bj->Mass, mag));
                }//END of j loop
                bi->Position =
                    Avx.Add(iPos,
                            Avx.Multiply(iVel, stepV));
                bi->Velocity = iVel;
            }//END of i loop

            bi->Position =
                Avx.Add(bi->Position,
                        Avx.Multiply(bi->Velocity, stepV));
        }//END Method AdvanceAVX


        [MethodImpl(AllOptimizations)]
        private static V256d Square(this V256d x) => Avx.Multiply(x, x);

        [MethodImpl(AllOptimizations)]
        private static V128d SumLanes(this V256d x) => Sse2.Add(x.GetLower(), x.GetUpper());

        [MethodImpl(AllOptimizations)]
        private static double Sum(this V128d x) => x.ToScalar() + x.GetElement(1);

        [MethodImpl(AllOptimizations)]
        private static double Sum(this V256d x) => x.SumLanes().Sum();
    }//END Class
}//END Namespace
