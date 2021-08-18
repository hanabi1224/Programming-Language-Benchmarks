using System;

namespace BenchTool
{
    public static class NumberExtensions
    {
        public static double Round(this double num, int precision = 1)
        {
            return Math.Round(num, precision);
        }
    }
}
