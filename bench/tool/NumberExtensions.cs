using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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
