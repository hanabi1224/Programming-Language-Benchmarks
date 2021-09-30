import std;
import std.array : array;
import std.bitmanip : BitArray;

void main(string[] args)
{
    auto n = args.length > 1 ? args[1].to!int() : 10;
    auto u = new double[n];
    u[0 .. u.length] = 1.0;
    auto v = new double[n];
    v[0 .. u.length] = 1.0;
    for (auto i = 0; i < 10; i++)
    {
        a_times_transp(v, u, n);
        a_times_transp(u, v, n);
    }
    auto vbv = 0.0;
    auto vv = 0.0;
    for (auto i = 0; i < n; i++)
    {
        vbv += u[i] * v[i];
        vv += std.math.pow(v[i], 2);
    }
    auto ans = std.math.sqrt(vbv / vv);
    writeln(format("%.9f", ans));
}

void a_times_transp(double[] v, double[] u, int n)
{
    auto x = new double[n];
    times(x, u, n, false);
    times(v, x, n, true);
}

void times(double[] v, double[] u, int n, bool reverse)
{
    for (auto i = 0; i < n; i++)
    {
        auto sum = 0.0;
        for (auto j = 0; j < n; j++)
        {
            if (reverse)
            {
                sum += u[j] / double(evala(j, i));
            }
            else
            {
                sum += u[j] / double(evala(i, j));
            }

        }
        v[i] = sum;
    }
}

int evala(int i, int j)
{
    auto sum = i + j;
    return sum * (sum + 1) / 2 + i + 1;
}
