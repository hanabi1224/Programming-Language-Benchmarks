@safe:
import std;
import std.math;
import std.bigint : BigInt;

void main(string[] args)
{
    auto n = args.length > 1 ? args[1].to!int() : 27;
    auto k = binary_search(n);
    auto t = sum_terms(0, k - 1);
    auto p = t[0];
    auto q = t[1];
    p += q;
    auto a = "1";
    for (auto i = 1; i < n; i++)
    {
        a ~= "0";
    }
    auto answer = p * BigInt(a) / q;
    auto s = format("%s", answer);
    auto i = 0;
    for (; i + 10 <= n; i += 10)
    {
        auto end = i + 10;
        writeln(format("%s\t:%d", s[i .. end], end));
    }
    auto rem_len = n - 1 - i;
    if (rem_len > 0)
    {
        auto padding = "";
        for (auto j = 1; j < 10 - rem_len; j++)
        {
            padding ~= " ";
        }
        writeln(format("%s%s\t:%d", s[i .. n], padding, n));
    }
}

Tuple!(BigInt, BigInt) sum_terms(int a, int b)
{
    if (b == a + 1)
    {
        return tuple(BigInt(1), BigInt(b));
    }
    auto mid = (a + b) / 2;
    auto t_left = sum_terms(a, mid);
    auto t_right = sum_terms(mid, b);
    return tuple(t_left[0] * t_right[1] + t_right[0], t_left[1] * t_right[1]);
}

int binary_search(int n)
{
    auto a = 0;
    auto b = 1;
    while (!test_k(n, b))
    {
        a = b;
        b *= 2;
    }
    while (b - a > 1)
    {
        auto m = (a + b) / 2;
        if (test_k(n, m))
        {
            b = m;
        }
        else
        {
            a = m;
        }
    }
    return b;
}

bool test_k(int n, int k)
{
    if (k < 0)
    {
        return false;
    }
    auto ln_k_factorial = k * (log(k) - 1) + 0.5 * log(PI * 2);
    auto log_10_k_factorial = ln_k_factorial / LN10;
    return log_10_k_factorial >= n + 50;
}
