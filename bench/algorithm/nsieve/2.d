import std;
import std.array : array;
import std.bitmanip : BitArray;

void nsieve(int n)
{
    BitArray flags = false.repeat(n).array;
    auto count = 0;
    for (auto i = 2; i < n; i++)
    {
        if (!flags[i])
        {
            count += 1;
            for (auto j = i << 1; j < n; j += i)
            {
                flags[j] = true;
            }
        }
    }
    writeln(format("Primes up to %8s %8s", n, count));
}

void main(string[] args)
{
    auto n = args.length > 1 ? args[1].to!int() : 10;
    for (auto i = 0; i < 3; i++)
    {
        nsieve(10000 << (n - i));
    }
}
