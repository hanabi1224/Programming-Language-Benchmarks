@safe:
import std;
import std.bigint : BigInt;

void main(string[] args)
{
    auto n = args.length > 1 ? args[1].to!int() : 27;
    BigInt k = 1;
    BigInt n1 = 4;
    BigInt n2 = 3;
    BigInt d = 1;
    BigInt u = 0;
    BigInt v = 0;
    BigInt w = 0;

    auto n_printed = 0;
    while (true)
    {
        u = n1 / d;
        v = n2 / d;
        if (u == v)
        {
            write(u);
            n_printed += 1;
            auto mod_ten = n_printed % 10;
            if (mod_ten == 0)
            {
                writeln(format("\t:%d", n_printed));
            }
            if (n_printed >= n)
            {
                if (mod_ten > 0)
                {
                    foreach (i; 0 .. (10 - mod_ten))
                    {
                        write(" ");
                    }
                    writeln(format("\t:%d", n_printed));
                }
                return;
            }
            auto to_minus = u * 10 * d;
            n1 = n1 * 10 - to_minus;
            n2 = n2 * 10 - to_minus;
        }
        else
        {
            auto k2 = k * 2;
            u = n1 * (k2 - 1);
            v = n2 * 2;
            w = n1 * (k - 1);
            n1 = u + v;
            u = n2 * (k + 2);
            n2 = w + u;
            d = d * (k2 + 1);
            k += 1;
        }
    }
}
