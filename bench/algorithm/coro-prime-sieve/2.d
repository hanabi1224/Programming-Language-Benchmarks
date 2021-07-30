// https://forum.dlang.org/post/mailman.2452.1627656081.3446.digitalmars-d-learn@puremagic.com
import std;

void main(string[] args) {
    auto n = args.length > 1 ? args[1].to!int() : 10;

    auto r = new Generator!int(
    {
        for(auto i = 2;;i++)
            yield(i);
    });

    for(auto i=0;i<n;i++)
    {
        auto prime = r.front;
        writeln(prime);
        r = filter(r, prime);
    }

}

Generator!int filter(Generator!int input, int prime)
{
    return new Generator!int(
    {
        while (input.empty is false)
        {
            input.popFront();
            auto i = input.front;
            if (i % prime != 0)
            {
                yield(i);
            }
        }
    });
}
