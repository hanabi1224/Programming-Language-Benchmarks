// port from 8-m.rs and 1.zig
import std;
import std.outbuffer : OutBuffer;

alias Map = uint[Nullable!Code];
static double hundreed = 100.0;

static struct Code
{
    ulong data;

    void push(ubyte c, ulong mask)
    {
        data = ((data << 2) | cast(ulong) c) & mask;
    }

    static Nullable!Code fromStr(ubyte[] s)
    {
        auto mask = Code.makeMask(s.length);
        auto res = Code(0);
        foreach (c; s)
            res.push(Code.encodeByte(c), mask);
        return nullable(res);
    }

    string toStr(size_t frame)
    {
        char[] res;
        auto code = this.data;
        ubyte c;
        foreach (_; 0 .. frame)
        {
            switch (cast(ubyte) code & 0b11)
            {
            case Code.encodeByte('A'):
                c = 'A';
                break;
            case Code.encodeByte('C'):
                c = 'C';
                break;
            case Code.encodeByte('G'):
                c = 'G';
                break;
            case Code.encodeByte('T'):
                c = 'T';
                break;
            default:
                break;
            }
            res ~= c;
            code >>= 2;
        }
        return cast(string) res.reverse;
    }

    pragma(inline, true)
    static ulong makeMask(size_t frame)
    {
        return (1L << (2 * frame)) - 1L;
    }

    pragma(inline, true)
    static ubyte encodeByte(ubyte c)
    {
        return (c >> 1) & 0b11;
    }
}

static struct CodeRange
{
    size_t i = 0;
    ubyte[] input;
    Nullable!Code code;
    ulong mask;

    bool empty() {
        return this.i >= this.input.length;
    }

    Nullable!Code front() {
        const c = this.input[this.i];
        this.code.get.push(c, this.mask);
        return this.code;
    }

    void popFront() {
        this.i += 1;
    }

    this(ubyte[] input, size_t frame)
    {
        const mask = Code.makeMask(frame);
        Nullable!Code tmpCode = Code(0);
        foreach (c; input[0 .. frame - 1])
            tmpCode.get.push(c, mask);
        this.mask = mask;
        this.code = tmpCode;
        this.input = input[frame - 1 .. $];
    }
}

Map genMap(Tuple!(ubyte[], size_t) t)
{
    Map myMap;
    foreach(code; CodeRange(t[0], t[1]))
    {
        myMap.update(code,
            () => 1,
            (ref uint v) { v += 1; });
    }
    return myMap;
}

struct CountCode
{
    ulong count;
    Nullable!Code code;
}

void printMap(size_t self, Map myMap, ref OutBuffer buf)
{
    CountCode[] v;
    ulong total;
    uint count;
    foreach (pair; myMap.byPair)
    {
        total += pair.value;
        v ~= CountCode(pair.value, pair.key);
    }
    alias asc = (a, b) =>
        a.count < b.count ||
        (a.count == b.count && b.code.get.data < a.code.get.data);

    v.sort!(asc);

    foreach (i; iota(cast(int)(v.length) - 1, -1, -1))
    {
        auto cc = v[i];
        buf.writefln("%s %.3f", cc.code.get.toStr(self), cast(double) cc.count / cast(
                double) total * hundreed);
    }
    buf.write("\n");
}

void printOcc(ubyte[] s, ref Map myMap, ref OutBuffer buf)
{
    auto tmp = Code.fromStr(s);
    buf.writefln("%d\t%s", myMap.get(tmp, 0), cast(string) s);
}

ubyte[] readInput(string[] args)
{
    immutable fileName = args.length > 1 ? args[1] : "25000_in";
    char key = '>';
    ubyte[] res;
    auto app = appender(&res);
    app.reserve(65_536);
    auto file = File(args[1]);
    byte x = 3;
    foreach (line; file.byLine())
    {
        if (line[0] == key)
            x--;
        else
            continue;
        if (x == 0)
            break;
    }
    foreach (line; file.byChunk(61))
    {
        app ~= line[0..$-1].map!(a => Code.encodeByte(a));
    }

    return res;
}

void main(string[] args)
{
    auto buf1 = new OutBuffer();
    auto buf2 = new OutBuffer();

    static ubyte[][5] occs = [
        cast(ubyte[]) "GGTATTTTAATTTATAGT",
        cast(ubyte[]) "GGTATTTTAATT",
        cast(ubyte[]) "GGTATT",
        cast(ubyte[]) "GGTA",
        cast(ubyte[]) "GGT",
    ];
    auto input = readInput(args);

    alias myTaskType = Task!(run, uint[Nullable!(Code)]function(Tuple!(ubyte[], ulong)), Tuple!(ubyte[], ulong))*;
    myTaskType[] calls;
    foreach (i; 0 .. occs.length)
    {
        auto t = task(&genMap, tuple(input, occs[i].length));
        t.executeInNewThread();
        calls ~= t;
    }

    printMap(1, genMap(tuple(input, 1UL)), buf1);
    printMap(2, genMap(tuple(input, 2UL)), buf1);

    foreach (i; iota(4, -1, -1))
    {
        printOcc(occs[i], calls[i].yieldForce(), buf2);
    }
    write(buf1);
    write(buf2);
}
