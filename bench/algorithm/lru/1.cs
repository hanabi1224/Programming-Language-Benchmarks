using System;
using System.Collections.Specialized;

public class Program
{
    public static void Main(string[] args)
    {
        var n = args.Length > 0 ? int.Parse(args[0]) : 10000;
        var rng0 = new LCG(0);
        var rng1 = new LCG(1);
        var lru = new LRU(10);
        var hit = 0;
        var missed = 0;
        for (var i = 0; i < n; i++)
        {
            var n0 = rng0.NextRandom() % 100;
            lru.Put(n0, n0);

            var n1 = rng1.NextRandom() % 100;
            if (lru.TryGet(n1, out _))
            {
                hit += 1;
            }
            else
            {
                missed += 1;
            }
        }
        Console.WriteLine($"{hit}\n{missed}");
    }
}

public class LRU
{
    public int Size { get; private set; }

    private readonly OrderedDictionary _dict;

    public LRU(int size)
    {
        Size = size;
        _dict = new OrderedDictionary(size);
    }

    public bool TryGet(object key, out object value)
    {
        if (_dict.Contains(key))
        {
            value = _dict[key];
            // Move to end
            _dict.Remove(key);
            _dict.Add(key, value);

            return true;
        }
        value = null;
        return false;
    }

    public void Put(object key, object value)
    {
        var contains = _dict.Contains(key);
        if (contains)
        {
            _dict.Remove(key);
        }
        else if (_dict.Count == Size)
        {
            _dict.RemoveAt(0);
        }
        _dict.Add(key, value);
    }
}

public class LCG
{
    private uint _seed;
    public LCG(uint seed)
    {
        _seed = seed;
    }
    public uint NextRandom()
    {
        lcg();
        return _seed;
    }
    private void lcg()
    {
        const uint a = 1103515245;
        const uint c = 12345;
        const uint mod = 1u << 31;
        _seed = (a * _seed + c) % mod;
    }
}
