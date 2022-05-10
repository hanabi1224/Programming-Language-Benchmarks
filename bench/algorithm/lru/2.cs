using System;
using System.Collections.Generic;

public class Program
{
    public static void Main(string[] args)
    {
        var size = args.Length > 0 ? int.Parse(args[0]) : 100;
        var n = args.Length > 0 ? int.Parse(args[1]) : 10000;
        var mod = (uint)size * 10;
        var rng0 = new LCG(0);
        var rng1 = new LCG(1);
        var lru = new LRU<uint, uint>(size);
        var hit = 0;
        var missed = 0;
        for (var i = 0; i < n; i++)
        {
            var n0 = rng0.NextRandom() % mod;
            lru.Put(n0, n0);

            var n1 = rng1.NextRandom() % mod;
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

public class Pair<TK, TV>
{
    public TK Key;

    public TV Value;
}

public class LRU<TK, TV>
{
    public int Size { get; private set; }

    private readonly Dictionary<TK, LinkedListNode<Pair<TK, TV>>> _key_lookup;

    private readonly LinkedList<Pair<TK, TV>> _entries;

    public LRU(int size)
    {
        Size = size;
        _key_lookup = new Dictionary<TK, LinkedListNode<Pair<TK, TV>>>(size);
        _entries = new LinkedList<Pair<TK, TV>>();
    }

    public bool TryGet(TK key, out TV value)
    {
        if (_key_lookup.TryGetValue(key, out var node))
        {
            value = node.Value.Value;
            _entries.Remove(node);
            _entries.AddLast(node);
            return true;
        }
        value = default;
        return false;
    }

    public void Put(TK key, TV value)
    {
        if (_key_lookup.TryGetValue(key, out var node))
        {
            node.Value.Value = value;
            _entries.Remove(node);
            _entries.AddLast(node);
        }
        else if (_entries.Count == Size)
        {
            var first = _entries.First;
            _key_lookup.Remove(first.Value.Key);
            _entries.RemoveFirst();
            first.Value.Key = key;
            first.Value.Value = value;
            _entries.AddLast(first);
            _key_lookup[key] = first;
        }
        else
        {
            _entries.AddLast(new Pair<TK, TV> { Key = key, Value = value });
            _key_lookup[key] = _entries.Last;
        }
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
