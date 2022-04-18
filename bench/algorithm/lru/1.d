@safe:
import std;

// std.container.dlist does not provide suitable APIs
class LinkedListNode(T)
{
    T data;
    LinkedListNode!T prev;
    LinkedListNode!T next;

    this(T data)
    {
        this.data = data;
    }
}

class LinkedList(T)
{
    int len = 0;
    LinkedListNode!T head;
    LinkedListNode!T tail;

    this()
    {
    }

    LinkedListNode!(T) add(T data)
    {
        auto node = new LinkedListNode!(T)(data);
        __add_node(node);
        len += 1;
        return node;
    }

    void __add_node(LinkedListNode!(T) node)
    {
        if (head is null)
        {
            head = node;
        }
        else if (tail !is null)
        {
            node.prev = tail;
            tail.next = node;
        }
        tail = node;
        node.next = null;
    }

    void __remove(LinkedListNode!(T) node)
    {
        if (head is node)
        {
            head = node.next;
        }
        if (tail is node)
        {
            tail = node.prev;
        }
        if (node.prev !is null)
        {
            node.prev.next = node.next;
        }
        if (node.next !is null)
        {
            node.next.prev = node.prev;
        }
    }

    void move_to_end(LinkedListNode!(T) node)
    {
        __remove(node);
        __add_node(node);
    }
}

class LRU(TK, TV)
{
    int size;
    LinkedListNode!(Tuple!(TK, TV))[TK] _key_lookup;
    LinkedList!(Tuple!(TK, TV)) _entries;
    this(int size)
    {
        this.size = size;
        _entries = new LinkedList!(Tuple!(TK, TV))();
    }

    Nullable!TV get(TK key)
    {
        auto node = _key_lookup.get(key, null);
        if (node is null)
        {
            return Nullable!TV.init;
        }
        _entries.move_to_end(node);
        return node.data[1].nullable;
    }

    void put(TK key, TV value)
    {
        auto node = _key_lookup.get(key, null);
        if (node !is null)
        {
            node.data[1] = value;
            _entries.move_to_end(node);
            return;
        }
        else if (_entries.len == size)
        {
            auto head = _entries.head;
            _key_lookup.remove(head.data[0]);
            head.data = Tuple!(TK, TV)(key, value);
            _key_lookup[key] = head;
            _entries.move_to_end(head);
            return;
        }
        _key_lookup[key] = _entries.add(Tuple!(TK, TV)(key, value));
    }
}

class LCG
{
    uint seed;
    this(uint seed)
    {
        this.seed = seed;
    }

    uint next()
    {
        seed = (1103515245 * seed + 12345) % 2147483648;
        return seed;
    }
}

void main(string[] args)
{
    auto size = args.length > 1 ? args[1].to!int() : 100;
    auto n = args.length > 2 ? args[2].to!int() : 100;
    auto mod = size * 10;
    auto rng0 = new LCG(0);
    auto rng1 = new LCG(1);
    auto hit = 0;
    auto missed = 0;
    auto lru = new LRU!(uint, uint)(size);
    foreach (int i; 0 .. n)
    {
        auto n0 = rng0.next() % mod;
        lru.put(n0, n0);
        auto n1 = rng1.next() % mod;
        if (lru.get(n1).isNull)
        {
            missed += 1;
        }
        else
        {
            hit += 1;
        }
    }

    writefln("%d\n%d", hit, missed);
}
