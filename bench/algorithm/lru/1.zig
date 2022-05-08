const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const HashMap = std.HashMap;
const AutoContext = std.hash_map.AutoContext;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var global_allocator = gpa.allocator();

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const args = try get_args();
    const size = args[0];
    const n = args[1];
    const mod = size * 10;
    var rng0 = LCG.init(0);
    var rng1 = LCG.init(1);
    var lru = try LRU(u32, u32).init(size, global_allocator);
    defer lru.deinit();
    var missed: usize = 0;
    var hit: usize = 0;
    var i: u32 = 0;
    while (i < n) : (i += 1) {
        const n0 = rng0.next() % mod;
        try lru.put(n0, n0);
        const n1 = rng1.next() % mod;
        if (lru.get(n1) == null) {
            missed += 1;
        } else {
            hit += 1;
        }
    }
    try stdout.print("{d}\n{d}\n", .{ hit, missed });
}

fn get_args() ![2]u32 {
    var arg_it = std.process.args();
    _ = arg_it.skip();
    var arg = arg_it.next() orelse return [_]u32{ 100, 100 };
    const size = try std.fmt.parseInt(u32, arg, 10);
    arg = arg_it.next() orelse return [_]u32{ size, 100 };
    const n = try std.fmt.parseInt(u32, arg, 10);
    return [_]u32{ size, n };
}

const LCG = struct {
    seed: u32,

    pub fn init(seed: u32) LCG {
        return LCG{ .seed = seed };
    }

    pub fn next(self: *LCG) u32 {
        const A: u32 = comptime 1103515245;
        const C: u32 = comptime 12345;
        const M: u32 = comptime 1 << 31;
        self.seed = (A * self.seed + C) % M;
        return self.seed;
    }
};

fn LinkedList(comptime T: type) type {
    return struct {
        pub const Node = struct {
            prev: ?*Node = null,
            next: ?*Node = null,
            data: T,
        };

        allocator: Allocator,
        head: ?*Node = null,
        tail: ?*Node = null,
        len: usize = 0,

        const Self = @This();

        pub fn init(allocator: Allocator) !*Self {
            var list = try allocator.create(Self);
            list.allocator = allocator;
            return list;
        }

        pub fn deinit(self: *Self) void {
            var ptr = self.head;
            while (ptr != null) {
                var tmp = ptr.?;
                ptr = tmp.next;
                self.allocator.destroy(tmp);
            }
            self.allocator.destroy(self);
        }

        pub fn add(self: *Self, data: T) !*Node {
            var node = try self.allocator.create(Node);
            node.data = data;
            self.__add_node(node);
            self.len += 1;
            return node;
        }

        fn __add_node(self: *Self, node: *Node) void {
            if (self.head == null) {
                self.head = node;
                node.prev = null;
            } else if (self.tail != null) {
                node.prev = self.tail;
                self.tail.?.next = node;
            }
            self.tail = node;
            node.next = null;
        }

        fn __remove(self: *Self, node: *Node) void {
            if (self.head == node) {
                self.head = node.next;
            }
            if (self.tail == node) {
                self.tail = node.prev;
            }
            if (node.prev != null) {
                node.prev.?.next = node.next;
            }
            if (node.next != null) {
                node.next.?.prev = node.prev;
            }
        }

        pub fn move_to_end(self: *Self, node: *Node) void {
            self.__remove(node);
            self.__add_node(node);
        }
    };
}

fn Pair(comptime K: type, comptime V: type) type {
    return struct {
        k: K,
        v: V,
    };
}

fn LRU(
    comptime K: type,
    comptime V: type,
) type {
    const PairType = Pair(K, V);
    const ListType = LinkedList(PairType);
    const MapType = HashMap(K, *ListType.Node, AutoContext(K), 1.0);
    return struct {
        allocator: Allocator,
        size: u32,
        keys: MapType,
        entries: *ListType,

        const Self = @This();

        pub fn init(size: u32, allocator: Allocator) !*Self {
            var lru = try allocator.create(Self);
            lru.allocator = allocator;
            lru.size = size;
            lru.keys = MapType.init(allocator);
            try lru.keys.ensureTotalCapacity(size);
            lru.entries = try ListType.init(allocator);
            return lru;
        }

        pub fn deinit(self: *Self) void {
            self.keys.deinit();
            self.entries.deinit();
            self.allocator.destroy(self);
        }

        pub fn get(self: *Self, key: K) ?V {
            var node = self.keys.get(key);
            if (node == null) {
                return null;
            }
            self.entries.move_to_end(node.?);
            return node.?.data.v;
        }

        pub fn put(self: *Self, key: K, value: V) !void {
            var node = self.keys.get(key);
            if (node != null) {
                node.?.data.v = value;
                self.entries.move_to_end(node.?);
            } else if (self.entries.len == self.size) {
                var head = self.entries.head.?;
                _ = self.keys.remove(head.data.k);
                head.data.k = key;
                head.data.v = value;
                self.entries.move_to_end(head);
                try self.keys.put(key, head);
            } else {
                try self.keys.put(key, try self.entries.add(.{ .k = key, .v = value }));
            }
        }
    };
}
