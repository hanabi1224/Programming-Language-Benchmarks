const std = @import("std");
const builtin = @import("builtin");
const math = std.math;
const Allocator = std.mem.Allocator;
const MIN_DEPTH = 4;

pub fn main() !void {
    const global_allocator = std.heap.c_allocator;
    const stdout = std.io.getStdOut().writer();
    const n = try get_n(global_allocator);
    const max_depth = math.max(MIN_DEPTH + 2, n);
    {
        const stretch_depth = max_depth + 1;
        var stretch_tree = try Tree.init(stretch_depth, global_allocator);
        defer stretch_tree.deinit();
        try stdout.print("stretch tree of depth {d}\t check: {d}\n", .{ stretch_depth, stretch_tree.check() });
    }
    var long_lived_tree = try Tree.init(max_depth, global_allocator);
    defer long_lived_tree.deinit();

    var depth: usize = MIN_DEPTH;
    while (depth <= max_depth) : (depth += 2) {
        const iterations = @intCast(usize, 1) << @intCast(u6, max_depth - depth + MIN_DEPTH);
        var sum: usize = 0;
        var i: usize = 0;
        while (i < iterations) : (i += 1) {
            var tree = try Tree.init(depth, global_allocator);
            defer tree.deinit();
            sum += tree.check();
        }
        try stdout.print("{d}\t trees of depth {d}\t check: {d}\n", .{ iterations, depth, sum });
    }

    try stdout.print("long lived tree of depth {d}\t check: {d}\n", .{ max_depth, long_lived_tree.check() });
}

fn get_n(allocator: Allocator) !usize {
    var arg_it = try std.process.ArgIterator.initWithAllocator(allocator);
    defer arg_it.deinit();
    _ = arg_it.skip();
    const arg = arg_it.next() orelse return 10;
    return try std.fmt.parseInt(u32, arg, 10);
}

const NodePool = std.heap.MemoryPool(Node);

const Tree = struct {
    node_pool: NodePool,
    root_node: *Node,

    pub fn init(depth: usize, allocator: Allocator) !Tree {
        var node_pool = NodePool.init(allocator);
        var root_node = Node.create_tree(&node_pool, depth) catch |err| {
            node_pool.deinit();
            return err;
        };
        return Tree{ .node_pool = node_pool, .root_node = root_node };
    }

    pub fn check(self: *Tree) usize {
        return self.root_node.check();
    }

    pub fn deinit(self: *Tree) void {
        self.node_pool.deinit();
        self.* = undefined;
    }
};

const Node = struct {
    left: ?*Node = null,
    right: ?*Node = null,

    pub fn check(node: *Node) usize {
        var sum: usize = 1;
        if (node.left) |left| sum += left.check();
        if (node.right) |right| sum += right.check();
        return sum;
    }

    pub fn create_tree(pool: *NodePool, depth: usize) !*Node {
        var node: *Node = try pool.create();
        if (depth > 0) {
            const sub_tree_depth = depth - 1;
            node.left = try create_tree(pool, sub_tree_depth);
            node.right = try create_tree(pool, sub_tree_depth);
        } else {
            node.left = null;
            node.right = null;
        }
        return node;
    }
};

test "Test depth 0" {
    var tree1 = try Tree.init(0, std.testing.allocator);
    defer tree1.deinit();
    try std.testing.expect(tree1.check() == 1);
}

test "Test depth 1" {
    var tree1 = try Tree.init(1, std.testing.allocator);
    defer tree1.deinit();
    try std.testing.expect(tree1.check() == 3);
}

test "Test depth 2" {
    var tree1 = try Tree.init(2, std.testing.allocator);
    defer tree1.deinit();
    try std.testing.expect(tree1.check() == 7);
}

test "Test multiple trees" {
    var tree1 = try Tree.init(2, std.testing.allocator);
    defer tree1.deinit();

    var tree2 = try Tree.init(2, std.testing.allocator);
    defer tree2.deinit();

    var tree3 = try Tree.init(2, std.testing.allocator);
    defer tree3.deinit();

    try std.testing.expect(tree1.check() == 7);
    try std.testing.expect(tree2.check() == 7);
    try std.testing.expect(tree3.check() == 7);
}
