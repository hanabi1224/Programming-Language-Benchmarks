const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const MIN_DEPTH = 4;

const global_allocator = std.heap.c_allocator;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const n = try get_n();
    const max_depth = @max(MIN_DEPTH + 2, n);
    {
        const stretch_depth = max_depth + 1;
        const stretch_tree = Node.make(stretch_depth).?;
        defer stretch_tree.deinit();
        try stdout.print("stretch tree of depth {d}\t check: {d}\n", .{ stretch_depth, stretch_tree.check() });
    }
    const long_lived_tree = Node.make(max_depth).?;
    defer long_lived_tree.deinit();

    var depth: usize = MIN_DEPTH;
    while (depth <= max_depth) : (depth += 2) {
        const iterations = @intCast(usize, 1) << @intCast(u6, max_depth - depth + MIN_DEPTH);
        var sum: usize = 0;
        var i: usize = 0;
        while (i < iterations) : (i += 1) {
            const tree = Node.make(depth).?;
            defer tree.deinit();
            sum += tree.check();
        }
        try stdout.print("{d}\t trees of depth {d}\t check: {d}\n", .{ iterations, depth, sum });
    }

    try stdout.print("long lived tree of depth {d}\t check: {d}\n", .{ max_depth, long_lived_tree.check() });
}

fn get_n() !usize {
    var arg_it = std.process.args();
    _ = arg_it.skip();
    const arg = arg_it.next() orelse return 10;
    return try std.fmt.parseInt(u32, arg, 10);
}

const Node = struct {
    left: ?*Node = null,
    right: ?*Node = null,

    pub fn init() !*Node {
        var node = try global_allocator.create(Node);
        node.* = .{ };
        return node;
    }

    pub fn deinit(self: *Node) void {
        if (self.left != null) {
            self.left.?.deinit();
        }
        if (self.right != null) {
            self.right.?.deinit();
        }
        global_allocator.destroy(self);
    }

    pub fn make(depth: usize) ?*Node {
        var node = Node.init() catch return null;
        if (depth > 0) {
            const d = depth - 1;
            node.left = Node.make(d);
            node.right = Node.make(d);
        }
        return node;
    }

    pub fn check(self: *Node) usize {
        var sum: usize = 1;
        if (self.left != null) {
            sum += self.left.?.check();
        }
        if (self.right != null) {
            sum += self.right.?.check();
        }
        return sum;
    }
};
