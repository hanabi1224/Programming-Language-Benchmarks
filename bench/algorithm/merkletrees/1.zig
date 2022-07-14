const std = @import("std");
const builtin = @import("builtin");
const math = std.math;
const Allocator = std.mem.Allocator;
const MIN_DEPTH = 4;

const global_allocator = std.heap.c_allocator;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const n = try get_n();
    const max_depth = math.max(MIN_DEPTH + 2, n);
    {
        const stretch_depth = max_depth + 1;
        const stretch_tree = Node.make(stretch_depth, global_allocator).?;
        defer stretch_tree.deinit();
        stretch_tree.cal_hash();
        try stdout.print("stretch tree of depth {d}\t root hash: {d} check: {d}\n", .{ stretch_depth, stretch_tree.get_hash(), stretch_tree.check() });
    }
    const long_lived_tree = Node.make(max_depth, global_allocator).?;
    defer long_lived_tree.deinit();

    var depth: usize = MIN_DEPTH;
    while (depth <= max_depth) : (depth += 2) {
        const iterations = @intCast(usize, 1) << @intCast(u6, max_depth - depth + MIN_DEPTH);
        var sum: i64 = 0;
        var i: usize = 0;
        while (i < iterations) : (i += 1) {
            const tree = Node.make(depth, global_allocator).?;
            defer tree.deinit();
            tree.cal_hash();
            sum += tree.get_hash();
        }
        try stdout.print("{d}\t trees of depth {d}\t root hash sum: {d}\n", .{ iterations, depth, sum });
    }
    long_lived_tree.cal_hash();
    try stdout.print("long lived tree of depth {d}\t root hash: {d} check: {d}\n", .{ max_depth, long_lived_tree.get_hash(), long_lived_tree.check() });
}

fn get_n() !usize {
    var arg_it = std.process.args();
    _ = arg_it.skip();
    const arg = arg_it.next() orelse return 10;
    return try std.fmt.parseInt(u32, arg, 10);
}

const Node = struct {
    const Self = @This();

    allocator: Allocator,

    hash: ?i64 = null,
    value: ?i64 = null,
    left: ?*Self = null,
    right: ?*Self = null,

    pub fn init(allocator: Allocator) !*Self {
        var node = try allocator.create(Self);
        node.* = .{ .allocator = allocator };
        return node;
    }

    pub fn deinit(self: *Self) void {
        if (self.left != null) {
            self.left.?.deinit();
        }
        if (self.right != null) {
            self.right.?.deinit();
        }
        self.allocator.destroy(self);
    }

    pub fn make(depth: usize, allocator: Allocator) ?*Self {
        var node = Self.init(allocator) catch return null;
        if (depth > 0) {
            const d = depth - 1;
            node.left = Self.make(d, allocator);
            node.right = Self.make(d, allocator);
        } else {
            node.value = 1;
        }
        return node;
    }

    pub fn check(self: *Self) bool {
        if (self.hash == null) {
            return false;
        }
        if (self.value != null) {
            return true;
        }
        return self.left.?.check() and self.right.?.check();
    }

    pub fn cal_hash(self: *Self) void {
        if (self.hash != null) {
            return;
        }
        if (self.value != null) {
            self.hash = self.value;
        } else {
            self.left.?.cal_hash();
            self.right.?.cal_hash();
            self.hash = self.left.?.get_hash() + self.right.?.get_hash();
        }
    }

    pub fn get_hash(self: *Self) i64 {
        if (self.hash == null) {
            return 0;
        }
        return self.hash.?;
    }
};
