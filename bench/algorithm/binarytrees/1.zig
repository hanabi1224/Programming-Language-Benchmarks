const std = @import("std");
const print = std.io.getStdOut().writer().print;
const builtin = @import("builtin");
const math = std.math;
const Allocator = std.mem.Allocator;
const Thread = std.Thread;

const MIN_DEPTH = 4;

const CheckPass = struct { iterations: usize, depth: usize, sum: usize };

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var alloc = arena.allocator();

    const n = try getN();
    const max_depth = math.max(MIN_DEPTH + 2, n);
    {
        const stretch_depth = max_depth + 1;
        const stretch_tree = Node.make(stretch_depth, alloc).?;
        defer stretch_tree.deinit();
        try print("stretch tree of depth {d}\t check: {d}\n", .{ stretch_depth, stretch_tree.check() });
    }
    const long_lived_tree = Node.make(max_depth, alloc).?;
    defer long_lived_tree.deinit();

    var depth: usize = MIN_DEPTH;

    var threads = std.ArrayList(std.Thread).init(alloc);
    defer threads.deinit();

    var sums = std.ArrayList(CheckPass).init(alloc);
    defer sums.deinit();
    var i: usize = 0;

    while (depth <= max_depth) : (depth += 2) {
        const iterations = @intCast(usize, 1) << @intCast(u6, max_depth - depth + MIN_DEPTH);
        try sums.append(CheckPass{
            .iterations = iterations,
            .depth = depth,
            .sum = 0,
        });

        const thread = try Thread.spawn(.{}, calc, .{&sums.items[i]});
        try threads.append(thread);
        i += 1;
    }

    i = 0;
    for (threads.items) |thread| {
        thread.join();
        const c = sums.items[i];
        try print("{d}\t trees of depth {d}\t check: {d}\n", .{ c.iterations, c.depth, c.sum });
        i += 1;
    }

    try print("long lived tree of depth {d}\t check: {d}\n", .{ max_depth, long_lived_tree.check() });
}

fn calc(check: *CheckPass) !void {
    // each thread needs its own allocator
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var alloc = arena.allocator();

    var i: usize = 0;
    while (i < check.iterations) : (i += 1) {
        const tree = Node.make(check.depth, alloc).?;
        defer tree.deinit();
        check.sum += tree.check();
    }
}

fn getN() !usize {
    var arg_it = std.process.args();
    _ = arg_it.skip();
    const arg = arg_it.next() orelse return 10;
    return try std.fmt.parseInt(u32, arg, 10);
}

const Node = struct {
    const Self = @This();

    allocator: Allocator,

    left: ?*Self = null,
    right: ?*Self = null,

    pub fn init(allocator: Allocator) !*Self {
        var node = try allocator.create(Self);
        // need to init the values of the node, because malloc
        node.left = null;
        node.right = null;
        node.allocator = allocator;
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
        var node = Self.init(allocator) catch unreachable;
        if (depth > 0) {
            const d = depth - 1;
            node.left = Self.make(d, allocator);
            node.right = Self.make(d, allocator);
        }
        return node;
    }

    pub fn check(self: *Self) usize {
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
