const std = @import("std");
const print = @import("print.zig");
const global_allocator = std.heap.c_allocator;

pub fn main() !void {
    const args = try std.process.argsAlloc(global_allocator);
    defer std.process.argsFree(global_allocator, args);
    if (args.len > 1) {
        try print.printFmt("Hello world {s}!\n", .{args[1]});
    } else {
        try print.printFmt("Hello world!\n", .{});
    }
}
