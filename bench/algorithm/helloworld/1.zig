const std = @import("std");
const global_allocator = std.heap.c_allocator;
pub fn main() !void {
    const args = try std.process.argsAlloc(global_allocator);
    defer std.process.argsFree(global_allocator, args);
    const stdout = std.io.getStdOut().writer();
    if (args.len > 1) {
        try stdout.print("Hello world {s}!\n", .{args[1]});
    } else {
        try stdout.print("Hello world!\n", .{});
    }
}
