//! Centralized print utility for Zig benchmarks
//! 
//! This file was created to consolidate I/O handling across all Zig benchmark implementations
//! And simplitication of update to Zig >= 0.15 StdLib breaking change
//! Usage: const print = @import("../../include/zig/print.zig");
//!        try print.printFmt("Hello {s}!\n", .{"world"});

const std = @import("std");

/// Universal formatted print function for benchmark output
/// Uses a 256-byte stack buffer and direct stdout write for performance
pub fn printFmt(comptime fmt: []const u8, args: anytype) !void {
    var buf: [256]u8 = undefined;
    const out = try std.fmt.bufPrint(&buf, fmt, args);
    _ = try std.posix.write(std.posix.STDOUT_FILENO, out);
}
