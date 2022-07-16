// all credit to SpexGuy for this solution:
// https://gist.github.com/SpexGuy/953e5780cd2d2c524cba6a79f13076e6

const std = @import("std");

const Channel = struct {
    value: u32,
    frame: anyframe,
};

fn generate(channel: *Channel) void {
    suspend {
        channel.frame = @frame();
    }
    var i: u32 = 2;
    while (true) : (i +%= 1) {
        channel.value = i;
        suspend {}
    }
}

fn filter(out_channel: *Channel, in_channel: *Channel, prime: u32) void {
    suspend {
        out_channel.frame = @frame();
    }
    while (true) {
        resume in_channel.frame;
        if (in_channel.value % prime != 0) {
            out_channel.value = in_channel.value;
            suspend {}
        }
    }
}

var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const global_allocator = arena.allocator();
const stdout = std.io.getStdOut().writer();

pub fn main() !void {
    defer arena.deinit();
    const args = try std.process.argsAlloc(global_allocator);

    const n = if (args.len > 1)
        try std.fmt.parseInt(usize, args[1], 10)
    else
        100;

    var ch = try global_allocator.create(Channel);
    _ = async generate(ch);

    var i: u32 = 0;
    while (i < n) : (i += 1) {
        resume ch.frame;
        const prime = ch.value;
        try stdout.print("{}\n", .{prime});
        if (i >= n - 1) break;
        const ch1 = try global_allocator.create(Channel);
        const frame = try global_allocator.create(@Frame(filter));
        frame.* = async filter(ch1, ch, prime);
        ch = ch1;
    }
}
