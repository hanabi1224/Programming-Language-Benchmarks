const std = @import("std");
const json = std.json;

const global_allocator = std.heap.c_allocator;

pub fn main() !void {
    const args = try std.process.argsAlloc(global_allocator);
    defer std.process.argsFree(global_allocator, args);
    var file: ?std.fs.File = null;
    if (args.len > 1) {
        var file_name = try concat(global_allocator, args[1], ".json");
        file = try std.fs.cwd().openFile(file_name, .{
            .read = true,
        });
    } else {
        file = try std.fs.cwd().openFile("sample.json", .{
            .read = true,
        });
    }
    var n: usize = 3;
    if (args.len > 2) {
        n = try std.fmt.parseInt(usize, args[2], 10);
    }
    const stdout = std.io.getStdOut().writer();
    if (file) |fileValue| {
        const json_str = try fileValue.readToEndAlloc(global_allocator, 10485760);
        // const b = json.validate(json_str);
        // try stdout.print("{b}", .{b});
        var stream = json.TokenStream.init(json_str);
        const data = try json.parse(GeoData, &stream, .{ .allocator = global_allocator, .ignore_unknown_fields = true });
        try stdout.print("{s}\n", .{data.features[0].properties.name});
        // TODO: prettify
    }
}

fn concat(allocator: *std.mem.Allocator, a: []const u8, b: []const u8) ![]u8 {
    const result = try allocator.alloc(u8, a.len + b.len);
    std.mem.copy(u8, result, a);
    std.mem.copy(u8, result[a.len..], b);
    return result;
}

const GeoData = struct { features: []Feature };

const Feature = struct { properties: Properties };

const Properties = struct { name: []u8 };
