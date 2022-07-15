const std = @import("std");
const json = std.json;

const global_allocator = std.heap.c_allocator;

pub fn main() !void {
    const args = try std.process.argsAlloc(global_allocator);
    defer std.process.argsFree(global_allocator, args);

    const file = if (args.len > 1) blk: {
        var file_name = try std.mem.concat(global_allocator, u8, &.{ args[1], ".json" });
        defer global_allocator.free(file_name);
        break :blk try std.fs.cwd().openFile(file_name, .{});
    } else try std.fs.cwd().openFile("sample.json", .{});

    var n: usize = 3;
    if (args.len > 2) {
        n = try std.fmt.parseInt(usize, args[2], 10);
    }
    const stdout = std.io.getStdOut().writer();

    const json_str = try file.readToEndAlloc(global_allocator, std.math.maxInt(u32));
    defer global_allocator.free(json_str);
    {
        var tokens = json.TokenStream.init(json_str);
        const data = try json.parse(GeoData, &tokens, .{ .allocator = global_allocator });
        defer json.parseFree(GeoData, data, .{ .allocator = global_allocator });
        var json_str_des = std.ArrayList(u8).init(global_allocator);
        defer json_str_des.deinit();
        try json.stringify(data, .{}, json_str_des.writer());
        try printHash(json_str_des.items, stdout);
    }
    {
        var array = std.ArrayList(GeoData).init(global_allocator);
        defer {
            for (array.items) |data|
                json.parseFree(GeoData, data, .{ .allocator = global_allocator });
            array.deinit();
        }
        var i: usize = 0;
        while (i < n) : (i += 1) {
            var tokens = json.TokenStream.init(json_str);
            const data = try json.parse(GeoData, &tokens, .{ .allocator = global_allocator });
            try array.append(data);
        }
        var json_str_des = std.ArrayList(u8).init(global_allocator);
        defer json_str_des.deinit();
        try json.stringify(array.items, .{}, json_str_des.writer());
        try printHash(json_str_des.items, stdout);
    }
}

fn printHash(bytes: []const u8, stdout: anytype) !void {
    const Md5 = std.crypto.hash.Md5;
    var hash: [Md5.digest_length]u8 = undefined;
    Md5.hash(bytes, &hash, .{});
    try stdout.print("{s}\n", .{std.fmt.fmtSliceHexLower(&hash)});
}

const GeoData = struct {
    type: []const u8,
    features: []const Feature,
};
const Feature = struct {
    type: []const u8,
    properties: Properties,
    geometry: Geometry,
};
const Properties = struct { name: []const u8 };
const Geometry = struct {
    type: []const u8,
    coordinates: []const []const [2]f64,
    // provide a custom jsonStringify
    // - this is only necessary to remove spaces between coordinates array
    //   and end up with the correct md5 (compared with 1.js)
    pub fn jsonStringify(
        value: Geometry,
        _: json.StringifyOptions,
        out_stream: anytype,
    ) @TypeOf(out_stream).Error!void {
        const typestr =
            \\{"type":"
        ;
        _ = try out_stream.write(typestr);
        _ = try out_stream.write(value.type);
        const coordsstr =
            \\","coordinates":[
        ;
        _ = try out_stream.write(coordsstr);
        for (value.coordinates) |row, rowi| {
            if (rowi != 0) _ = try out_stream.write(",");
            _ = try out_stream.write("[");
            for (row) |col, coli| {
                if (coli != 0) _ = try out_stream.write(",");
                try out_stream.print("[{d},{d}]", .{ col[0], col[1] });
            }
            _ = try out_stream.write("]");
        }
        _ = try out_stream.write("]}");
    }
};
