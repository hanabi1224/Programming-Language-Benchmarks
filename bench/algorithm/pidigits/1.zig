const std = @import("std");
const bigint = std.math.big.int;
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var global_allocator = &gpa.allocator;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const n = try get_n();

    const one = (try bigint.Managed.initSet(global_allocator, 1)).toConst();
    const two = (try bigint.Managed.initSet(global_allocator, 2)).toConst();
    const ten = (try bigint.Managed.initSet(global_allocator, 10)).toConst();

    var k = try bigint.Managed.initSet(global_allocator, 1);
    var n1 = try bigint.Managed.initSet(global_allocator, 4);
    var n2 = try bigint.Managed.initSet(global_allocator, 3);
    var d = try bigint.Managed.initSet(global_allocator, 1);
    var u = try bigint.Managed.init(global_allocator);
    var v = try bigint.Managed.init(global_allocator);
    var w = try bigint.Managed.init(global_allocator);
    var tmp = try bigint.Managed.init(global_allocator);

    var digits_printed: i32 = 0;
    while (true) {
        // u = &n1 / &d;
        try bigint.Managed.divFloor(&u, &tmp, n1.toConst(), d.toConst());
        // v = &n2 / &d;
        try bigint.Managed.divFloor(&v, &tmp, n2.toConst(), d.toConst());
        // if u == v
        if (bigint.Managed.eq(u, v)) {
            try stdout.print("{}", .{u});
            digits_printed += 1;
            const rem = @rem(digits_printed, 10);
            if (rem == 0) {
                try stdout.print("\t:{}\n", .{digits_printed});
            }

            if (digits_printed >= n) {
                if (rem != 0) {
                    var i = 10 - rem;
                    while (i > 0) {
                        try stdout.print(" ", .{});
                        i -= 1;
                    }
                    try stdout.print("\t:{}\n", .{digits_printed});
                }
                break;
            }

            // let to_minus = &u * &ten * &d;
            var to_minus_managed = try bigint.Managed.init(global_allocator);
            defer to_minus_managed.deinit();
            try bigint.Managed.mul(&to_minus_managed, u.toConst(), d.toConst());
            try bigint.Managed.mul(&to_minus_managed, to_minus_managed.toConst(), ten);
            const to_minus = to_minus_managed.toConst();
            // n1 = &n1 * &ten - &to_minus;
            try bigint.Managed.mul(&tmp, n1.toConst(), ten);
            try bigint.Managed.sub(&n1, tmp.toConst(), to_minus);
            // n2 = &n2 * &ten - &to_minus;
            try bigint.Managed.mul(&tmp, n2.toConst(), ten);
            try bigint.Managed.sub(&n2, tmp.toConst(), to_minus);
        } else {
            // let k2 = &k * &two;
            var k2 = try bigint.Managed.init(global_allocator);
            defer k2.deinit();
            try bigint.Managed.mul(&k2, k.toConst(), two);
            // u = &n1 * (&k2 - &one);
            try bigint.Managed.sub(&tmp, k2.toConst(), one);
            try bigint.Managed.mul(&u, tmp.toConst(), n1.toConst());
            // v = &n2 * &two;
            try bigint.Managed.mul(&v, n2.toConst(), two);
            // w = &n1 * (&k - &one);
            try bigint.Managed.sub(&tmp, k.toConst(), one);
            try bigint.Managed.mul(&w, tmp.toConst(), n1.toConst());
            // n1 = &u + &v;
            try bigint.Managed.add(&n1, u.toConst(), v.toConst());
            // u = &n2 * (&k + &two);
            try bigint.Managed.add(&tmp, k.toConst(), two);
            try bigint.Managed.mul(&u, tmp.toConst(), n2.toConst());
            // n2 = &w + &u;
            try bigint.Managed.add(&n2, w.toConst(), u.toConst());
            // d = &d * (&k2 + &one);
            try bigint.Managed.add(&tmp, k2.toConst(), one);
            try bigint.Managed.mul(&d, tmp.toConst(), d.toConst());
            // k = &k + &one;
            try bigint.Managed.add(&k, k.toConst(), one);
        }
    }
}

fn get_n() !i32 {
    const args = try std.process.argsAlloc(global_allocator);
    defer std.process.argsFree(global_allocator, args);
    if (args.len > 1) {
        return try std.fmt.parseInt(i32, args[1], 10);
    } else {
        return 1;
    }
}
