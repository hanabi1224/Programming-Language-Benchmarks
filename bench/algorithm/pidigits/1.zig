const std = @import("std");
const bigint = std.math.big.int;
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var global_allocator = gpa.allocator();

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const n = try get_n();

    const one = (try bigint.Managed.initSet(global_allocator, 1));
    const two = (try bigint.Managed.initSet(global_allocator, 2));
    const ten = (try bigint.Managed.initSet(global_allocator, 10));

    var k = try bigint.Managed.initSet(global_allocator, 1);
    var n1 = try bigint.Managed.initSet(global_allocator, 4);
    var n2 = try bigint.Managed.initSet(global_allocator, 3);
    var d = try bigint.Managed.initSet(global_allocator, 1);
    var u = try bigint.Managed.init(global_allocator);
    var v = try bigint.Managed.init(global_allocator);
    var w = try bigint.Managed.init(global_allocator);

    var digits_printed: usize = 0;
    var sb = [10:0]u8{ ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' };
    while (true) {
        // u = &n1 / &d;
        {
            var tmp = try bigint.Managed.init(global_allocator);
            defer tmp.deinit();
            try bigint.Managed.divFloor(&u, &tmp, &n1, &d);
        }
        // v = &n2 / &d;
        {
            var tmp = try bigint.Managed.init(global_allocator);
            defer tmp.deinit();
            try bigint.Managed.divFloor(&v, &tmp, &n2, &d);
        }
        // if u == v
        if (bigint.Managed.eq(u, v)) {
            var digitStr = try u.toString(global_allocator, 10, std.fmt.Case.lower);
            const rem = @rem(digits_printed, 10);
            sb[rem] = digitStr[0];
            digits_printed += 1;
            if (rem == 9) {
                try stdout.print("{s}\t:{d}\n", .{ sb, digits_printed });
            }

            if (digits_printed >= n) {
                if (rem != 9) {
                    var i = rem + 1;
                    while (i < 10) {
                        sb[i] = ' ';
                        i += 1;
                    }
                    try stdout.print("{s}\t:{d}\n", .{ sb, digits_printed });
                }
                break;
            }

            // let to_minus = &u * &ten * &d;
            var to_minus_managed = try bigint.Managed.init(global_allocator);
            defer to_minus_managed.deinit();
            {
                var tmp = try bigint.Managed.init(global_allocator);
                defer tmp.deinit();
                try bigint.Managed.mul(&tmp, &u, &d);
                try bigint.Managed.mul(&to_minus_managed, &tmp, &ten);
            }

            // n1 = &n1 * &ten - &to_minus;
            {
                var tmp = try bigint.Managed.init(global_allocator);
                defer tmp.deinit();
                try bigint.Managed.mul(&tmp, &n1, &ten);
                try bigint.Managed.sub(&n1, &tmp, &to_minus_managed);
            }
            // n2 = &n2 * &ten - &to_minus;
            {
                var tmp = try bigint.Managed.init(global_allocator);
                defer tmp.deinit();
                try bigint.Managed.mul(&tmp, &n2, &ten);
                try bigint.Managed.sub(&n2, &tmp, &to_minus_managed);
            }
        } else {
            // let k2 = &k * &two;
            var k2 = try bigint.Managed.init(global_allocator);
            defer k2.deinit();
            try bigint.Managed.mul(&k2, &k, &two);
            // u = &n1 * (&k2 - &one);
            {
                var tmp = try bigint.Managed.init(global_allocator);
                defer tmp.deinit();
                try bigint.Managed.sub(&tmp, &k2, &one);
                var tmpu = try bigint.Managed.init(global_allocator);
                try bigint.Managed.mul(&tmpu, &tmp, &n1);
                u.deinit();
                u = tmpu;
            }
            // v = &n2 * &two;
            {
                var tmpv = try bigint.Managed.init(global_allocator);
                try bigint.Managed.mul(&tmpv, &n2, &two);
                v.deinit();
                v = tmpv;
            }
            // w = &n1 * (&k - &one);
            {
                var tmp = try bigint.Managed.init(global_allocator);
                defer tmp.deinit();
                try bigint.Managed.sub(&tmp, &k, &one);
                var tmpw = try bigint.Managed.init(global_allocator);
                try bigint.Managed.mul(&tmpw, &tmp, &n1);
                w.deinit();
                w = tmpw;
            }
            // n1 = &u + &v;
            try bigint.Managed.add(&n1, &u, &v);
            // u = &n2 * (&k + &two);
            {
                var tmp = try bigint.Managed.init(global_allocator);
                defer tmp.deinit();
                try bigint.Managed.add(&tmp, &k, &two);
                var tmpu = try bigint.Managed.init(global_allocator);
                try bigint.Managed.mul(&tmpu, &tmp, &n2);
                u.deinit();
                u = tmpu;
            }
            // n2 = &w + &u;
            try bigint.Managed.add(&n2, &w, &u);
            // d = &d * (&k2 + &one);
            {
                var tmp1 = try bigint.Managed.init(global_allocator);
                defer tmp1.deinit();
                try bigint.Managed.add(&tmp1, &k2, &one);
                var tmpd = try bigint.Managed.init(global_allocator);
                try bigint.Managed.mul(&tmpd, &tmp1, &d);
                d.deinit();
                d = tmpd;
            }

            // k = &k + &one;
            try bigint.Managed.add(&k, &k, &one);
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
