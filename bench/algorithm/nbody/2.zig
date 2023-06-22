const std = @import("std");
const math = std.math;

const solar_mass = 4.0 * math.pi * math.pi;
const year = 365.24;

const vec3 = std.meta.Vector(3, f64);

inline fn dot(a: vec3, b: vec3) f64 {
    @setFloatMode(.Optimized);
    return @reduce(.Add, a * b);
}
inline fn scale(v: vec3, f: f64) vec3 {
    @setFloatMode(.Optimized);
    return v * vec3{f, f, f};
}
inline fn length_sq(v: vec3) f64 {
    @setFloatMode(.Optimized);
    return dot(v, v);
}
inline fn length(v: vec3) f64 {
    @setFloatMode(.Optimized);
    return math.sqrt(length_sq(v));
}

inline fn range(len: usize) []void {
    var res: []void = &.{};
    res.len = len;
    return res;
}

const Body = struct {
    pos: vec3,
    vel: vec3,
    mass: f64,
};

fn offset_momentum(bodies: []Body) void {
    @setFloatMode(.Optimized);
    var pos = vec3{ 0, 0, 0 };
    for (bodies) |b| pos += scale(b.vel, b.mass);
    var sun = &bodies[0];
    sun.vel = -scale(pos, 1.0 / solar_mass);
}

fn advance(bodies: []Body, dt: f64) void {
    @setFloatMode(.Optimized);
    for (bodies[0..]) |*bi, i| {
        var vel = bi.vel;
        var mi = bi.mass;
        for (bodies[i + 1 ..]) |*bj| {
            var d = bi.pos - bj.pos;
            const dsq = length_sq(d);
            const dst = math.sqrt(dsq);
            const mag = dt / (dsq * dst);
            d = scale(d, mag);
            vel -= scale(d, bj.mass);
            bj.vel += scale(d, mi);
        }
        bi.vel = vel;
    }

    for (bodies) |*bi| bi.pos += scale(bi.vel, dt);
}

fn energy(bodies: []const Body) f64 {
    @setFloatMode(.Optimized);
    var e: f64 = 0.0;
    for (bodies) |bi, i| {
        e += 0.5 * length_sq(bi.vel) * bi.mass;
        for (bodies[i + 1 ..]) |bj| {
            e -= bi.mass * bj.mass / length(bi.pos - bj.pos);
        }
    }
    return e;
}

var solar_bodies = [_]Body{
    // Sun
    Body{
        .pos = vec3{ 0, 0, 0 },
        .vel = vec3{ 0, 0, 0 },
        .mass = solar_mass,
    },
    // Jupiter
    Body{
        .pos = vec3{ 4.84143144246472090, -1.16032004402742839, -0.103622044471123109 },
        .vel = scale(vec3{ 1.66007664274403694e-03, 7.69901118419740425e-03, -6.90460016972063023e-05 }, year),
        .mass = 9.54791938424326609e-04 * solar_mass,
    },
    // Saturn
    Body{
        .pos = vec3{ 8.34336671824457987, 4.12479856412430479, -0.403523417114321381 },
        .vel = scale(vec3{ -2.76742510726862411e-03, 4.99852801234917238e-03, 2.30417297573763929e-05 }, year),
        .mass = 2.85885980666130812e-04 * solar_mass,
    },
    // Uranus
    Body{
        .pos = vec3{ 12.8943695621391310, -15.1111514016986312, -0.223307578892655734 },
        .vel = scale(vec3{ 2.96460137564761618e-03, 2.37847173959480950e-03, -2.96589568540237556e-05 }, year),
        .mass = 4.36624404335156298e-05 * solar_mass,
    },
    // Neptune
    Body{
        .pos = vec3{ 15.3796971148509165, -25.9193146099879641, 0.179258772950371181 },
        .vel = scale(vec3{ 2.68067772490389322e-03, 1.62824170038242295e-03, -9.51592254519715870e-05 }, year),
        .mass = 5.15138902046611451e-05 * solar_mass,
    },
};

pub fn main() !void {
    const steps = try get_steps();

    offset_momentum(&solar_bodies);
    const initial_energy = energy(&solar_bodies);
    for (range(steps)) |_| advance(&solar_bodies, 0.01);
    const final_energy = energy(&solar_bodies);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d:.9}\n{d:.9}\n", .{ initial_energy, final_energy });
}

fn get_steps() !usize {
    var arg_it = std.process.args();
    _ = arg_it.skip();
    const arg = arg_it.next() orelse return 1000;
    return try std.fmt.parseInt(usize, arg, 10);
}
