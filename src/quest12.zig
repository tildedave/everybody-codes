const std = @import("std");
const util = @import("util.zig");
const expectEqual = std.testing.expectEqual;

const RocketMode = enum { ascending, cresting, descending };

fn fireGrid(
    grid: util.Grid,
    x: usize,
    y: usize,
    power: usize,
) ?struct { usize, usize } {
    var rx = x;
    var ry: i64 = @intCast(y); // y can go negative
    var mode: RocketMode = .ascending;
    var mode_left = power;

    while (true) {
        switch (mode) {
            .ascending => {
                rx += 1;
                ry -= 1;
            },
            .cresting => {
                rx += 1;
            },
            .descending => {
                rx += 1;
                ry += 1;
            },
        }

        if (ry >= grid.height - 1 or rx >= grid.width) {
            return null;
        }

        if (ry >= 0) {
            const ridx = util.index(grid, rx, @intCast(ry));
            if (grid.lines[ridx] != '.') {
                // check above too
                const above_ridx = util.index(grid, rx, @intCast(ry - 1));
                if (0 < above_ridx and above_ridx < grid.lines.len) {
                    if (grid.lines[above_ridx] != '.') {
                        return null;
                    }
                }

                // boom!
                return .{ rx, @intCast(ry) };
            }
        }

        if (mode != .descending) {
            mode_left -= 1;
        }
        if (mode_left == 0) {
            switch (mode) {
                .ascending => {
                    mode = .cresting;
                    mode_left = power;
                },
                .cresting => {
                    mode = .descending;
                },
                .descending => {},
            }
        }
    }
}

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    var total: u64 = 0;

    while (std.mem.indexOfScalar(u8, grid.lines, 'T') != null or std.mem.indexOfScalar(u8, grid.lines, 'H') != null) {
        var y: usize = 0;
        while (y < grid.height) : (y += 1) {
            const ch = lines[util.index(grid, 1, y)];
            if (ch == '.' or ch == '=') {
                continue;
            }

            for (1..grid.width) |p| {
                if (fireGrid(grid, 1, y, p)) |hit| {
                    const multiplier: u64 = (if (ch == 'A') 1 else if (ch == 'B') 2 else if (ch == 'C') 3 else unreachable);
                    total += p * multiplier;

                    const hidx = util.index(grid, hit[0], hit[1]);
                    grid.lines[hidx] = switch (grid.lines[hidx]) {
                        'T' => '.',
                        'H' => 'T',
                        else => unreachable,
                    };
                }
            }
        }
    }

    // std.debug.print("total {d}\n", .{total});
    // std.debug.print("{s}", .{grid.lines});
    return total;
}

const Coord = struct { x: usize, y: usize, t: usize };

fn fire(
    map: *std.AutoHashMap(Coord, u64),
    x: usize,
    y: usize,
    power: usize,
) !void {
    // y = 0 -> A, y = 1 -> B, y = 2 -> C
    var rx = x;
    var ry: usize = y;
    var t: usize = 0;
    var mode: RocketMode = .ascending;
    var mode_left = power;

    const ranking_score: u64 = (y + 1) * power;

    while (true) {
        switch (mode) {
            .ascending => {
                rx += 1;
                ry += 1;
            },
            .cresting => {
                rx += 1;
            },
            .descending => {
                rx += 1;
                ry -= 1;
            },
        }
        t += 1;

        const coord = Coord{ .x = rx, .y = ry, .t = t };
        if (map.get(coord)) |score| {
            if (ranking_score < score) {
                std.debug.print("({d}, {d}, {d}) - score now {d}\n", .{ rx, ry, t, score });
                map.putAssumeCapacity(coord, ranking_score);
            }
        } else {
            std.debug.print("({d}, {d}, {d}) - score now {d}\n", .{ rx, ry, t, ranking_score });
            try map.put(coord, ranking_score);
        }

        if (mode != .descending) {
            mode_left -= 1;
        }
        if (mode_left == 0) {
            switch (mode) {
                .ascending => {
                    mode = .cresting;
                    mode_left = power;
                },
                .cresting => {
                    mode = .descending;
                },
                .descending => {},
            }
        }

        if (ry == 0) {
            break;
        }
    }
}

fn collisionScore(map: *std.AutoHashMap(Coord, u64), mx_start: usize, my_start: usize) !u64 {
    var delay: i64 = 0;

    while (true) : (delay -= 1) {
        var t: i64 = delay;
        var mx = mx_start;
        var my = my_start;

        while (true) {
            if (t >= 0) {
                if (map.get(Coord{ .x = mx, .y = my, .t = @intCast(t) })) |score| {
                    return score;
                }
            }

            if ((mx == 0) or (my == 0)) {
                break;
            }
            mx -= 1;
            my -= 1;
            t += 1;
        }
    }

    return 0;
}

pub fn answer3(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    var total: usize = 0;
    var map = std.AutoHashMap(Coord, u64).init(allocator);
    defer map.deinit();

    // something like this will be fine
    for (1..3000) |p| {
        for (0..3) |y| {
            try fire(&map, 0, y, p);
        }
    }

    var it = std.mem.splitScalar(u8, lines, '\n');
    while (it.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        const space = std.mem.indexOfScalar(u8, line, ' ').?;
        const mx: usize = try std.fmt.parseInt(usize, line[0..space], 10);
        const my: usize = try std.fmt.parseInt(usize, line[space + 1 ..], 10);

        std.debug.print("finding collision for ({d}, {d})\n", .{ mx, my });
        total += try collisionScore(&map, mx, my);
    }

    return total;
}

test "part 1 (given example)" {
    const lines = ".............\n.C...........\n.B......T....\n.A......T.T..\n=============\n";
    try expectEqual(13, answer1(std.testing.allocator, lines));
}

test "part 2 (given example)" {
    const lines = ".............\n.C...........\n.B......H....\n.A......T.H..\n=============\n";
    try expectEqual(22, answer1(std.testing.allocator, lines));
}

test "part 3 (toy example)" {
    const allocator = std.testing.allocator;
    var map = std.AutoHashMap(Coord, u64).init(allocator);
    defer map.deinit();

    for (1..4) |p| {
        for (0..3) |y| {
            try fire(&map, 0, y, p);
        }
    }

    try expectEqual(2, collisionScore(&map, 6, 5));
    try expectEqual(6, collisionScore(&map, 6, 7));
    try expectEqual(3, collisionScore(&map, 10, 5));
    try expectEqual(2, collisionScore(&map, 5, 5));
}

test "part 3 (fuller example)" {
    const allocator = std.testing.allocator;
    var map = std.AutoHashMap(Coord, u64).init(allocator);
    defer map.deinit();

    // something like this will be fine
    for (1..1000) |p| {
        for (0..3) |y| {
            try fire(&map, 0, y, p);
        }
    }

    try expectEqual(984, collisionScore(&map, 3839, 2904));
}
