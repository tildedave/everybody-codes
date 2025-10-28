const std = @import("std");
const util = @import("util.zig");
const expectEqual = std.testing.expectEqual;

const RocketMode = enum { ascending, cresting, descending };

fn fire(
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
            std.debug.print("({d}, {d}) {c}\n", .{ rx, ry, grid.lines[ridx] });
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

        // // there is no reason to check the grid until we are descending.
        // for (0..power) |_| {
        //     std.debug.print("({d}, {d})\n", .{ rx, ry });
        // }
        // for (0..power) |_| {
        //     rx += 1;
        //     std.debug.print("({d}, {d})\n", .{ rx, ry });
        // }
        // rx += 1;
        // ry += 1;

        // // there is also no reason to check the grid until we are at least at the
        // // point where we launched (y)
        // if (ry < y) {
        //     std.debug.print("({d}, {d})\n", .{ rx, ry });
        //     continue;
        // }

    }
}

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    // height doesn't matter
    // we can be kind of sloppy for part 1, we'll see what happens in part 2

    // _ = fire(grid, 1, 1, 2);

    // const first_tidx = std.mem.indexOfScalar(u8, lines, 'T').?;
    // const last_tidx = std.mem.lastIndexOfScalar(u8, lines, 'T').?;
    // const first_tx = first_tidx % (grid.width + 1);
    // const last_tx = last_tidx % (grid.width + 1);

    var total: u64 = 0;

    while (std.mem.indexOfScalar(u8, grid.lines, 'T') != null or std.mem.indexOfScalar(u8, grid.lines, 'H') != null) {
        var y: usize = 0;
        while (y < grid.height) : (y += 1) {
            const ch = lines[util.index(grid, 1, y)];
            if (ch == '.' or ch == '=') {
                continue;
            }

            for (1..grid.width) |p| {
                std.debug.print("{c}: testing power {d}\n", .{ ch, p });
                if (fire(grid, 1, y, p)) |hit| {
                    const multiplier: u64 = (if (ch == 'A') 1 else if (ch == 'B') 2 else if (ch == 'C') 3 else unreachable);
                    total += p * multiplier;

                    const hidx = util.index(grid, hit[0], hit[1]);
                    grid.lines[hidx] = switch (grid.lines[hidx]) {
                        'T' => '.',
                        'H' => 'T',
                        else => unreachable,
                    };
                    std.debug.print("hit! now\n{s}\n", .{grid.lines});
                }
            }
            // break;
        }
    }

    std.debug.print("total {d}\n", .{total});
    std.debug.print("{s}", .{grid.lines});
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
