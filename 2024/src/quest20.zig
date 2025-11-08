const std = @import("std");
const expectEqual = std.testing.expectEqual;
const util = @import("util.zig");

const Coord = struct {
    location: usize,
    altitude: u32 = 1000,
    direction: util.Direction = .any,
    time_left: u32 = 100,
};

const SearchContext = struct {
    grid: util.Grid,
    distances: std.AutoHashMap(Coord, u32),
    best_altitude_so_far: *u64,
};

fn gliderNeighbors(node: Coord, allocator: std.mem.Allocator, l: *std.ArrayList(Coord), context: SearchContext) !void {
    if (node.time_left == 0) {
        return;
    }

    var it = util.NeighborIterator{ .grid = context.grid, .idx = node.location };
    while (it.next()) |ni| {
        switch (node.direction) {
            .up => {
                if (it.last_direction == .down) {
                    continue;
                }
            },
            .left => {
                if (it.last_direction == .right) {
                    continue;
                }
            },
            .right => {
                if (it.last_direction == .left) {
                    continue;
                }
            },
            .down => {
                if (it.last_direction == .up) {
                    continue;
                }
            },
            else => {},
        }

        if (ni == '#' or ni == 'S') {
            continue;
        }

        try l.append(allocator, Coord{
            .location = it.next_idx,
            .altitude = (if (ni == '-') node.altitude - 2 else if (ni == '+') node.altitude + 1 else if (ni == '.') node.altitude - 1 else unreachable),
            .direction = it.last_direction,
            .time_left = node.time_left - 1,
        });
    }
}

fn distance(_: Coord, _: Coord, _: SearchContext) u32 {
    return 1;
}

fn heuristic(c: Coord, _: SearchContext) u32 {
    return (100 - c.time_left) * 500 + (c.altitude * 10);
}

fn isGoal(c: Coord, ctx: SearchContext) bool {
    if (c.time_left != 0) {
        return false;
    }

    // std.debug.print("{any}\n", .{c});

    if (ctx.best_altitude_so_far.* < c.altitude) {
        ctx.best_altitude_so_far.* = c.altitude;
    }

    return false;
}

fn shouldPrune(c: Coord, ctx: SearchContext) bool {
    if (ctx.best_altitude_so_far.* > c.altitude + c.time_left) {
        return true;
    }
    return false;
}

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    // we'll use a* search I guess.
    // it will be important to add pruning logic to it.
    // guess we are back to "not enough minerals" :-(
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    const start = std.mem.indexOfScalar(u8, lines, 'S').?;

    var distances = std.AutoHashMap(Coord, u32).init(allocator);
    defer distances.deinit();

    var best_altitude_so_far: u64 = 0;
    const context = SearchContext{
        .grid = grid,
        .distances = distances,
        .best_altitude_so_far = &best_altitude_so_far,
    };

    // zig fmt: off
    var searcher = util.Searcher(Coord).init();
    _ = searcher.astar(
        allocator, Coord{.location = start}, &distances,
        context, gliderNeighbors, distance,
        heuristic, isGoal, shouldPrune) catch null;
    // zig fmt: on

    std.debug.print("best altitude {d}\n", .{best_altitude_so_far});

    return best_altitude_so_far;
}

test "given example (part 1)" {
    const lines = "#....S....#\n#.........#\n#---------#\n#.........#\n#..+.+.+..#\n#.+-.+.++.#\n#.........#\n";
    try expectEqual(1045, try answer1(std.testing.allocator, lines));
}
