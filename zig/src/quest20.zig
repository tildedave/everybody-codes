const std = @import("std");
const expectEqual = std.testing.expectEqual;
const util = @import("util.zig");

const Coord_Part1 = struct {
    location: usize,
    altitude: u32 = 1000,
    direction: util.Direction = .any,
    time_left: u32 = 100,
};

const SearchContext_Part1 = struct {
    grid: util.Grid,
    best_altitude_so_far: *u64,
};

fn gliderNeighborsPart1(node: Coord_Part1, allocator: std.mem.Allocator, l: *std.ArrayList(Coord_Part1), context: SearchContext_Part1) !void {
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

        try l.append(allocator, Coord_Part1{
            .location = it.next_idx,
            .altitude = (if (ni == '-') node.altitude - 2 else if (ni == '+') node.altitude + 1 else if (ni == '.') node.altitude - 1 else unreachable),
            .direction = it.last_direction,
            .time_left = node.time_left - 1,
        });
    }
}

fn distance(_: Coord_Part1, _: Coord_Part1, _: SearchContext_Part1) u32 {
    return 1;
}

fn heuristic(c: Coord_Part1, _: SearchContext_Part1) u32 {
    // this should be adjusted so minimum heuristic is better
    return c.time_left * 500 + if (c.altitude > 1200) 0 else (1200 - c.altitude);
}

fn isGoal(c: Coord_Part1, ctx: SearchContext_Part1) bool {
    if (c.time_left != 0) {
        return false;
    }

    // std.debug.print("{any}\n", .{c});

    if (ctx.best_altitude_so_far.* < c.altitude) {
        ctx.best_altitude_so_far.* = c.altitude;
    }

    return false;
}

fn shouldPrune(c: Coord_Part1, ctx: SearchContext_Part1) bool {
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

    var distances = std.AutoHashMap(Coord_Part1, u32).init(allocator);
    defer distances.deinit();

    var best_altitude_so_far: u64 = 0;
    const context = SearchContext_Part1{
        .grid = grid,
        .best_altitude_so_far = &best_altitude_so_far,
    };

    // zig fmt: off
    var searcher = util.Searcher(Coord_Part1).init();
    _ = searcher.astar(
        allocator, Coord_Part1{.location = start}, &distances,
        context, gliderNeighborsPart1, distance,
        heuristic, isGoal, shouldPrune) catch null;
    // zig fmt: on

    // std.debug.print("best altitude {d}\n", .{best_altitude_so_far});

    return best_altitude_so_far;
}

const Coord_Part2 = struct {
    location: usize,
    altitude: u32 = 10000,
    direction: util.Direction = .any,
    progress: u4 = 0,
    time: u32 = 0,
};

const SearchContext_Part2 = struct {
    grid: *const util.Grid,
    best_time_so_far: *u64,
    best_altitude: *std.AutoHashMap(struct { usize, util.Direction, u4 }, u32),
    a_location: usize,
    b_location: usize,
    c_location: usize,
    start_location: usize,

    after_a_distance: usize,
    after_b_distance: usize,
    after_c_distance: usize,
};

fn gliderNeighborsPart2(node: Coord_Part2, allocator: std.mem.Allocator, l: *std.ArrayList(Coord_Part2), context: SearchContext_Part2) !void {
    if (node.progress == 4) {
        return;
    }

    var it = util.NeighborIterator{ .grid = context.grid.*, .idx = node.location };
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

        if (ni == '#') {
            continue;
        }

        var next_progress: u4 = node.progress;
        if (ni == 'A') {
            if (node.progress == 0) {
                next_progress = 1;
            } else {
                continue;
            }
        }
        if (ni == 'B') {
            if (node.progress == 1) {
                next_progress = 2;
            } else {
                continue;
            }
        }
        if (ni == 'C') {
            if (node.progress == 2) {
                next_progress = 3;
            } else {
                continue;
            }
        }
        if (ni == 'S') {
            if (node.progress == 3 and node.altitude >= 10000) {
                next_progress = 4;
            } else {
                continue;
            }
        }

        try l.append(allocator, Coord_Part2{
            .location = it.next_idx,
            .altitude = (if (ni == '-') node.altitude - 2 else if (ni == '+') node.altitude + 1 else node.altitude - 1),
            .direction = it.last_direction,
            .progress = next_progress,
            .time = node.time + 1,
        });
    }
}

fn distancePart2(_: Coord_Part2, _: Coord_Part2, _: SearchContext_Part2) u32 {
    return 1;
}

fn heuristicPart2(c: Coord_Part2, ctx: SearchContext_Part2) u32 {
    if (c.progress == 4) {
        return 0;
    }

    const target = switch (c.progress) {
        0 => ctx.a_location,
        1 => ctx.b_location,
        2 => ctx.c_location,
        3 => ctx.start_location,
        else => unreachable,
    };
    const progress_scoring: u32 = 500 * @as(u32, 3 - c.progress);
    const distance_to_next: usize = util.manhattanDistance(ctx.grid, c.location, target);
    // const distance_scoring: u32 = (ctx.grid.height + ctx.grid.width) - distance_to_next;
    const distance_scoring: u32 = @intCast(distance_to_next);
    const altitude_scoring: u32 = if (c.altitude < 10_000) 10_000 - c.altitude else 0;

    const f_score = progress_scoring + distance_scoring + altitude_scoring;
    // std.debug.print("{any}: {d}; progress={d} distance={d} altitude={d}\n", .{ c, f_score, progress_scoring, distance_scoring, altitude_scoring });

    return f_score;
}

fn isGoalPart2(c: Coord_Part2, ctx: SearchContext_Part2) bool {
    if (c.progress != 4) {
        return false;
    }

    std.debug.print("end state -> {any}\n", .{c});

    if (ctx.best_altitude.get(.{ c.location, c.direction, c.progress })) |best_altitude| {
        if (c.altitude > best_altitude) {
            ctx.best_altitude.putAssumeCapacity(.{ c.location, c.direction, c.progress }, c.altitude);
        }
    } else {
        _ = ctx.best_altitude.put(.{ c.location, c.direction, c.progress }, c.altitude) catch null;
    }

    if (c.time < ctx.best_time_so_far.* and c.altitude >= 10000) {
        std.debug.print("best so far {d}\n", .{c.time});
        ctx.best_time_so_far.* = c.time;
    }

    return false;
}

fn shouldPrunePart2(c: Coord_Part2, ctx: SearchContext_Part2) bool {
    // do you have enough time to get to the all the remaining things
    // and the end location with the altitude
    if (c.progress == 4) {
        return false;
    }

    const remaining_distance = switch (c.progress) {
        0 => util.manhattanDistance(ctx.grid, c.location, ctx.a_location) + ctx.after_a_distance,
        1 => util.manhattanDistance(ctx.grid, c.location, ctx.b_location) + ctx.after_b_distance,
        2 => util.manhattanDistance(ctx.grid, c.location, ctx.c_location) + ctx.after_c_distance,
        3 => util.manhattanDistance(ctx.grid, c.location, ctx.start_location),
        else => unreachable,
    };
    if (c.time + remaining_distance > ctx.best_time_so_far.*) {
        return true;
    }
    if (c.altitude < 10000) {
        const altitude_to_make_up = 10000 - c.altitude;
        if (c.time + altitude_to_make_up > ctx.best_time_so_far.*) {
            return true;
        }
    }
    if (ctx.best_altitude.get(.{ c.location, c.direction, c.progress })) |best_altitude| {
        if (c.altitude < best_altitude) {
            return true;
        }
    }

    return false;
}

pub fn answer2(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    const start = std.mem.indexOfScalar(u8, lines, 'S').?;
    const a_location = std.mem.indexOfScalar(u8, lines, 'A').?;
    const b_location = std.mem.indexOfScalar(u8, lines, 'B').?;
    const c_location = std.mem.indexOfScalar(u8, lines, 'C').?;

    var distances = std.AutoHashMap(Coord_Part2, u32).init(allocator);
    defer distances.deinit();

    const after_c_distance = util.manhattanDistance(&grid, c_location, start);
    const after_b_distance = after_c_distance + util.manhattanDistance(&grid, b_location, c_location);
    const after_a_distance = after_b_distance + util.manhattanDistance(&grid, a_location, b_location);
    var best_altitude = std.AutoHashMap(struct { usize, util.Direction, u4 }, u32).init(allocator);
    defer best_altitude.deinit();

    var best_time_so_far: u64 = std.math.maxInt(u64);
    const context = SearchContext_Part2{
        .grid = &grid,
        .best_time_so_far = &best_time_so_far,
        .best_altitude = &best_altitude,
        .a_location = a_location,
        .b_location = b_location,
        .c_location = c_location,
        .start_location = start,

        .after_a_distance = after_a_distance,
        .after_b_distance = after_b_distance,
        .after_c_distance = after_c_distance,
    };

    // zig fmt: off
    var searcher = util.Searcher(Coord_Part2).init();
    _ = searcher.astar(
        allocator, Coord_Part2{.location = start}, &distances,
        context, gliderNeighborsPart2, distancePart2,
        heuristicPart2, isGoalPart2, shouldPrunePart2) catch null;
    // zig fmt: on

    return best_time_so_far;
}

const Coord_Part3 = struct {
    location: usize,
    altitude: u32 = 1000,
    direction: util.Direction = .any,
    wraps: usize = 0,
};

const SearchContext_Part3 = struct {
    grid: *const util.Grid,
    best_altitude: *std.AutoHashMap(usize, u32),
    best_distance: *u64,
};

fn gliderNeighborsPart3(node: Coord_Part3, allocator: std.mem.Allocator, l: *std.ArrayList(Coord_Part3), context: SearchContext_Part3) !void {
    if (node.altitude == 0) {
        return;
    }

    var it = util.NeighborIterator{ .grid = context.grid.*, .idx = node.location, .walk_opts = .{ .wraparound_vertical = true } };
    while (it.next()) |ni| {
        switch (node.direction) {
            .up => {
                // never a reason to go up in part 3
                continue;
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

        if (it.last_direction == .up) {
            continue;
        }

        if (ni == '#') {
            continue;
        }

        const wraps = if (it.last_direction == .down and it.next_idx < node.location) node.wraps + 1 else node.wraps;

        try l.append(allocator, Coord_Part3{
            .location = it.next_idx,
            .altitude = if (ni == '-') (if (node.altitude == 1) 0 else node.altitude - 2) else if (ni == '+') node.altitude + 1 else node.altitude - 1,
            .direction = it.last_direction,
            .wraps = wraps,
        });
    }
}

fn distancePart3(_: Coord_Part3, _: Coord_Part3, _: SearchContext_Part3) u32 {
    return 1;
}

fn heuristicPart3(_: Coord_Part3, _: SearchContext_Part3) u32 {
    // won't matter
    // const next_wraps = c.wraps + 1;
    // if (!ctx.best_altitude.contains(next_wraps)) {
    //     return 0;
    // } else {

    // }
    return 0;
}

fn isGoalPart3(c: Coord_Part3, ctx: SearchContext_Part3) bool {
    _, const y = util.coords(ctx.grid, c.location);

    if (c.altitude == 0) {
        const south_distance = c.wraps * ctx.grid.height + y;
        // std.debug.print("Altitude 0 {any} south distance {d}\n", .{ c, south_distance });
        if (south_distance > ctx.best_distance.*) {
            ctx.best_distance.* = south_distance;
        }
    }

    if (y != 0) {
        return false;
    }

    if (ctx.best_altitude.get(c.wraps)) |best_altitude| {
        if (c.altitude > best_altitude) {
            // std.debug.print("{d} - new winner: {d}\n", .{ c.wraps, c.altitude });
            ctx.best_altitude.putAssumeCapacity(c.wraps, c.altitude);
        }
    } else {
        // std.debug.print("{d} - new winner: {d}\n", .{ c.wraps, c.altitude });
        _ = ctx.best_altitude.put(c.wraps, c.altitude) catch null;
    }

    return false;
}

fn shouldPrunePart3(c: Coord_Part3, ctx: SearchContext_Part3) bool {
    _, const y = util.coords(ctx.grid, c.location);
    const fudge = 3;

    if (y == 0) {
        if (ctx.best_altitude.get(c.wraps)) |best_altitude| {
            if (c.altitude + fudge < best_altitude) {
                return true;
            }
        }
    }
    const next_wraps = c.wraps + 1;
    if (ctx.best_altitude.get(next_wraps)) |best_next_altitude| {
        const distance_to_wrap = ctx.grid.height - y;
        const turning_cost: u32 = if (c.direction == .down or c.direction == .any) 0 else 1;
        const altitude_burn_guess = distance_to_wrap + turning_cost;
        const best_altitude_guess = if (c.altitude + fudge < altitude_burn_guess) 0 else c.altitude + fudge - altitude_burn_guess;

        if (best_altitude_guess < best_next_altitude) {
            return true;
        }
    }

    return false;
}

pub fn answer3(allocator: std.mem.Allocator, lines: []const u8, initial_altitude: u32) !u64 {
    // feels like this will be easy, my input has a straight '+' line
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    const start = std.mem.indexOfScalar(u8, lines, 'S').?;

    var distances = std.AutoHashMap(Coord_Part3, u32).init(allocator);
    defer distances.deinit();

    var best_altitude = std.AutoHashMap(usize, u32).init(allocator);
    defer best_altitude.deinit();

    var best_distance: u64 = std.math.minInt(u64);
    const context = SearchContext_Part3{
        .grid = &grid,
        .best_distance = &best_distance,
        .best_altitude = &best_altitude,
    };

    // zig fmt: off
    var searcher = util.Searcher(Coord_Part3).init();
    _ = searcher.astar(
        allocator, Coord_Part3{.location = start, .altitude = initial_altitude}, &distances,
        context, gliderNeighborsPart3, distancePart3,
        heuristicPart3, isGoalPart3, shouldPrunePart3) catch null;
    // zig fmt: on

    return best_distance;
}

test "given example (part 1)" {
    const lines = "#....S....#\n#.........#\n#---------#\n#.........#\n#..+.+.+..#\n#.+-.+.++.#\n#.........#\n";
    try expectEqual(1045, try answer1(std.testing.allocator, lines));
}

test "given example (part 2)" {
    const lines = "####S####\n#-.+++.-#\n#.+.+.+.#\n#-.+.+.-#\n#A+.-.+C#\n#.+-.-+.#\n#.+.B.+.#\n#########\n";
    try expectEqual(24, try answer2(std.testing.allocator, lines));
}

test "second example (part 2)" {
    const lines = "###############S###############\n#+#..-.+.-++.-.+.--+.#+.#++..+#\n#-+-.+-..--..-+++.+-+.#+.-+.+.#\n#---.--+.--..++++++..+.-.#.-..#\n#+-+.#+-.#-..+#.--.--.....-..##\n#..+..-+-.-+.++..-+..+#-.--..-#\n#.--.A.-#-+-.-++++....+..C-...#\n#++...-..+-.+-..+#--..-.-+..-.#\n#..-#-#---..+....#+#-.-.-.-+.-#\n#.-+.#+++.-...+.+-.-..+-++..-.#\n##-+.+--.#.++--...-+.+-#-+---.#\n#.-.#+...#----...+-.++-+-.+#..#\n#.---#--++#.++.+-+.#.--..-.+#+#\n#+.+.+.+.#.---#+..+-..#-...---#\n#-#.-+##+-#.--#-.-......-#..-##\n#...+.-+..##+..+B.+.#-+-++..--#\n###############################\n";
    try expectEqual(78, try answer2(std.testing.allocator, lines));
}

test "second example (part 3)" {
    const lines = "#......S......#\n#-...+...-...+#\n#.............#\n#..+...-...+..#\n#.............#\n#-...-...+...-#\n#.............#\n#..#...+...+..#\n";
    try expectEqual(1, try answer3(std.testing.allocator, lines, 1));
    try expectEqual(2, try answer3(std.testing.allocator, lines, 2));
    try expectEqual(3, try answer3(std.testing.allocator, lines, 3));
    try expectEqual(4, try answer3(std.testing.allocator, lines, 4));
    try expectEqual(5, try answer3(std.testing.allocator, lines, 5));
    try expectEqual(6, try answer3(std.testing.allocator, lines, 6));
    try expectEqual(7, try answer3(std.testing.allocator, lines, 7));
    try expectEqual(9, try answer3(std.testing.allocator, lines, 8));
    try expectEqual(10, try answer3(std.testing.allocator, lines, 9));
    try expectEqual(11, try answer3(std.testing.allocator, lines, 10));
    try expectEqual(190, try answer3(std.testing.allocator, lines, 100));
    try expectEqual(1990, try answer3(std.testing.allocator, lines, 1000));
    try expectEqual(19990, try answer3(std.testing.allocator, lines, 10000));
}
