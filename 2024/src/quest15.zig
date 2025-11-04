const std = @import("std");
const expectEqual = std.testing.expectEqual;
const util = @import("util.zig");

const SearchContextQuest1 = struct { grid: util.Grid, start: usize };

fn gridDistance(_: StateQuest1, _: StateQuest1, _: SearchContextQuest1) u32 {
    return 1;
}

const StateQuest1 = struct {
    idx: usize,
    has_fruit: bool,
};

fn neighborsQuest1(state: StateQuest1, allocator: std.mem.Allocator, l: *std.ArrayList(StateQuest1), context: SearchContextQuest1) !void {
    var it = util.NeighborIterator{ .grid = context.grid, .idx = state.idx };
    while (it.next()) |ch| {
        if (ch != '.' and ch != 'H') {
            continue;
        }

        if (ch == 'H') {
            try l.append(allocator, StateQuest1{ .idx = it.next_idx, .has_fruit = true });
        }

        try l.append(allocator, StateQuest1{ .idx = it.next_idx, .has_fruit = state.has_fruit });
    }
}

fn heuristicQuest1(state: StateQuest1, _: SearchContextQuest1) u32 {
    var total: u32 = 0;
    if (state.has_fruit) {
        total += 1000;
    }
    return total;
}

fn isGoalQuest1(state: StateQuest1, ctx: SearchContextQuest1) bool {
    return state.idx == ctx.start and state.has_fruit;
}

fn shouldPruneQuest1(_: StateQuest1, _: SearchContextQuest1) bool {
    return false;
}

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8) !u32 {
    const start_coord = std.mem.indexOfScalar(u8, lines, '.').?;
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    var distances = std.AutoHashMap(StateQuest1, u32).init(allocator);
    defer distances.deinit();

    const search_context = SearchContextQuest1{ .grid = grid, .start = start_coord };
    var searcher = util.Searcher(StateQuest1).init();
    try searcher.astar(allocator, StateQuest1{ .idx = start_coord, .has_fruit = false }, &distances, search_context, neighborsQuest1, gridDistance, heuristicQuest1, isGoalQuest1, shouldPruneQuest1);

    return distances.get(StateQuest1{ .idx = start_coord, .has_fruit = true }) orelse 0;
}

const SearchContextQuest2 = struct { grid: util.Grid, start: usize, fruit_mask: u32, best_so_far: u32 = std.math.maxInt(u32) };

const StateQuest2 = struct {
    idx: usize,
    fruits: u32,
};

fn isFruit(ch: u8) bool {
    return ch >= 'A' and ch <= 'Z';
}

fn neighborsQuest2(state: StateQuest2, allocator: std.mem.Allocator, l: *std.ArrayList(StateQuest2), context: SearchContextQuest2) !void {
    var it = util.NeighborIterator{ .grid = context.grid, .idx = state.idx };
    while (it.next()) |ch| {
        if (ch != '.' and !isFruit(ch)) {
            continue;
        }

        if (isFruit(ch)) {
            const fruit_idx: u5 = @intCast(ch - 'A');
            const fruit_mask: u32 = @as(u32, 0x1) << fruit_idx;
            try l.append(allocator, StateQuest2{ .idx = it.next_idx, .fruits = state.fruits | fruit_mask });
        }

        try l.append(allocator, StateQuest2{ .idx = it.next_idx, .fruits = state.fruits });
    }
}

fn isGoalQuest2(state: StateQuest2, ctx: SearchContextQuest2) bool {
    return (state.idx == ctx.start and state.fruits == ctx.fruit_mask);
}

fn heuristicQuest2(state: StateQuest2, _: SearchContextQuest2) u32 {
    var total: u32 = 0;
    var mask = state.fruits;
    while (mask != 0) : (mask = mask >> 1) {
        if (mask & 0x1 == 1) {
            total += 1000;
        }
    }

    return total;
}

fn shouldPruneQuest2(_: StateQuest2, _: SearchContextQuest2) bool {
    return false;
}

test "isGoalQuest2" {
    const ctx = SearchContextQuest2{ .grid = util.Grid{ .lines = "", .width = 0, .height = 0 }, .num_fruits = 3, .start = 0 };
    try expectEqual(true, isGoalQuest2(StateQuest2{ .idx = 0, .fruits = 0x7 }, ctx));
    try expectEqual(false, isGoalQuest2(StateQuest2{ .idx = 0, .fruits = 0x5 }, ctx));
}

fn gridDistanceQuest2(_: StateQuest2, _: StateQuest2, _: SearchContextQuest2) u32 {
    return 1;
}

pub fn answer2(allocator: std.mem.Allocator, lines: []const u8) !u32 {
    const start_coord = std.mem.indexOfScalar(u8, lines, '.').?;
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    var fruit_mask: u32 = 0;
    var fruit_ch: u8 = 'Z';
    while (fruit_ch >= 'A') : (fruit_ch -= 1) {
        if (std.mem.indexOfScalar(u8, lines, fruit_ch) == null) {
            fruit_mask = fruit_mask << 1;
        } else {
            std.debug.print("there is a {c}\n", .{fruit_ch});
            fruit_mask = 0x1 | (fruit_mask << 1);
        }
    }
    std.debug.print("fruit mask {d}\n", .{fruit_mask});

    var distances = std.AutoHashMap(StateQuest2, u32).init(allocator);
    defer distances.deinit();

    const search_context = SearchContextQuest2{ .grid = grid, .start = start_coord, .fruit_mask = fruit_mask };
    var searcher = util.Searcher(StateQuest2).init();
    try searcher.astar(allocator, StateQuest2{ .idx = start_coord, .fruits = 0 }, &distances, search_context, neighborsQuest2, gridDistanceQuest2, heuristicQuest2, isGoalQuest2, shouldPruneQuest2);

    return distances.get(StateQuest2{ .idx = start_coord, .fruits = fruit_mask }) orelse 0;
}

test "given example (part 1)" {
    const lines = "#####.#####\n#.........#\n#.######.##\n#.........#\n###.#.#####\n#H.......H#\n###########\n";
    try expectEqual(26, answer1(std.testing.allocator, lines));
}

test "given example (part 2)" {
    const lines = "##########.##########\n#...................#\n#.###.##.###.##.#.#.#\n#..A#.#..~~~....#A#.#\n#.#...#.~~~~~...#.#.#\n#.#.#.#.~~~~~.#.#.#.#\n#...#.#.B~~~B.#.#...#\n#...#....BBB..#....##\n#C............#....C#\n#####################\n    ";
    try expectEqual(38, answer2(std.testing.allocator, lines));
}
