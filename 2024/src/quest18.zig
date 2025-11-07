const std = @import("std");
const expectEqual = std.testing.expectEqual;
const util = @import("util.zig");

// flood fill

const SearchContext = struct {
    grid: util.Grid,
};

pub fn gridNeighbors(node: usize, allocator: std.mem.Allocator, l: *std.ArrayList(usize), context: SearchContext) !void {
    var it = util.NeighborIterator{ .grid = context.grid, .idx = node };
    while (it.next()) |ni| {
        if (ni == '#' or ni == ' ') {
            continue;
        }

        try l.append(allocator, it.next_idx);
    }
}

pub fn gridDistance(_: usize, _: usize, _: SearchContext) u32 {
    return 1;
}

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    // const num_trees = std.mem.count(usize, lines, "P");
    var palm_tree_positions = try std.ArrayList(usize).initCapacity(allocator, 0);
    defer palm_tree_positions.deinit(allocator);

    var palm_pos = std.mem.indexOfScalar(u8, grid.lines, 'P');
    while (palm_pos != null) : (palm_pos = std.mem.indexOfScalarPos(u8, grid.lines, palm_pos.? + 1, 'P')) {
        try palm_tree_positions.append(allocator, palm_pos.?);
    }

    // actually this can be solved with dijkstra's algo
    const start = std.mem.indexOfScalar(u8, grid.lines, '.').?;

    var distances = std.AutoHashMap(usize, u32).init(allocator);
    defer distances.deinit();

    var searcher = util.Searcher(usize).init();
    try searcher.dijkstra(allocator, start, &distances, SearchContext{ .grid = grid }, gridNeighbors, gridDistance);

    var max_distance: u32 = std.math.minInt(u32);
    for (palm_tree_positions.items) |i| {
        max_distance = @max(distances.get(i).?, max_distance);
    }

    return max_distance;
}

pub fn answer2(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    // const num_trees = std.mem.count(usize, lines, "P");
    var palm_tree_positions = try std.ArrayList(usize).initCapacity(allocator, 0);
    defer palm_tree_positions.deinit(allocator);

    var palm_pos = std.mem.indexOfScalar(u8, grid.lines, 'P');
    while (palm_pos != null) : (palm_pos = std.mem.indexOfScalarPos(u8, grid.lines, palm_pos.? + 1, 'P')) {
        try palm_tree_positions.append(allocator, palm_pos.?);
    }

    // actually this can be solved with dijkstra's algo
    const start1 = std.mem.indexOfScalar(u8, grid.lines, '.').?;
    const start2 = std.mem.lastIndexOfScalar(u8, grid.lines, '.').?;

    var distances1 = std.AutoHashMap(usize, u32).init(allocator);
    defer distances1.deinit();
    var distances2 = std.AutoHashMap(usize, u32).init(allocator);
    defer distances2.deinit();

    var searcher = util.Searcher(usize).init();
    try searcher.dijkstra(allocator, start1, &distances1, SearchContext{ .grid = grid }, gridNeighbors, gridDistance);
    try searcher.dijkstra(allocator, start2, &distances2, SearchContext{ .grid = grid }, gridNeighbors, gridDistance);

    var max_distance: u32 = std.math.minInt(u32);
    for (palm_tree_positions.items) |i| {
        max_distance = @max(@min(distances1.get(i).?, distances2.get(i).?), max_distance);
    }

    return max_distance;
}

test "given example (part 1)" {
    const lines = "##########\n..#......#\n#.P.####P#\n#.#...P#.#\n##########\n";
    try expectEqual(11, try answer1(std.testing.allocator, lines));
}

test "given example (part 2)" {
    const lines = "#######################\n...P..P...#P....#.....#\n#.#######.#.#.#.#####.#\n#.....#...#P#.#..P....#\n#.#####.#####.#########\n#...P....P.P.P.....P#.#\n#.#######.#####.#.#.#.#\n#...#.....#P...P#.#....\n#######################\n";
    try expectEqual(21, try answer2(std.testing.allocator, lines));
}
