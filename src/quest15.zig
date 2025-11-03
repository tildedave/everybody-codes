const std = @import("std");
const expectEqual = std.testing.expectEqual;
const util = @import("util.zig");

const SearchContext = struct { grid: util.Grid };

fn gridNeighbors(n: usize, allocator: std.mem.Allocator, l: *std.ArrayList(usize), context: SearchContext) !void {
    var it = util.NeighborIterator{ .grid = context.grid, .idx = n };
    while (it.next()) |ch| {
        if (ch != '.' and ch != 'H') {
            continue;
        }

        try l.append(allocator, it.next_idx);
    }
}

fn gridDistance(_: usize, _: usize, _: SearchContext) u32 {
    return 1;
}

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8) !u32 {
    const start_coord = std.mem.indexOfScalar(u8, lines, '.').?;
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    var distances = std.AutoHashMap(usize, u32).init(allocator);
    defer distances.deinit();

    const search_context = SearchContext{ .grid = grid };
    var searcher = util.Searcher(usize).init();
    try searcher.dijkstra(allocator, start_coord, &distances, search_context, gridNeighbors, gridDistance);

    var flower_coord: ?usize = std.mem.indexOfScalar(u8, lines, 'H').?;
    var min_dist: u32 = std.math.maxInt(u32);

    while (flower_coord != null) : (flower_coord = std.mem.indexOfScalarPos(u8, lines, flower_coord.? + 1, 'H')) {
        min_dist = @min(min_dist, distances.get(flower_coord.?).?);
    }

    return min_dist * 2;
}

test "given example (part 1)" {
    const lines = "#####.#####\n#.........#\n#.######.##\n#.........#\n###.#.#####\n#H.......H#\n###########\n";
    try expectEqual(26, answer1(std.testing.allocator, lines));
}
