const std = @import("std");
const expectEqual = std.testing.expectEqual;
const util = @import("util.zig");

// this is a MST algorithm
// construct the graph
// then kruskal's algo

const Edge = struct {
    source: usize,
    dest: usize,
    weight: usize,
};

fn manhattanDistance(grid: *const util.Grid, i: usize, j: usize) usize {
    const ix, const iy = util.coords(grid, i);
    const jx, const jy = util.coords(grid, j);

    const x_delta: usize = if (ix > jx) ix - jx else jx - ix;
    const y_delta: usize = if (iy > jy) iy - jy else jy - iy;

    return x_delta + y_delta;
}

fn lessThanEdges(_: @TypeOf(.{}), e1: Edge, e2: Edge) bool {
    return e1.weight < e2.weight;
}

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    return constellations(allocator, lines, false);
}

pub fn answer3(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    return constellations(allocator, lines, true);
}

fn constellations(allocator: std.mem.Allocator, lines: []const u8, brilliant: bool) !u64 {
    var star_indices = try std.ArrayList(usize).initCapacity(allocator, 0);
    defer star_indices.deinit(allocator);

    var pos: ?usize = std.mem.indexOfScalar(u8, lines, '*');
    while (pos) |p| : (pos = std.mem.indexOfScalarPos(u8, lines, p + 1, '*')) {
        try star_indices.append(allocator, p);
    }

    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    var all_edges = try std.ArrayList(Edge).initCapacity(allocator, 0);
    defer all_edges.deinit(allocator);

    for (star_indices.items) |i| {
        for (star_indices.items) |j| {
            if (j <= i) { // lazy way of deduplicating edges
                continue;
            }
            const distance = manhattanDistance(&grid, i, j);
            if (!brilliant or distance < 6) {
                try all_edges.append(allocator, Edge{ .source = i, .dest = j, .weight = distance });
            }
        }
    }

    std.mem.sort(Edge, all_edges.items, .{}, lessThanEdges);

    const Forest = util.DisjointSet(usize);
    var forest = try Forest.init(allocator);
    defer forest.deinit();

    var forest_map = std.AutoHashMap(usize, *Forest.Node).init(allocator);
    defer forest_map.deinit();

    for (star_indices.items) |i| {
        try forest_map.put(i, try forest.makeSet(i));
    }

    var mst_edges = try std.ArrayList(Edge).initCapacity(allocator, 0);
    defer mst_edges.deinit(allocator);

    for (all_edges.items) |e| {
        const source_node = forest_map.get(e.source).?;
        const dest_node = forest_map.get(e.dest).?;

        if (forest.find(source_node) != forest.find(dest_node)) {
            forest.unionSets(source_node, dest_node);
            try mst_edges.append(allocator, e);
        }
    }

    var mst_weight_map = std.AutoHashMap(usize, struct { usize, u64 }).init(allocator);
    defer mst_weight_map.deinit();

    for (mst_edges.items) |e| {
        const source_root = forest.find(forest_map.get(e.source).?);
        const dest_root = forest.find(forest_map.get(e.dest).?);
        std.debug.assert(source_root == dest_root);

        const existing = mst_weight_map.get(source_root.elem) orelse .{ 0, 0 };
        try mst_weight_map.put(source_root.elem, .{ existing.@"0" + 1, existing.@"1" + e.weight });
    }

    var totals = try std.ArrayList(u64).initCapacity(allocator, 0);
    defer totals.deinit(allocator);

    var it_msg_weights = mst_weight_map.iterator();
    while (it_msg_weights.next()) |a| {
        try totals.append(allocator, 1 + a.value_ptr.@"0" + a.value_ptr.@"1");
    }
    std.mem.sort(u64, totals.items, {}, comptime std.sort.desc(u64));

    var total: u64 = 1;
    for (0..3) |i| {
        if (i < totals.items.len) {
            total *= totals.items[i];
        }
    }
    return total;
}

test "given example (part 1)" {
    const lines = "*...*\n..*..\n.....\n.....\n*.*..\n";

    try expectEqual(16, try answer1(std.testing.allocator, lines));
}

test "given example (part 3)" {
    const lines = ".......................................\n..*.......*...*.....*...*......**.**...\n....*.................*.......*..*..*..\n..*.........*.......*...*.....*.....*..\n......................*........*...*...\n..*.*.....*...*.....*...*........*.....\n.......................................\n";
    try expectEqual(15624, try answer3(std.testing.allocator, lines));
}
