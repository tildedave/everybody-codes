const std = @import("std");
const expectEqual = std.testing.expectEqual;
const util = @import("util.zig");

pub fn answer1(instructions: []const u8) !u64 {
    var it = std.mem.splitScalar(u8, instructions[0 .. instructions.len - 1], ',');
    var x: i64 = 0;
    var y: i64 = 0;
    var z: i64 = 0;
    var max_y: u64 = std.math.minInt(u64);

    while (it.next()) |instr| {
        const num = try std.fmt.parseInt(u32, instr[1..], 10);
        switch (instr[0]) {
            'U' => {
                y += num;
            },
            'D' => {
                y -= num;
            },
            'R' => {
                x -= num;
            },
            'L' => {
                x += num;
            },
            'F' => {
                z -= num;
            },
            'B' => {
                z += num;
            },
            else => unreachable,
        }

        max_y = @max(max_y, y);
    }

    return max_y;
}

const Coord = struct { i64, i64, i64 };

pub fn answer2(allocator: std.mem.Allocator, instructions: []const u8) !u32 {
    var segments = std.AutoHashMap(Coord, bool).init(allocator);
    defer segments.deinit();
    var leafs = std.AutoHashMap(Coord, bool).init(allocator);
    defer leafs.deinit();

    try growBranches(instructions, &segments, &leafs);

    return segments.count();
}

fn growBranches(instructions: []const u8, segments: *std.AutoHashMap(Coord, bool), leafs: *std.AutoHashMap(Coord, bool)) !void {
    var lines = std.mem.splitScalar(u8, instructions[0 .. instructions.len - 1], '\n');
    while (lines.next()) |line| {
        var it = std.mem.splitScalar(u8, line, ',');
        var x: i64 = 0;
        var y: i64 = 0;
        var z: i64 = 0;

        while (it.next()) |instr| {
            const num = try std.fmt.parseInt(u32, instr[1..], 10);
            switch (instr[0]) {
                'U' => {
                    for (1..num + 1) |n| {
                        const dy: i64 = @intCast(n);
                        try segments.put(Coord{ x, y + dy, z }, true);
                    }
                    y += num;
                },
                'D' => {
                    for (1..num + 1) |n| {
                        const dy: i64 = @intCast(n);
                        try segments.put(Coord{ x, y - dy, z }, true);
                    }
                    y -= num;
                },
                'R' => {
                    for (1..num + 1) |n| {
                        const dx: i64 = @intCast(n);
                        try segments.put(Coord{ x - dx, y, z }, true);
                    }
                    x -= num;
                },
                'L' => {
                    for (1..num + 1) |n| {
                        const dx: i64 = @intCast(n);
                        try segments.put(Coord{ x + dx, y, z }, true);
                    }
                    x += num;
                },
                'F' => {
                    for (1..num + 1) |n| {
                        const dz: i64 = @intCast(n);
                        try segments.put(Coord{ x, y, z - dz }, true);
                    }
                    z -= num;
                },
                'B' => {
                    for (1..num + 1) |n| {
                        const dz: i64 = @intCast(n);
                        try segments.put(Coord{ x, y, z + dz }, true);
                    }
                    z += num;
                },
                else => unreachable,
            }
        }

        try leafs.put(Coord{ x, y, z }, true);
    }
}

const SearchContext = struct {
    segments: std.AutoHashMap(Coord, bool),
};

fn coordNeighbors(
    node: Coord,
    allocator: std.mem.Allocator,
    neighbors: *std.ArrayList(Coord),
    context: SearchContext,
) error{OutOfMemory}!void {
    const x = node.@"0";
    const y = node.@"1";
    const z = node.@"2";

    for ([2]i64{ -1, 1 }) |dx| {
        const neighbor_candidate = Coord{ x + dx, y, z };
        if (context.segments.contains(neighbor_candidate)) {
            try neighbors.append(allocator, neighbor_candidate);
        }
    }
    for ([2]i64{ -1, 1 }) |dy| {
        const neighbor_candidate = Coord{ x, y + dy, z };
        if (context.segments.contains(neighbor_candidate)) {
            try neighbors.append(allocator, neighbor_candidate);
        }
    }
    for ([2]i64{ -1, 1 }) |dz| {
        const neighbor_candidate = Coord{ x, y, z + dz };
        if (context.segments.contains(neighbor_candidate)) {
            try neighbors.append(allocator, neighbor_candidate);
        }
    }
}

fn coordDistance(_: Coord, _: Coord, _: SearchContext) u32 {
    return 1;
}

pub fn answer3(allocator: std.mem.Allocator, instructions: []const u8) !u32 {
    var segments = std.AutoHashMap(Coord, bool).init(allocator);
    defer segments.deinit();
    var leafs = std.AutoHashMap(Coord, bool).init(allocator);
    defer leafs.deinit();

    try growBranches(instructions, &segments, &leafs);

    // now Dijkstra search from each leaf, remember the distances to Coord{0, y, 0} types
    // seems like we can do that with a Coord{0, y, 0} -> list of distances structure

    var stalk_distances = std.AutoHashMap(Coord, std.ArrayList(u32)).init(allocator);
    defer {
        var val_it = stalk_distances.valueIterator();
        while (val_it.next()) |p| {
            p.*.deinit(allocator);
        }
        stalk_distances.deinit();
    }

    const search_context = SearchContext{
        .segments = segments,
    };

    var leaf_it = leafs.iterator();
    while (leaf_it.next()) |leaf_e| {
        const start_coord: Coord = leaf_e.key_ptr.*;

        var distances = std.AutoHashMap(Coord, u32).init(allocator);
        defer distances.deinit();

        try util.dijkstraSearch(Coord, allocator, start_coord, &distances, search_context, coordNeighbors, coordDistance);
        var distances_it = distances.iterator();
        while (distances_it.next()) |e| {
            const coord: Coord = e.key_ptr.*;
            const distance = e.value_ptr.*;
            if (coord.@"0" != 0 or coord.@"2" != 0) {
                continue;
            }

            if (stalk_distances.get(coord)) |l| {
                var _l = l;
                try _l.append(allocator, distance);
                try stalk_distances.put(coord, _l);
            } else {
                var l: std.ArrayList(u32) = try std.ArrayList(u32).initCapacity(allocator, 0);
                try l.append(allocator, distance);
                try stalk_distances.put(coord, l);
            }
        }
    }

    var min_murkiness: u32 = std.math.maxInt(u32);
    var stalk_it = stalk_distances.iterator();

    while (stalk_it.next()) |e| {
        const l = e.value_ptr.*;
        var total: u32 = 0;
        for (0..l.items.len) |i| {
            total += l.items[i];
        }

        min_murkiness = @min(min_murkiness, total);
    }

    return min_murkiness;
}

test "given example (part 1)" {
    const instructions = "U5,R3,D2,L5,U4,R5,D2\n";
    try expectEqual(7, answer1(instructions));
}

test "given example (part 2)" {
    const instructions = "U5,R3,D2,L5,U4,R5,D2\nU6,L1,D2,R3,U2,L1\n";
    try expectEqual(32, answer2(std.testing.allocator, instructions));
}

test "given examples (part 3)" {
    const instructions = "U5,R3,D2,L5,U4,R5,D2\nU6,L1,D2,R3,U2,L1\n";
    try expectEqual(5, answer3(std.testing.allocator, instructions));

    const instructions2 = "U20,L1,B1,L2,B1,R2,L1,F1,U1\nU10,F1,B1,R1,L1,B1,L1,F1,R2,U1\nU30,L2,F1,R1,B1,R1,F2,U1,F1\nU25,R1,L2,B1,U1,R2,F1,L2\nU16,L1,B1,L1,B3,L1,B1,F1\n";
    try expectEqual(46, answer3(std.testing.allocator, instructions2));
}
