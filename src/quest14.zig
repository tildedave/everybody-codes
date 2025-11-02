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

pub fn answer3(allocator: std.mem.Allocator, instructions: []const u8) !u32 {
    var segments = std.AutoHashMap(Coord, bool).init(allocator);
    defer segments.deinit();
    var leafs = std.AutoHashMap(Coord, bool).init(allocator);
    defer leafs.deinit();

    try growBranches(instructions, &segments, &leafs);

    // now Dijkstra search from each leaf, remember the distances to Coord{0, y, 0} types
    // seems like we can do that with a Coord{0, y, 0} -> list of distances structure

    var stalk_distance = std.AutoHashMap(Coord, std.ArrayList(u32)).init(allocator);
    defer {
        var val_it = stalk_distance.valueIterator();
        while (val_it.next()) |p| {
            allocator.free(p.*);
        }
        stalk_distance.deinit();
    }

    var leaf_it = leafs.iterator();
    while (leaf_it.next()) |leaf_e| {
        const start_coord = leaf_e.key_ptr.*;

        var distances = std.AutoHashMap(Coord, u64).init(allocator);
        defer distances.deinit();
        var visited = std.AutoHashMap(Coord, bool).init(allocator);
        defer visited.deinit();
        var frontier = std.AutoHashMap(Coord, bool).init(allocator);
        defer frontier.deinit();

        try distances.put(start_coord, 0);
        try frontier.put(start_coord, true);

        while (frontier.count() > 0) {
            var frontier_it = frontier.iterator();

            var node_dist: u64 = std.math.maxInt(u64);
            var node: ?Coord = null;
            while (frontier_it.next()) |e| {
                if (distances.get(e.key_ptr.*)) |d| {
                    if (d < node_dist) {
                        node = e.key_ptr.*;
                        node_dist = d;
                    }
                }
            }

            try visited.put(node.?, true);
            _ = frontier.remove(node.?);

            const x = node.?.@"0";
            const y = node.?.@"1";
            const z = node.?.@"2";
            const d = node_dist;

            // otherwise look at the neighbors
            // neighbors are +-1 x, y, z

            var neighbors = try std.ArrayList(Coord).initCapacity(allocator, 6);
            defer neighbors.deinit(allocator);

            for ([]const i64{ -1, 1 }) |dx| {
                const neighbor_candidate = Coord{ x + dx, y, z };
                if (segments.conatins(neighbor_candidate)) {
                    neighbors.addOneAssumeCapacity(neighbor_candidate);
                }
            }
            for ([]const i64{ -1, 1 }) |dy| {
                const neighbor_candidate = Coord{ x, y + dy, z };
                if (segments.conatins(neighbor_candidate)) {
                    neighbors.addOneAssumeCapacity(neighbor_candidate);
                }
            }
            for ([]const i64{ -1, 1 }) |dz| {
                const neighbor_candidate = Coord{ x, y, z + dz };
                if (segments.conatins(neighbor_candidate)) {
                    neighbors.addOneAssumeCapacity(neighbor_candidate);
                }
            }

            for (0..neighbors.items.len) |i| {
                const n: Coord = neighbors.items[i];
                if (visited.contains(n)) {
                    continue;
                }

                try frontier.put(n, true);
                if (distances.get(n)) |neighbor_dist| {
                    if (d + 1 < neighbor_dist) {
                        distances.putAssumeCapacity(n, d + 1);
                    }
                } else {
                    try distances.put(n, d + 1);
                }
            }
        }
    }
}

test "given example (part 1)" {
    const instructions = "U5,R3,D2,L5,U4,R5,D2\n";
    try expectEqual(7, answer1(instructions));
}

test "given example (part 2)" {
    const instructions = "U5,R3,D2,L5,U4,R5,D2\nU6,L1,D2,R3,U2,L1\n";
    try expectEqual(32, answer2(std.testing.allocator, instructions));
}
