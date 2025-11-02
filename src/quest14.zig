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
    var lines = std.mem.splitScalar(u8, instructions[0 .. instructions.len - 1], '\n');
    var map = std.AutoHashMap(Coord, bool).init(allocator);
    defer map.deinit();

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
                        try map.put(Coord{ x, y + dy, z }, true);
                    }
                    y += num;
                },
                'D' => {
                    for (1..num + 1) |n| {
                        const dy: i64 = @intCast(n);
                        try map.put(Coord{ x, y - dy, z }, true);
                    }
                    y -= num;
                },
                'R' => {
                    for (1..num + 1) |n| {
                        const dx: i64 = @intCast(n);
                        try map.put(Coord{ x - dx, y, z }, true);
                    }
                    x -= num;
                },
                'L' => {
                    for (1..num + 1) |n| {
                        const dx: i64 = @intCast(n);
                        try map.put(Coord{ x + dx, y, z }, true);
                    }
                    x += num;
                },
                'F' => {
                    for (1..num + 1) |n| {
                        const dz: i64 = @intCast(n);
                        try map.put(Coord{ x, y, z - dz }, true);
                    }
                    z -= num;
                },
                'B' => {
                    for (1..num + 1) |n| {
                        const dz: i64 = @intCast(n);
                        try map.put(Coord{ x, y, z + dz }, true);
                    }
                    z += num;
                },
                else => unreachable,
            }
        }
    }

    return map.count();
}

test "given example (part 1)" {
    const instructions = "U5,R3,D2,L5,U4,R5,D2\n";
    try expectEqual(7, answer1(instructions));
}

test "given example (part 3)" {
    const instructions = "U5,R3,D2,L5,U4,R5,D2\nU6,L1,D2,R3,U2,L1\n";
    try expectEqual(32, answer2(std.testing.allocator, instructions));
}
