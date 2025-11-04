const std = @import("std");
const expectEqual = std.testing.expectEqual;

pub fn numBeetlesGreedy(target: u32, stamps: []const u32) u32 {
    // assumption: stamps is sorted in descending order
    // assumption: greedy solution will be good enough (no longer true in part 2)
    var beetles: u32 = 0;
    var hitting_score = target;

    for (stamps) |stamp| {
        while (hitting_score >= stamp) {
            hitting_score -= stamp;
            beetles += 1;
        }

        if (hitting_score == 0) {
            return beetles;
        }
    }

    unreachable;
}

pub fn numBeetlesDynamic(memo: []u32, target: u32) u32 {
    if (memo[target] != 0) {
        // std.debug.print("min beetles for {d} is {d}\n", .{ target, memo[target] });
        return memo[target];
    }

    // otherwise try every addition combo :|
    var min_beetles: u32 = std.math.maxInt(u32);
    // can actually do this halfway
    var i: u32 = 1;
    while (i <= target - i) : (i += 1) {
        const l = numBeetlesDynamic(memo, target - i);
        const r = numBeetlesDynamic(memo, i);
        min_beetles = @min(min_beetles, l + r);
    }

    memo[target] = min_beetles;
    // std.debug.print("min beetles for {d} is {d}\n", .{ target, min_beetles });

    return min_beetles;
}

pub fn answer1(allocator: std.mem.Allocator, targets: []const u32, stamps: []const u32) !u32 {
    const mut_stamps = try allocator.alloc(u32, stamps.len);
    defer allocator.free(mut_stamps);

    @memcpy(mut_stamps, stamps);
    std.mem.sort(u32, mut_stamps, {}, comptime std.sort.desc(u32));

    var total: u32 = 0;
    for (targets) |target| {
        total += numBeetlesGreedy(target, mut_stamps);
    }

    return total;
}

pub fn answer2(allocator: std.mem.Allocator, targets: []const u32, stamps: []const u32) !u32 {
    var max_target: u32 = 0;
    for (targets) |target| {
        max_target = @max(max_target, target);
    }
    const memo = try allocator.alloc(u32, max_target + 1);
    defer allocator.free(memo);
    for (0..max_target + 1) |i| {
        memo[i] = 0;
    }

    for (stamps) |stamp| {
        memo[stamp] = 1;
    }

    var i: u32 = 1;
    while (i < max_target + 1) : (i += 1) {
        _ = numBeetlesDynamic(memo, i);
    }

    var total: u32 = 0;
    for (targets) |target| {
        total += numBeetlesDynamic(memo, target);
    }

    return total;
}

pub fn answer3(allocator: std.mem.Allocator, targets: []const u32, stamps: []const u32) !u32 {
    var max_target: u32 = 0;
    for (targets) |target| {
        max_target = @max(max_target, target);
    }
    const memo = try allocator.alloc(u32, max_target + 1);
    defer allocator.free(memo);
    for (0..max_target + 1) |i| {
        memo[i] = 0;
    }

    for (stamps) |stamp| {
        memo[stamp] = 1;
    }

    var i: u32 = 1;
    while (i < (max_target / 2) + 100) : (i += 1) {
        _ = numBeetlesDynamic(memo, i);
    }

    var total: u32 = 0;
    for (targets) |target| {
        var min_beetles: u32 = std.math.maxInt(u32);
        for (0..101) |diff| {
            const midpoint = (target - diff) / 2;
            const left: u32 = @intCast(midpoint);
            const right: u32 = @intCast(midpoint + diff);

            if (left + right != target) {
                continue;
            }

            min_beetles = @min(min_beetles, numBeetlesDynamic(memo, left) + numBeetlesDynamic(memo, right));
        }
        std.debug.print("computed min for {d}\n", .{target});
        total += min_beetles;
    }

    return total;
}

// pub fn answer3(allocator: std.mem.Allocator, targets: []const u32, stamps: []const u32) !u32 {}

test "integer division" {
    try expectEqual(80206, 160413 / 2);
}

test "given example" {
    const allocator = std.testing.allocator;
    try expectEqual(10, answer1(allocator, &[_]u32{ 2, 4, 7, 16 }, &[_]u32{ 1, 3, 5, 10 }));
}

test "given example (part 2)" {
    const allocator = std.testing.allocator;
    try expectEqual(10, answer2(allocator, &[_]u32{ 33, 41, 55, 99 }, &[_]u32{ 1, 3, 5, 10, 15, 16, 20, 24, 25, 30 }));
}

test "given example (part 3)" {
    const allocator = std.testing.allocator;
    try expectEqual(10449, answer3(allocator, &[_]u32{ 156488, 352486, 546212 }, &[_]u32{ 1, 3, 5, 10, 15, 16, 20, 24, 25, 30, 37, 38, 49, 50, 74, 75, 100, 101 }));
}

test "feasibility (part 3)" {
    // this example is actually >> the example input
    // const allocator = std.testing.allocator;
    // try expectEqual(10, answer2(allocator, &[_]u32{546212}, &[_]u32{ 1, 3, 5, 10, 15, 16, 20, 24, 25, 30, 37, 38, 49, 50, 74, 75, 100, 101 }));
}
