const std = @import("std");
const util = @import("util.zig");
const expectEqual = std.testing.expectEqual;

pub fn answer1(lines: []const u8) u32 {
    var lowest: ?u32 = null;
    var it = util.NumberIterator{ .lines = lines };

    while (it.next()) |num| {
        if (lowest == null or num < lowest.?) {
            lowest = num;
        }
    }
    var total: u32 = 0;
    it = util.NumberIterator{ .lines = lines };
    while (it.next()) |num| {
        total += (num - lowest.?);
    }
    return total;
}

test "answer1 - given example" {
    const lines = "3\n4\n7\n8\n";
    try expectEqual(10, answer1(lines));
}

pub fn answer3(lines: []const u8) u32 {
    var it = util.NumberIterator{ .lines = lines };
    var lowest: ?u32 = null;
    while (it.next()) |num| {
        var total: u32 = 0;
        var it2 = util.NumberIterator{ .lines = lines };
        while (it2.next()) |n| {
            if (num < n) {
                total += (n - num);
            } else {
                total += (num - n);
            }
        }
        if (lowest == null or total < lowest.?) {
            lowest = total;
        }
    }

    return lowest.?;
}

test "answer3 - given example" {
    // |n - 2| + |n - 4| + |n - 5| + |n - 6| + |n - 8|
    const lines = "2\n4\n5\n6\n8\n";
    try expectEqual(8, answer3(lines));
}
