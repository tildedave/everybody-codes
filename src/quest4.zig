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
