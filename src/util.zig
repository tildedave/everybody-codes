const std = @import("std");
const expectEqual = std.testing.expectEqual;

pub fn nthIndexOfScalar(comptime T: type, slice: []const T, value: T, n: usize) ?usize {
    var pos: usize = 0;
    for (0..n + 1) |idx| {
        const start_pos = if (idx == 0) pos else pos + 1;
        const next_pos = std.mem.indexOfScalarPos(T, slice, start_pos, value);
        if (next_pos == null) {
            return null;
        }
        pos = next_pos.?;
    }

    return pos;
}

test "nthIndexOfScalar" {
    try expectEqual(0, nthIndexOfScalar(u8, "aaaa", 'a', 0));
    try expectEqual(1, nthIndexOfScalar(u8, "aaaa", 'a', 1));
    try expectEqual(2, nthIndexOfScalar(u8, "aaaa", 'a', 2));
    try expectEqual(3, nthIndexOfScalar(u8, "aaaa", 'a', 3));
}

test "modulus" {
    try expectEqual(5, (-1) % 6);
}
