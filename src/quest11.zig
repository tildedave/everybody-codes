const std = @import("std");
const expectEqual = std.testing.expectEqual;

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8, days: usize) !u64 {
    return populationCount(allocator, lines, "A", days);
}

pub fn answer2(allocator: std.mem.Allocator, lines: []const u8, days: usize) !u64 {
    // we don't really know if we're going to be asked to be order dependent,
    // so at first let's assume not order dependency
    return populationCount(allocator, lines, "Z", days);
}

pub fn answer3(allocator: std.mem.Allocator, lines: []const u8, days: usize) !u64 {
    var smallest: u64 = std.math.maxInt(u64);
    var largest: u64 = std.math.minInt(u64);

    var line_it = std.mem.splitScalar(u8, lines, '\n');
    while (line_it.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        var colon_it = std.mem.splitScalar(u8, line, ':');
        const lhs = colon_it.next().?;
        const result = try populationCount(allocator, lines, lhs, days);
        if (result < smallest) {
            smallest = result;
        }
        if (result > largest) {
            largest = result;
        }
    }

    return largest - smallest;
}

fn populationCount(allocator: std.mem.Allocator, lines: []const u8, initial: []const u8, days: usize) !u64 {
    // we don't really know if we're going to be asked to be order dependent,
    // so at first let's assume not order dependency
    var termite_list = std.StringHashMap(u64).init(allocator);
    defer termite_list.deinit();
    try termite_list.put(initial, 1);

    var line_it = std.mem.splitScalar(u8, lines, '\n');

    var i: usize = 0;
    while (i < days) : (i += 1) {
        var next_termite_list = std.StringHashMap(u64).init(allocator);
        defer next_termite_list.deinit();
        line_it.reset();

        while (line_it.next()) |line| {
            if (line.len == 0) {
                continue;
            }

            var colon_it = std.mem.splitScalar(u8, line, ':');
            const lhs = colon_it.next().?;
            const rhs = colon_it.next().?;

            if (termite_list.get(lhs)) |termite_cnt| {
                var comma_it = std.mem.splitScalar(u8, rhs, ',');
                while (comma_it.next()) |b| {
                    if (next_termite_list.get(b)) |b_cnt| {
                        try next_termite_list.put(b, b_cnt + termite_cnt);
                    } else {
                        try next_termite_list.put(b, termite_cnt);
                    }
                }
            }
        }

        termite_list.clearAndFree();
        var next_termite_it = next_termite_list.iterator();
        while (next_termite_it.next()) |e| {
            try termite_list.put(e.key_ptr.*, e.value_ptr.*);
        }
    }

    var total: usize = 0;
    var termite_it = termite_list.iterator();
    while (termite_it.next()) |e| {
        total += e.value_ptr.*;
    }

    return total;
}

test "given example (part 1)" {
    const lines = "A:B,C\nB:C,A\nC:A\n";
    try expectEqual(8, answer1(std.testing.allocator, lines, 4));
}

test "given example (part 3)" {
    const lines = "A:B,C\nB:C,A,A\nC:A\n";
    try expectEqual(268815, answer3(std.testing.allocator, lines, 20));
}
