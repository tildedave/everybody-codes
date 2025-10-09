const std = @import("std");

fn parseLines(lines: []const u8, allocator: std.mem.Allocator) !std.StringHashMap(std.ArrayList([]const u8)) {
    var lines_it = std.mem.splitScalar(u8, lines, '\n');
    var map = std.StringHashMap(std.ArrayList([]const u8)).init(allocator);

    while (lines_it.next()) |line| {
        if (line.len == 0) {
            continue;
        }
        var colon_it = std.mem.splitScalar(u8, line, ':');
        const source = colon_it.first();
        var comma_it = std.mem.splitScalar(u8, colon_it.rest(), ',');
        var list = try std.ArrayList([]const u8).initCapacity(allocator, 0);

        while (comma_it.next()) |dest| {
            std.debug.print("{s} --> {s}\n", .{ source, dest });
            try list.append(allocator, dest);
        }
        try map.put(source, list);
    }

    return map;
}

fn freeMap(map: *std.StringHashMap(std.ArrayList([]const u8)), allocator: std.mem.Allocator) void {
    var val_it = map.valueIterator();
    while (val_it.next()) |v| {
        v.deinit(allocator);
    }
    map.deinit();
}

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8) ![]const u8 {
    var map = try parseLines(lines, allocator);
    defer freeMap(&map, allocator);

    // need to DFS from RR, maintain predecessor of each
    // so need a queue, a predecessor map, and a distance map

    var stack = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer stack.deinit(allocator);
    try stack.append(allocator, "RR");

    var distance_map = std.StringHashMap(u32).init(allocator);
    defer distance_map.deinit();
    try distance_map.put("RR", 0);

    var pred_map = std.StringHashMap([]const u8).init(allocator);
    defer pred_map.deinit();

    var lowest_distance: u32 = std.math.maxInt(u32);
    var lowest_one: []const u8 = "RR";

    while (stack.items.len > 0) {
        const prev = stack.pop().?;
        const prev_distance = distance_map.get(prev).?;
        const children = map.get(prev).?;
        for (children.items) |child| {
            if (std.mem.eql(u8, child, "@")) {
                if (prev_distance < lowest_distance) {
                    lowest_one = prev;
                    lowest_distance = prev_distance;
                }
            } else {
                std.debug.print("curr {s} child {s}\n", .{ prev, child });
                try stack.append(allocator, child);
                try distance_map.put(child, prev_distance + 1);
                try pred_map.put(child, prev);
            }
        }
    }

    std.debug.print("lowest distance is {s}\n", .{lowest_one});

    var result_string = try std.ArrayList(u8).initCapacity(allocator, 0);
    defer result_string.deinit(allocator);

    const writer = result_string.writer(allocator);
    try std.fmt.format(writer, "@", .{});

    var curr: ?[]const u8 = lowest_one;
    while (curr != null) {
        try std.fmt.format(writer, "{s}", .{curr.?});
        curr = pred_map.get(curr.?);
    }

    const result = try allocator.alloc(u8, result_string.items.len);
    for (0..result.len) |i| {
        result[result.len - i - 1] = result_string.items[i];
    }

    return result;
}

test "example" {
    const allocator = std.testing.allocator;
    const result = try answer1(allocator, "RR:A,B,C\nA:D,E\nB:F,@\nC:G,H\nD:@\nE:@\nF:@\nG:@\nH:@\n");
    std.debug.print("{s}\n", .{result});
    defer allocator.free(result);
}
