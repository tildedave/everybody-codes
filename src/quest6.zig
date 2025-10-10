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

const SearchNode = struct {
    distance: u32,
    pred: ?[]const u8,
};

fn getPath(allocator: std.mem.Allocator, node: []const u8, search_map: *std.StringHashMap(SearchNode), final_answer: bool) !struct { []const u8, u32 } {
    var result_string = try std.ArrayList(u8).initCapacity(allocator, 0);
    defer result_string.deinit(allocator);

    const writer = result_string.writer(allocator);
    try std.fmt.format(writer, "@", .{});

    var curr: ?[]const u8 = node;
    var len: u32 = 0;
    while (curr != null) {
        if (final_answer) {
            try std.fmt.format(writer, "{c}", .{curr.?[0]});
        } else {
            for (0..curr.?.len) |i| {
                try std.fmt.format(writer, "{c}", .{curr.?[curr.?.len - 1 - i]});
            }
        }
        curr = search_map.get(curr.?).?.pred;
        len += 1;
    }

    const result = try allocator.alloc(u8, result_string.items.len);
    for (0..result.len) |i| {
        result[result.len - i - 1] = result_string.items[i];
    }

    return struct { []const u8, u32 }{ result, len };
}

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8) ![]const u8 {
    var map = try parseLines(lines, allocator);
    defer freeMap(&map, allocator);

    // need to DFS from RR, maintain predecessor of each
    // so need a queue, a predecessor map, and a distance map

    var stack = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer stack.deinit(allocator);
    try stack.append(allocator, "RR");

    var search_map = std.StringHashMap(SearchNode).init(allocator);
    defer search_map.deinit();
    try search_map.put("RR", SearchNode{ .distance = 0, .pred = null });

    var paths = std.StringHashMap(struct { []const u8, u32 }).init(allocator);

    while (stack.items.len > 0) {
        const prev = stack.pop().?;
        std.debug.print("beep {s} {any}\n", .{ prev, map.get(prev) });
        if (map.get(prev) == null) {
            continue;
        }

        const search_node = search_map.get(prev).?;
        const children = map.get(prev).?;
        for (children.items) |child| {
            if (std.mem.eql(u8, child, "@")) {
                // get path to root and put it in the
                const arr = try getPath(allocator, prev, &search_map, false);
                try paths.put(arr[0], struct { []const u8, u32 }{ prev, arr[1] });
            } else if (std.mem.eql(u8, child, "BUG")) {} else if (std.mem.eql(u8, child, "ANT")) {} else {
                std.debug.print("curr {s} child {s}\n", .{ prev, child });
                try stack.append(allocator, child);
                try search_map.put(child, SearchNode{ .distance = search_node.distance + 1, .pred = prev });
            }
        }
    }

    var counts = std.AutoHashMap(u32, u32).init(allocator);
    defer counts.deinit();

    var paths_it = paths.valueIterator();
    while (paths_it.next()) |v| {
        const path_length = v.*[1];
        if (counts.get(path_length) == null) {
            try counts.put(path_length, 1);
        } else {
            try counts.put(path_length, counts.get(path_length).? + 1);
        }
    }

    var counts_it = counts.keyIterator();
    var unique_path_length: ?u32 = null;
    while (counts_it.next()) |v| {
        if (counts.get(v.*) == 1) {
            unique_path_length = v.*;
            break;
        }
    }

    std.debug.print("unique length {d}\n", .{unique_path_length.?});
    // must clean everything
    var key_it = paths.keyIterator();
    while (key_it.next()) |k| {
        const path = paths.get(k.*);
        if (path.?[1] == unique_path_length) {
            const p = try getPath(allocator, path.?[0], &search_map, true);

            return p[0];
        }
    }

    return "joe";
}

test "example" {
    const allocator = std.testing.allocator;
    const result = try answer1(allocator, "RR:A,B,C\nA:D,E\nB:F,@\nC:G,H\nD:@\nE:@\nF:@\nG:@\nH:@\n");
    std.debug.print("{s}\n", .{result});
    defer allocator.free(result);
}
