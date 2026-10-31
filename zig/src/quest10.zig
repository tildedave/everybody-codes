const util = @import("util.zig");
const std = @import("std");
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

fn wordPower(str: []const u8) u64 {
    var total: u64 = 0;
    for (str, 0..) |s, i| {
        if (s == '.') {
            return 0;
        }
        total += (s - 'A' + 1) * (i + 1);
    }
    return total;
}

test "base power" {
    try expectEqual(25, 'Y' - 'A' + 1);
    try expectEqual(1851, wordPower("PTBVRCZHFLJWGMNS"));
}

fn solveLetter(allocator: std.mem.Allocator, grid: util.Grid, start_x: usize, start_y: usize, dx: usize, dy: usize) !bool {
    const x = start_x + dx;
    const y = start_y + dy;
    const current_idx = util.index(&grid, x, y);
    // OK, relevant letters are (start_x - 1, start_y + dy), (start_x - 2, start_y + dy) etc

    const pairs = [_]usize{
        util.index(&grid, start_x - 1, y),
        util.index(&grid, start_x - 2, y),
        util.index(&grid, start_x + 4, y),
        util.index(&grid, start_x + 5, y),
        util.index(&grid, x, start_y - 1),
        util.index(&grid, x, start_y - 2),
        util.index(&grid, x, start_y + 4),
        util.index(&grid, x, start_y + 5),
    };

    var seen = std.AutoHashMap(u8, u32).init(allocator);
    defer seen.deinit();

    var qstmark_idx: ?usize = null;

    for (pairs) |idx| {
        const ch = grid.lines[idx];
        if (ch == '?') {
            qstmark_idx = idx;
        }
        if (seen.get(ch)) |v| {
            try seen.put(ch, v + 1);
        } else {
            try seen.put(ch, 1);
        }
    }

    var letter: ?u8 = null;
    var it = seen.iterator();
    while (it.next()) |e| {
        if (e.key_ptr.* == '?') {
            continue;
        }

        if (e.value_ptr.* == 2) {
            letter = e.key_ptr.*;
        }

        // This case can never happen in a solvable block.
        if (e.value_ptr.* > 2) {
            return false;
        }
    }

    if (letter == null and qstmark_idx != null) {
        // need to look at everything in the row and column for the given x/y
        var visible = std.AutoHashMap(u8, u32).init(allocator);
        defer visible.deinit();

        var unsolvable = false;

        // std.debug.print("solving question mark ({d}, {d}) --> ", .{ x, y });

        for (0..4) |ddx| {
            const idx = util.index(&grid, start_x + ddx, y);
            if (idx == current_idx) {
                continue;
            }
            const ch = grid.lines[idx];
            if (ch == '.') {
                unsolvable = true;
                break;
            }
            if (visible.get(ch)) |v| {
                try visible.put(ch, v + 1);
                // std.debug.print("{c}", .{ch});
            } else {
                try visible.put(ch, 1);
                // std.debug.print("{c}", .{ch});
            }
        }

        for (0..4) |ddy| {
            const idx = util.index(&grid, x, start_y + ddy);
            if (idx == current_idx) {
                continue;
            }
            const ch = grid.lines[idx];
            if (ch == '.') {
                unsolvable = true;
                break;
            }
            if (visible.get(ch)) |v| {
                try visible.put(ch, v + 1);
                // std.debug.print("{c}", .{ch});
            } else {
                try visible.put(ch, 1);
                // std.debug.print("{c}", .{ch});
            }
        }

        // std.debug.print("\n", .{});

        if (!unsolvable) {
            // std.debug.print("not unsolvable!\n", .{});

            var seen_it = seen.iterator();
            while (seen_it.next()) |e| {
                const ch = e.key_ptr.*;
                const cnt = e.value_ptr.*;

                if (visible.get(ch)) |v| {
                    try visible.put(ch, v + cnt);
                    // std.debug.print("{c}", .{ch});
                } else {
                    try visible.put(ch, cnt);
                    // std.debug.print("{c}", .{ch});
                }
            }

            var visible_it = visible.iterator();
            while (visible_it.next()) |e| {
                std.debug.print("{c}", .{e.key_ptr.*});
                const ch = e.key_ptr.*;
                if (ch != '?' and e.value_ptr.* == 1) {
                    if (letter != null) {
                        unsolvable = true;
                    }
                    letter = ch;
                }
            }

            if (unsolvable) {
                letter = null;
            }
            std.debug.print(" ({d})\n", .{visible.count()});

            if (letter) |l| {
                // std.debug.print("solved as {c}!\n", .{l});
                grid.lines[qstmark_idx.?] = l;
            } else {
                std.debug.print("not solvable\n", .{});
                var vit = visible.iterator();
                while (vit.next()) |e| {
                    std.debug.print("{c}", .{e.key_ptr.*});
                }
                std.debug.print("\n", .{});
            }
        }
    }

    if (letter) |l| {
        grid.lines[current_idx] = l;
        return true;
    }

    return false;
}

fn solveBlock(allocator: std.mem.Allocator, grid: util.Grid, start_x: usize, start_y: usize) !bool {
    var any_solved = false;
    for (0..4) |dy| {
        for (0..4) |dx| {
            if (grid.lines[util.index(&grid, start_x + dx, start_y + dy)] == '.') {
                any_solved = try solveLetter(allocator, grid, start_x, start_y, dx, dy) or any_solved;
            }
        }
    }

    return any_solved;
}

fn readBlock(allocator: std.mem.Allocator, grid: util.Grid, start_x: usize, start_y: usize) ![]const u8 {
    var result = try allocator.alloc(u8, 16);
    for (0..16) |i| {
        result[i] = '.';
    }
    var result_idx: usize = 0;

    for (0..4) |dy| {
        for (0..4) |dx| {
            const x = start_x + dx;
            const y = start_y + dy;
            result[result_idx] = grid.lines[util.index(&grid, x, y)];
            result_idx += 1;
        }
    }

    return result;
}

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8) ![]const u8 {
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    _ = try solveBlock(allocator, grid, 2, 2);
    return readBlock(allocator, grid, 2, 2);
}

pub fn answer2(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    var total: u64 = 0;
    var y: usize = 2;
    while (y <= grid.height - 6) : (y += 9) {
        var x: usize = 2;
        while (x <= grid.width - 6) : (x += 9) {
            _ = try solveBlock(allocator, grid, x, y);
            const word = try readBlock(allocator, grid, x, y);
            total += wordPower(word);
            allocator.free(word);
        }
    }

    return total;
}

pub fn answer3(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    var solved_any = true;
    while (solved_any) {
        solved_any = false;
        var y: usize = 2;
        while (y <= grid.height - 6) : (y += 6) {
            var x: usize = 2;
            while (x <= grid.width - 6) : (x += 6) {
                solved_any = try solveBlock(allocator, grid, x, y) or solved_any;
            }
        }
    }

    std.debug.print("--------\n", .{});
    var it = std.mem.splitScalar(u8, grid.lines, '\n');
    while (it.next()) |line| {
        std.debug.print("{s}\n", .{line});
    }

    var total: u64 = 0;
    var y: usize = 2;
    while (y <= grid.height - 6) : (y += 6) {
        var x: usize = 2;
        while (x <= grid.width - 6) : (x += 6) {
            const w = try readBlock(
                allocator,
                grid,
                x,
                y,
            );
            std.debug.print("{s}\n", .{w});
            total += wordPower(w);
            defer allocator.free(w);
        }
    }

    return total;
}

test "given example (part 1)" {
    const lines = "**PCBS**\n**RLNW**\nBV....PT\nCR....HZ\nFL....JW\nSG....MN\n**FTZV**\n**GMJH**\n";
    const result = try answer1(std.testing.allocator, lines);
    defer std.testing.allocator.free(result);
    std.debug.print("{s}\n", .{result});
}

test "double example (part 2)" {
    const lines = "**QGSF** **PSHM**\n**RMPC** **GLWD**\nGX....DR XD....GS\nTW....MC ML....NP\nFN....QP WF....TQ\nJV....SL HB....CZ\n**WNXD** **QCNF**\n**VLTJ** **BTXZ**\n                 \n**VNPK** **MWGV**\n**RWBQ** **RZLP**\nFB....ZM NS....DQ\nHJ....VX MV....CK\nPW....QC PR....ZL\nTK....RN GB....XW\n**CHTX** **DQSB**\n**FZJM** **XKCN**\n";
    try expectEqual(wordPower("RGXDWMTCQNPFVLSJ") + wordPower("FZBMVHJXCWPQRNTK") + wordPower("GSXDPLNMQTWFBCHZ") + wordPower("DQSNMKCVRZLPXWGB"), try answer2(std.testing.allocator, lines));
}

test "example (part 3)" {
    const lines = "**XFZB**DCST**\n**LWQK**GQJH**\n?G....WL....DQ\nBS....H?....CN\nP?....KJ....TV\nNM....Z?....SG\n**NSHM**VKWZ**\n**PJGV**XFNL**\nWQ....?L....YS\nFX....DJ....HV\n?Y....WM....?J\nTJ....YK....LP\n**XRTK**BMSP**\n**DWZN**GCJV**\n";
    const allocator = std.testing.allocator;

    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    _ = try solveBlock(allocator, grid, 2, 2);
    const w = try readBlock(allocator, grid, 2, 2);
    std.debug.print("{s}\n", .{w});
    defer allocator.free(w);

    _ = try solveBlock(allocator, grid, 2, 2);
    const word = try readBlock(allocator, grid, 2, 2);
    defer allocator.free(word);
    try expectEqualStrings("LWGVXSHBPJQKNFZM", word);

    // const word: []u8 = try runicWord(allocator, grid, 2, 2);
    // defer std.testing.allocator.free(word);
}

test "full part 3" {
    const lines = "**XFZB**DCST**\n**LWQK**GQJH**\n?G....WL....DQ\nBS....H?....CN\nP?....KJ....TV\nNM....Z?....SG\n**NSHM**VKWZ**\n**PJGV**XFNL**\nWQ....?L....YS\nFX....DJ....HV\n?Y....WM....?J\nTJ....YK....LP\n**XRTK**BMSP**\n**DWZN**GCJV**\n";
    const allocator = std.testing.allocator;

    try expectEqual(3889, try answer3(allocator, lines));
}
