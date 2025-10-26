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

fn solveBlock(allocator: std.mem.Allocator, grid: util.Grid, start_x: usize, start_y: usize) !void {
    for (0..4) |dy| {
        for (0..4) |dx| {
            const x = start_x + dx;
            const y = start_y + dy;
            // OK, relevant letters are (start_x - 1, start_y + dy), (start_x - 2, start_y + dy) etc

            const pairs = [_]usize{
                util.index(grid, start_x - 1, y),
                util.index(grid, start_x - 2, y),
                util.index(grid, start_x + 4, y),
                util.index(grid, start_x + 5, y),
                util.index(grid, x, start_y - 1),
                util.index(grid, x, start_y - 2),
                util.index(grid, x, start_y + 4),
                util.index(grid, x, start_y + 5),
            };

            var seen = std.AutoHashMap(u8, u32).init(allocator);
            defer seen.deinit();

            for (pairs) |idx| {
                const ch = grid.lines[idx];
                if (seen.get(ch)) |v| {
                    try seen.put(ch, v + 1);
                } else {
                    try seen.put(ch, 1);
                }
            }

            var letter: ?u8 = null;
            var key_it = seen.keyIterator();
            while (key_it.next()) |ch| {
                if (seen.get(ch.*) == 2) {
                    letter = ch.*;
                }
            }

            if (letter) |l| {
                grid.lines[util.index(grid, x, y)] = l;
            }
        }
    }
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
            result[result_idx] = grid.lines[util.index(grid, x, y)];
            result_idx += 1;
        }
    }

    return result;
}

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8) ![]const u8 {
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    try solveBlock(allocator, grid, 2, 2);
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
            try solveBlock(allocator, grid, x, y);
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

    return 0;
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

    // const word: []u8 = try runicWord(allocator, grid, 2, 2);
    // defer std.testing.allocator.free(word);
    // try solveWord(allocator, word, grid, 2, 2);
    // try expectEqualStrings("LWGVXSHBPJQKNFZM", word);
}

test "full part 3" {
    const lines = "**XFZB**DCST**\n**LWQK**GQJH**\n?G....WL....DQ\nBS....H?....CN\nP?....KJ....TV\nNM....Z?....SG\n**NSHM**VKWZ**\n**PJGV**XFNL**\nWQ....?L....YS\nFX....DJ....HV\n?Y....WM....?J\nTJ....YK....LP\n**XRTK**BMSP**\n**DWZN**GCJV**\n";
    const allocator = std.testing.allocator;

    try expectEqual(3889, try answer3(allocator, lines));
}
