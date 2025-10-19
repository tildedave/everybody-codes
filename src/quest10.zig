const util = @import("util.zig");
const std = @import("std");
const expectEqual = std.testing.expectEqual;

fn runicWord(allocator: std.mem.Allocator, grid: util.Grid, start_x: usize, start_y: usize) ![]const u8 {
    var result = try allocator.alloc(u8, 16);
    var result_idx: usize = 0;

    var y: usize = start_y;
    while (y < grid.height - 2) : (y += 1) {
        var x: usize = start_x;
        const idx = util.index(grid, x, y);
        if (idx > grid.lines.len or grid.lines[idx] != '.') {
            break;
        }
        while (x < grid.width - 2) : (x += 1) {
            if (grid.lines[util.index(grid, x, y)] != '.') {
                break;
            }
            var index_seen = std.AutoHashMap(u8, bool).init(allocator);
            defer index_seen.deinit();
            var letter: ?u8 = null;

            for (util.cardinalDirections) |dir| {
                if (letter != null) {
                    break;
                }

                var it = util.DirectionIterator{
                    .grid = grid,
                    .direction = dir,
                    .idx = util.index(grid, x, y),
                };
                while (it.next()) |ch| {
                    if (ch == '.') {
                        continue;
                    }
                    if (ch == ' ') {
                        break;
                    }

                    if (index_seen.contains(ch)) {
                        letter = ch;
                        break;
                    } else {
                        try index_seen.put(ch, true);
                    }
                }
            }
            result[result_idx] = letter.?;
            result_idx += 1;
        }
    }

    return result;
}

fn wordPower(str: []const u8) u64 {
    var total: u64 = 0;
    for (str, 0..) |s, i| {
        total += (s - 'A' + 1) * (i + 1);
    }
    return total;
}

test "base power" {
    try expectEqual(25, 'Y' - 'A' + 1);
    try expectEqual(1851, wordPower("PTBVRCZHFLJWGMNS"));
}

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8) ![]const u8 {
    const grid = util.createGrid(lines);
    return runicWord(allocator, grid, 2, 2);
}

pub fn answer2(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    const grid = util.createGrid(lines);
    var total: u64 = 0;

    var y: usize = 2;
    while (y < grid.height - 2) {
        var x: usize = 2;
        while (x < grid.width - 2) {
            // we know x and y are within a grid (should be '.')
            var i = util.index(grid, x, y);
            if (grid.lines[i] != '.') {
                return 0;
            }
            const word = try runicWord(allocator, grid, x, y);
            defer allocator.free(word);
            total += wordPower(word);
            std.debug.print("({d}, {d}) {s}\n", .{ x, y, word });

            while (grid.lines[i] != ' ' and grid.lines[i] != '\n') : (i += 1) {}
            if (grid.lines[i] == '\n') {
                x = 2;
                break;
            }
            while (grid.lines[i] != '.') : (i += 1) {}
            x = i % (grid.width + 1);
        }
        var i = util.index(grid, x, y);

        while (i < grid.lines.len and grid.lines[i] != ' ' and grid.lines[i] != '\n') : (i += (grid.width + 1)) {}
        if (i >= grid.lines.len) {
            break;
        }
        while (grid.lines[i] != '.') : (i += (grid.width + 1)) {}
        y = i / (grid.width + 1);
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
