const util = @import("util.zig");
const std = @import("std");
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

fn solveLetter(allocator: std.mem.Allocator, grid: util.Grid, original_lines: []const u8, x: usize, y: usize) !?u8 {
    var seen = std.AutoHashMap(u8, u32).init(allocator);
    defer seen.deinit();

    for (util.cardinalDirections) |dir| {
        var it = util.DirectionIterator{
            .grid = grid,
            .direction = dir,
            .idx = util.index(grid, x, y),
        };

        _ = it.next();
        var seen_letter = false;

        // std.debug.print("({d}, {d}) {any} --> ", .{ x, y, dir });
        while (it.next()) |ch| {
            // std.debug.print("{c}", .{ch});
            // boundary checking
            if (original_lines[it.idx] == '.') {
                if (seen_letter) {
                    break;
                }
                continue;
            }
            if (ch == ' ') {
                break;
            }

            if (ch == '?') {
                continue;
            }

            seen_letter = true;
            if (seen.get(ch)) |v| {
                try seen.put(ch, v + 1);
            } else {
                try seen.put(ch, 1);
            }
        }
        // std.debug.print("\n", .{});
    }

    // std.debug.print("solve time\n", .{});

    var letter: ?u8 = null;
    var key_it = seen.keyIterator();
    while (key_it.next()) |ch| {
        if (seen.get(ch.*) == 2) {
            letter = ch.*;
        }
    }

    if (letter) |l| {
        return l;
    }

    return null;
}

fn solveQuestionMark(allocator: std.mem.Allocator, grid: util.Grid, original_lines: []const u8, x: usize, y: usize) !bool {
    var seen = std.AutoHashMap(u8, u32).init(allocator);
    defer seen.deinit();
    var questionmark_idx: ?usize = null;

    for (util.cardinalDirections) |dir| {
        var it = util.DirectionIterator{
            .grid = grid,
            .direction = dir,
            .idx = util.index(grid, x, y),
        };

        _ = it.next();
        var seen_letter = false;

        // std.debug.print("({d}, {d}) {any} --> ", .{ x, y, dir });
        while (it.next()) |ch| {
            // std.debug.print("{c}", .{ch});
            // boundary checking
            if (original_lines[it.idx] == '.') {
                if (seen_letter) {
                    // std.debug.print(" -- done", .{});
                    break;
                }
            }
            if (ch == ' ') {
                break;
            }

            if (ch == '?') {
                if (questionmark_idx != null) {
                    return false;
                }
                questionmark_idx = it.idx;
                continue;
            }

            if (original_lines[it.idx] != '.') {
                seen_letter = true;
            }
            if (seen.contains(ch)) {
                try seen.put(ch, seen.get(ch).? + 1);
            } else {
                // std.debug.print(" (not seen yet) ", .{});
                try seen.put(ch, 1);
            }
        }
        // std.debug.print("\n", .{});
    }

    if (questionmark_idx) |qi| {
        var letter: ?u8 = null;
        var key_it = seen.keyIterator();
        while (key_it.next()) |ch| {
            // std.debug.print("{c} {d}\n", .{ ch.*, seen.get(ch.*).? });
            if (seen.get(ch.*) == 1) {
                letter = ch.*;
            }
        }

        if (letter) |l| {
            // std.debug.print("solve time ({d}, {d}) - missing piece was {c}\n", .{ qi % (grid.width + 1), qi / (grid.width + 1), l });

            grid.lines[qi] = l;
            grid.lines[util.index(grid, x, y)] = l;

            return true;
        }
    }

    return false;
}

fn runicWord(allocator: std.mem.Allocator, grid: util.Grid, start_x: usize, start_y: usize) ![]u8 {
    var result = try allocator.alloc(u8, 16);
    for (0..16) |i| {
        result[i] = '.';
    }

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

            if (try solveLetter(allocator, grid, grid.lines, x, y)) |l| {
                std.debug.print("{c} {d}\n", .{ l, result_idx });
                result[result_idx] = l;
            }
            result_idx += 1;
        }
    }

    return result;
}

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

fn totalRunicPower(allocator: std.mem.Allocator, grid: util.Grid) !u64 {
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

pub fn answer3(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    var y: usize = 2;
    while (y < grid.height - 2) {
        var x: usize = 2;
        while (x < grid.width - 2) {
            // we know x and y are within a grid (should be '.')
            var i = util.index(grid, x, y);
            if (grid.lines[i] != '.') {
                std.debug.print("messed up!!! {d} {d} --------\n", .{ x, y });
                var it = std.mem.splitScalar(u8, grid.lines, '\n');
                while (it.next()) |line| {
                    std.debug.print("{s}\n", .{line});
                }
                return 0;
            }

            const letter = try solveLetter(allocator, grid, lines, x, y);
            if (letter) |ch| {
                grid.lines[util.index(grid, x, y)] = ch;
            }

            i += 1;
            while (lines[i] != '.' and lines[i] != '\n') : (i += 1) {}

            if (lines[i] == '\n') {
                x = 2;
                break;
            }
            x = i % (grid.width + 1);
        }
        var i = util.index(grid, x, y + 1);

        while (i < grid.lines.len and lines[i] != '.') : (i += (grid.width + 1)) {}
        if (i >= grid.lines.len) {
            break;
        }
        y = i / (grid.width + 1);
    }

    var solved_any = true;
    while (solved_any) {
        std.debug.print("solve loop\n", .{});
        solved_any = false;
        y = 2;
        while (y < grid.height - 2) : (y += 1) {
            var x: usize = 2;
            while (x < grid.width - 2) : (x += 1) {
                const i = util.index(grid, x, y);
                if (grid.lines[i] == '.') {
                    const result = try solveQuestionMark(allocator, grid, lines, x, y);
                    if (result) {
                        solved_any = true;
                    }

                    if (try solveLetter(allocator, grid, lines, x, y)) |l| {
                        grid.lines[i] = l;
                        solved_any = true;
                    }
                }
            }
        }
    }

    std.debug.print("--------\n", .{});
    var it = std.mem.splitScalar(u8, grid.lines, '\n');
    while (it.next()) |line| {
        std.debug.print("{s}\n", .{line});
    }

    var words = std.StringHashMap(bool).init(allocator);
    defer words.deinit();

    y = 2;
    while (y < grid.height - 2) : (y += 6) {
        var x: usize = 2;
        while (x < grid.width - 2) : (x += 6) {
            var word = try allocator.alloc(u8, 16);
            for (0..16) |i| {
                word[i] = '.';
            }
            var i: usize = 0;
            for (0..4) |dy| {
                for (0..4) |dx| {
                    word[i] = grid.lines[util.index(grid, x + dx, y + dy)];
                    i += 1;
                }
            }
            std.debug.print("word {s}\n", .{word});
            try words.put(word, true);
        }
    }

    var total: u64 = 0;
    var key_it = words.keyIterator();
    while (key_it.next()) |w| {
        total += wordPower(w.*);
        allocator.free(w.*);
    }

    return total;
}

fn solveWord(allocator: std.mem.Allocator, partially_filled_word: []u8, grid: util.Grid, x: usize, y: usize) !void {
    for (partially_filled_word, 0..) |c, i| {
        if (c != '.') {
            continue;
        }

        const x_offset = i % 4;
        const y_offset = i / 4;

        // need to find which letter only shows up once.
        var map = std.AutoHashMap(u8, u32).init(allocator);
        defer map.deinit();
        // walk from all directions.  should only see letters.

        for (util.cardinalDirections) |dir| {
            // duplicate of above sadly
            var it = util.DirectionIterator{
                .grid = grid,
                .direction = dir,
                .idx = util.index(grid, x + x_offset, y + y_offset),
            };
            // std.debug.print("({d}, {d}) {any} ", .{ x + x_offset, y + y_offset, dir });
            _ = it.next();
            while (it.next()) |ch| {
                var l = ch;
                if (l == '.') {
                    // must look up the partially filled word index
                    const it_x = it.idx % (grid.width + 1);
                    const it_y = it.idx / (grid.width + 1);

                    if (it_x < x or it_y < y or it_x - x >= 4 or it_y - y >= 4) {
                        break;
                    }
                    // std.debug.print("it_x - x {d} it_y - y {d}\n", .{ it_x - x, it_y - y });
                    const l_idx = (it_y - y) * 4 + (it_x - x);

                    if (l_idx == i) {
                        continue;
                    }
                    l = partially_filled_word[l_idx];
                }
                // std.debug.print("{c}", .{l});

                if (map.get(l)) |count| {
                    try map.put(l, count + 1);
                } else {
                    try map.put(l, 1);
                }
            }
            // std.debug.print("\n", .{});
        }

        var letter: ?u8 = null;
        var unsolvable = false;
        var key_it = map.keyIterator();
        while (key_it.next()) |ch| {
            if (map.get(ch.*) == 1 and ch.* != '?') {
                if (letter != null) {
                    unsolvable = true;
                }
                letter = ch.*;
            }
        }
        // std.debug.print("letter is {c}\n", .{letter.?});
        if (!unsolvable) {
            partially_filled_word[i] = letter.?;
        }
    }
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

    const word: []u8 = try runicWord(allocator, grid, 2, 2);
    defer std.testing.allocator.free(word);
    try solveWord(allocator, word, grid, 2, 2);
    try expectEqualStrings("LWGVXSHBPJQKNFZM", word);
}

test "full part 3" {
    const lines = "**XFZB**DCST**\n**LWQK**GQJH**\n?G....WL....DQ\nBS....H?....CN\nP?....KJ....TV\nNM....Z?....SG\n**NSHM**VKWZ**\n**PJGV**XFNL**\nWQ....?L....YS\nFX....DJ....HV\n?Y....WM....?J\nTJ....YK....LP\n**XRTK**BMSP**\n**DWZN**GCJV**\n";
    const allocator = std.testing.allocator;

    try expectEqual(3889, try answer3(allocator, lines));
}
