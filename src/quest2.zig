const std = @import("std");
const util = @import("util.zig");
const Grid = util.Grid;
const createGrid = util.createGrid;
const Direction = util.Direction;
const DirectionIterator = util.DirectionIterator;
const expectEqual = std.testing.expectEqual;

pub fn answer1(words: []const u8, line: []const u8) u64 {
    var result: u64 = 0;
    var it = std.mem.splitScalar(u8, words, ',');
    while (it.next()) |word| {
        result += std.mem.count(u8, line, word);
    }
    return result;
}

test "answer1 expected" {
    try expectEqual(answer1("THE,OWE,MES,ROD,HER", "AWAKEN THE POWER ADORNED WITH THE FLAMES BRIGHT IRE"), 4);
}

pub fn runicSymbols(words: []const u8, line: []const u8) u64 {
    // we'll say this is the max line we'll be asked to process, perhaps wrong
    var bset = std.bit_set.IntegerBitSet(1_024).initEmpty();
    var it = std.mem.splitScalar(u8, words, ',');

    while (it.next()) |word| {
        var i: usize = 0;
        while (i < line.len) {
            if (i <= line.len - word.len) {
                if (std.mem.eql(u8, line[i .. i + word.len], word)) {
                    var k: usize = i;
                    while (k < i + word.len) {
                        bset.setValue(k, true);
                        k += 1;
                    }
                }
            }
            if (i >= word.len - 1) {
                var k: usize = 0;
                while (k < word.len and line[i - k] == word[k]) {
                    k += 1;
                }
                // match, so we count back down and mark all the symbols as good
                if (k == word.len) {
                    k -= 1;
                    while (k > 0) {
                        bset.setValue(i - k, true);
                        k -= 1;
                    }
                    bset.setValue(i, true);
                }
            }

            i = i + 1;
        }
    }

    return bset.count();
}

pub fn answer2(words: []const u8, split_lines: std.mem.SplitIterator(u8, .scalar)) u64 {
    var result: u64 = 0;
    var foo = split_lines;
    while (foo.next()) |line| {
        result += runicSymbols(words, line);
    }

    return result;
}

test "answer2 expected" {
    const words = "THE,OWE,MES,ROD,HER,QAQ";
    try expectEqual(15, runicSymbols(words, "AWAKEN THE POWE ADORNED WITH THE FLAMES BRIGHT IRE"));
    try expectEqual(9, runicSymbols(words, "THE FLAME SHIELDED THE HEART OF THE KINGS"));
    try expectEqual(6, runicSymbols(words, "POWE PO WER P OWE R"));
    try expectEqual(7, runicSymbols(words, "THERE IS THE END"));
    try expectEqual(5, runicSymbols(words, "QAQAQ"));
    try expectEqual(3, runicSymbols(words, "THE"));
    try expectEqual(3, runicSymbols(words, "EHT"));
}

pub fn findWord(word: []const u8, grid: Grid, bset: *std.bit_set.IntegerBitSet(32_768)) void {
    for (0..grid.lines.len) |idx| {
        if (grid.lines[idx] == '\n') {
            continue;
        }

        for ([_]Direction{ .up, .down, .left, .right }) |dir| {
            var it = DirectionIterator{ .direction = dir, .idx = idx, .grid = grid, .walk_opts = .{ .wraparound_horizontal = true } };
            var k: usize = 0;
            while (k < word.len and it.next() == word[k]) {
                k += 1;
            }
            if (k == word.len) {
                k = 0;
                it = DirectionIterator{ .direction = dir, .idx = idx, .grid = grid, .walk_opts = .{ .wraparound_horizontal = true } };
                while (k < word.len and it.next() == word[k]) {
                    bset.setValue(it.idx, true);
                    k += 1;
                }
            } else {
                // std.debug.print("no match at x={d} y={d} k={d}\n", .{ x, y, k });
            }
        }
    }
}

test "findWord" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = createGrid(lines);
    var bset = std.bit_set.IntegerBitSet(32_768).initEmpty();
    findWord("THE", grid, &bset);
    try expectEqual(3, bset.count());

    bset = std.bit_set.IntegerBitSet(32_768).initEmpty();
    findWord("RODEO", grid, &bset);
    try expectEqual(5, bset.count());

    bset = std.bit_set.IntegerBitSet(32_768).initEmpty();
    findWord("ROD", grid, &bset);
    try expectEqual(3, bset.count());
}

pub fn answer3(words: []const u8, lines: []const u8) u64 {
    var word_it = std.mem.splitScalar(u8, words, ',');
    var bset = std.bit_set.IntegerBitSet(32_768).initEmpty();
    const grid = createGrid(lines);

    while (word_it.next()) |word| {
        findWord(word, grid, &bset);
    }
    return bset.count();
}

test "answer3" {
    const words = "THE,OWE,MES,ROD,RODEO";
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";

    try expectEqual(10, answer3(words, lines));
}
