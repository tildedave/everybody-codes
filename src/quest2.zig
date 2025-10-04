const std = @import("std");
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

const Direction = enum { up, down, left, right };

const Grid = struct {
    lines: []const u8,
    width: usize,
    height: usize,
};

// infinite iterator
const DirectionIterator = struct {
    grid: Grid,

    direction: Direction,
    x: usize,
    y: usize,

    idx: usize = 0,
    is_first: bool = true,

    fn walk(self: *DirectionIterator) bool {
        switch (self.direction) {
            .up => {
                if (self.y == 0) {
                    return false;
                } else {
                    self.y = self.y - 1;
                }
            },
            .down => {
                if (self.y == self.grid.height - 1) {
                    return false;
                }
                self.y = (self.y + 1);
            },
            .left => {
                if (self.x == 0) {
                    self.x = self.grid.width - 1;
                } else {
                    self.x = self.x - 1;
                }
            },
            .right => self.x = (self.x + 1) % self.grid.width,
        }

        return true;
    }

    fn next(self: *DirectionIterator) ?u8 {
        if (self.is_first) {
            self.is_first = false;
        } else if (!self.walk()) {
            return null;
        }
        self.idx = self.y * (self.grid.width + 1) + self.x;
        return self.grid.lines[self.idx];
    }
};

pub fn createGrid(lines: []const u8) Grid {
    return Grid{
        .lines = lines,
        .width = std.mem.indexOfScalar(u8, lines, '\n').?,
        .height = std.mem.count(u8, lines, &[_]u8{'\n'}),
    };
}

test "DirectionIterator" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = createGrid(lines);
    var it = DirectionIterator{ .direction = .right, .x = 0, .y = 0, .grid = grid };
    try expectEqual('H', it.next());
    try expectEqual('E', it.next());
    try expectEqual('L', it.next());
    try expectEqual('W', it.next());
    try expectEqual('O', it.next());
    try expectEqual('R', it.next());
    try expectEqual('L', it.next());
    try expectEqual('T', it.next());
    try expectEqual('H', it.next());
    it = DirectionIterator{ .direction = .down, .x = 0, .y = 0, .grid = grid };
    try expectEqual('H', it.next());
    try expectEqual('E', it.next());
    try expectEqual('T', it.next());
    try expectEqual(null, it.next());
    it = DirectionIterator{ .direction = .left, .x = 0, .y = 0, .grid = grid };
    try expectEqual('H', it.next());
    try expectEqual('T', it.next());
    try expectEqual('L', it.next());
    it = DirectionIterator{ .direction = .up, .x = 0, .y = 0, .grid = grid };
    try expectEqual('H', it.next());
    try expectEqual(null, it.next());
}

pub fn findWord(word: []const u8, grid: Grid, bset: *std.bit_set.IntegerBitSet(32_768)) void {
    for (0..grid.width) |x| {
        for (0..grid.height) |y| {
            // std.debug.print("x={d} y={d}\n", .{ x, y });
            for ([_]Direction{ .up, .down, .left, .right }) |dir| {
                var it = DirectionIterator{ .direction = dir, .x = x, .y = y, .grid = grid };
                var k: usize = 0;
                while (k < word.len and it.next() == word[k]) {
                    k += 1;
                }
                if (k == word.len) {
                    k = 0;
                    it = DirectionIterator{ .direction = dir, .x = x, .y = y, .grid = grid };
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
