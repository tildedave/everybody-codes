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
