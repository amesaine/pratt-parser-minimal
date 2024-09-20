// Atoms can only be a single character/digit
pub const Token = union(enum) {
    atom: u8,
    op: u8,
    eof,
};

source: []const u8,
idx: usize = 0,

pub fn next(l: *Lexer) Token {
    var result: Token = .eof;

    while (l.idx < l.source.len) {
        const ch = l.source[l.idx];
        switch (ch) {
            ' ',
            '\t',
            '\r',
            '\n',
            => l.idx += 1,

            '0'...'9',
            'a'...'z',
            'A'...'Z',
            => {
                result = Token{ .atom = ch };
                break;
            },

            else => {
                result = Token{ .op = ch };
                break;
            },
        }
    }

    l.idx += 1;
    return result;
}

const std = @import("std");
const assert = std.debug.assert;
const Lexer = @This();
