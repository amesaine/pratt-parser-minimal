const std = @import("std");
const assert = std.debug.assert;

const Token = union(Tag) {
    atom: u8,
    op: u8,
    eof,

    const Tag = enum {
        atom,
        op,
        eof,
    };
};

// Only handles single digit/characters as atomics
const Lexer = struct {
    tokens: std.ArrayList(Token),
    index: i32,

    fn init(allocator: std.mem.Allocator, input: []const u8) !Lexer {
        var tokens = std.ArrayList(Token).init(allocator);
        for (input) |ch| {
            const token = switch (ch) {
                ' ', '\t', '\n', '\r' => continue,
                '0'...'9' => Token{ .atom = ch },
                else => Token{ .op = ch },
            };

            try tokens.append(token);
        }

        return Lexer{ .tokens = tokens, .index = -1 };
    }

    fn next(self: *Lexer) Token {
        if (self.index < self.tokens.items.len) {
            self.index += 1;
            assert(self.index >= 0);
            const token = self.tokens.items[@intCast(self.index)];
            return token;
        } else {
            return Token.eof;
        }
    }

    fn peek(self: *Lexer) Token {
        const i = self.index + 1;
        assert(i >= 0);
        if (i < self.tokens.items.len) {
            return self.tokens.items[@intCast(i)];
        } else {
            return Token.eof;
        }
    }
};

const Sexpr = union(enum) {
    atom: u8,
    cons: struct {
        op: u8,
        left: *const Sexpr,
        right: *const Sexpr,
    },

    fn stringifyAlloc(self: *const Sexpr, allocator: std.mem.Allocator) ![]u8 {
        var list = std.ArrayList(u8).init(allocator);
        errdefer list.deinit();

        switch (self.*) {
            .atom => |byte| {
                try list.append(byte);
            },
            .cons => |cons| {
                try list.append('(');
                try list.appendSlice(try cons.left.stringifyAlloc(allocator));
                try list.append(cons.op);
                try list.appendSlice(try cons.right.stringifyAlloc(allocator));
                try list.append(')');
            },
        }

        return list.toOwnedSlice();
    }
};

fn expr(allocator: std.mem.Allocator, input: []const u8) !*Sexpr {
    var lexer = try Lexer.init(allocator, input);
    return exprBindingPower(allocator, &lexer, 0);
}

fn exprBindingPower(allocator: std.mem.Allocator, lexer_ptr: *Lexer, binding_power_min: u8) *Sexpr {
    var left_ptr = allocator.create(Sexpr) catch unreachable;
    left_ptr.* = switch (lexer_ptr.next()) {
        .atom => |ch| Sexpr{ .atom = ch },
        else => @panic("Expecting Atom"),
    };

    while (true) {
        const op = switch (lexer_ptr.peek()) {
            .eof => break,
            .op => |ch| ch,
            else => @panic("expecting EOF or Operator, got Atom"),
        };

        const binding_power = BindingPower.infix(op);
        if (binding_power.left < binding_power_min) {
            break;
        }

        _ = lexer_ptr.next(); // eat op
        const right_ptr = exprBindingPower(allocator, lexer_ptr, binding_power.right);

        const cons_ptr = allocator.create(Sexpr) catch unreachable;
        cons_ptr.* = Sexpr{ .cons = .{
            .op = op,
            .left = left_ptr,
            .right = right_ptr,
        } };
        left_ptr = cons_ptr;
    }

    return left_ptr;
}

const BindingPower = struct {
    left: u8,
    right: u8,

    fn infix(op: u8) BindingPower {
        return switch (op) {
            '+', '-' => .{ .left = 1, .right = 2 },
            '*', '/' => .{ .left = 3, .right = 4 },
            '.' => .{ .left = 6, .right = 5 },
            else => @panic("bad operator"),
        };
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    const s = try expr(allocator, "1 + 2");
    const str = try s.stringifyAlloc(allocator);
    std.debug.print("{s}", .{str});
}

fn testExprStringify(input: []const u8, expected: []const u8) !void {
    const allocator = std.testing.allocator;
    const s = try expr(allocator, input);

    const actual = try s.stringifyAlloc(allocator);
    try std.testing.expectEqual(expected, actual);
}

test "expressions" {
    try testExprStringify("1", "1");
    try testExprStringify("1 + 2", "(1+2)");
    try testExprStringify("1 + 2 * 3 * 4 - 5", "((1+((2*3)*4))-5");
}
