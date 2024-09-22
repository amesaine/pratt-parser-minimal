const Sexpr = struct {
    ch: u8,
    operands: ?[]const Sexpr,

    comptime {
        assert(@sizeOf(Sexpr) == 24 or @sizeOf(Sexpr) == 12);
    }

    fn stringify(s: Sexpr, writer: std.io.AnyWriter) !void {
        if (s.operands) |operands| {
            _ = try writer.writeByte('(');
            _ = try writer.writeByte(s.ch);
            for (operands) |o| {
                try writer.writeByte(' ');
                try o.stringify(writer);
            }
            try writer.writeByte(')');
        } else try writer.writeByte(s.ch);
    }
};

const Parser = struct {
    allocator: Allocator,
    root: Sexpr,
    tokens: []const Lexer.Token,
    tok_i: usize,

    fn parse(p: *Parser) !void {
        p.root = try p.parse_bp(0);
    }

    fn parse_bp(p: *Parser, bp_min: u8) !Sexpr {
        var root = switch (p.next_tok()) {
            .atom => |ch| Sexpr{ .ch = ch, .operands = null },
            .op => |op| root: {
                if (op == '(') {
                    const temp = try p.parse_bp(0);
                    assert(p.next_tok().op == ')');
                    break :root temp;
                } else {
                    const bp = BindingPower.prefix(op);
                    const right = try p.parse_bp(bp.right);
                    var operands = try p.allocator.alloc(Sexpr, 1);
                    operands[0] = right;

                    break :root Sexpr{
                        .ch = op,
                        .operands = operands,
                    };
                }
            },
            else => std.debug.panic("Expected atom. Got {?}\n", .{p.tok_i}),
        };
        while (p.peek_tok() != .eof) {
            const operator = p.peek_tok().op;
            if (BindingPower.postfix(operator)) |bp| {
                if (bp.left < bp_min) break;
                _ = p.next_tok();
                var operands = try DynamicArray(Sexpr).initCapacity(p.allocator, 3);
                try operands.append(p.allocator, root);
                if (operator == '[') {
                    try operands.append(p.allocator, try p.parse_bp(bp.right));
                    assert(p.next_tok().op == ']');
                }
                root = .{ .ch = operator, .operands = try operands.toOwnedSlice(p.allocator) };
                continue;
            } else if (BindingPower.infix(operator)) |bp| {
                if (bp.left < bp_min) break;
                _ = p.next_tok();
                var operands = try DynamicArray(Sexpr).initCapacity(p.allocator, 3);
                try operands.append(p.allocator, root);
                if (operator == '?') {
                    try operands.append(p.allocator, try p.parse_bp(bp.right));
                    assert(p.next_tok().op == ':');
                }
                try operands.append(p.allocator, try p.parse_bp(bp.right));
                root = .{ .ch = operator, .operands = try operands.toOwnedSlice(p.allocator) };
                continue;
            }
            break;
        }
        return root;
    }

    fn peek_tok(p: Parser) Lexer.Token {
        return p.tokens[p.tok_i];
    }

    fn next_tok(p: *Parser) Lexer.Token {
        p.tok_i += 1;
        return p.tokens[p.tok_i - 1];
    }
};

pub const Ast = struct {
    root: Sexpr,

    pub fn init(allocator: Allocator, input: []const u8) !Ast {
        var lexer = Lexer{ .source = input };
        var tokens = DynamicArray(Lexer.Token){};
        while (true) {
            const token = lexer.next();
            try tokens.append(allocator, token);
            switch (token) {
                .eof => break,
                else => {},
            }
        }
        var parser = Parser{
            .allocator = allocator,
            .tokens = try tokens.toOwnedSlice(allocator),
            .tok_i = 0,
            .root = undefined,
        };

        try parser.parse();

        return .{ .root = parser.root };
    }

    // We don't need to implement this because we reset the arena after parsing.
    fn deinit() void {}

    pub fn stringify(self: Ast, allocator: Allocator) ![]const u8 {
        var str = DynamicArray(u8){};
        errdefer str.deinit(allocator);
        try self.root.stringify(str.writer(allocator).any());
        return str.toOwnedSlice(allocator);
    }
};

fn testExprStringify(arena: Allocator, input: []const u8, expected: []const u8) !void {
    const ast = try Ast.init(arena, input);
    const actual = try ast.stringify(arena);

    try std.testing.expectEqualStrings(expected, actual);
}

test "expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    try testExprStringify(
        allocator,
        "1",
        "1",
    );

    try testExprStringify(
        allocator,
        "1 + 2 * 3",
        "(+ 1 (* 2 3))",
    );

    try testExprStringify(
        allocator,
        "a + b * c * d + e",
        "(+ (+ a (* (* b c) d)) e)",
    );

    try testExprStringify(
        allocator,
        "f . g . h",
        "(. f (. g h))",
    );

    try testExprStringify(
        allocator,
        " 1 + 2 + f . g . h * 3 * 4",
        "(+ (+ 1 2) (* (* (. f (. g h)) 3) 4))",
    );

    try testExprStringify(
        allocator,
        "--1 * 2",
        "(* (- (- 1)) 2)",
    );

    try testExprStringify(
        allocator,
        "--f . g",
        "(- (- (. f g)))",
    );

    try testExprStringify(
        allocator,
        "-9!",
        "(- (! 9))",
    );

    try testExprStringify(
        allocator,
        "f . g !",
        "(! (. f g))",
    );

    try testExprStringify(
        allocator,
        "(((0)))",
        "0",
    );

    try testExprStringify(
        allocator,
        "x[0][1]",
        "([ ([ x 0) 1)",
    );

    try testExprStringify(
        allocator,
        "a ? b : c ? d : e",
        "(? a b (? c d e))",
    );

    try testExprStringify(
        allocator,
        "a = 0 ? b : c = d",
        "(= a (= (? 0 b c) d))",
    );
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
// ArrayList is planned to be renamed as DynamicArray and Unmanaged as the default.
const DynamicArray = std.ArrayListUnmanaged;
const Lexer = @import("lexer.zig");
const BindingPower = @import("binding_power.zig");
