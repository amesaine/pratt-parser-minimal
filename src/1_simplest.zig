const Node = union(enum) {
    atom: u8,
    cons: struct {
        op: u8,
        left: *Node,
        right: *Node,
    },

    fn stringify(s: Node, writer: std.io.AnyWriter) !void {
        switch (s) {
            .atom => |byte| try writer.writeByte(byte),
            .cons => |cons| {
                try writer.writeByte('(');
                try writer.writeByte(cons.op);
                try writer.writeByte(' ');
                try cons.left.stringify(writer);
                try writer.writeByte(' ');
                try cons.right.stringify(writer);
                try writer.writeByte(')');
            },
        }
    }
};

const BindingPower = struct {
    left: u8,
    right: u8,

    fn infix(op: u8) BindingPower {
        return switch (op) {
            '+', '-' => .{ .left = 1, .right = 2 },
            '*', '/' => .{ .left = 3, .right = 4 },
            '.' => .{ .left = 6, .right = 5 },
            else => std.debug.panic("Bad operator. Got {?}\n", .{op}),
        };
    }
};

const Parser = struct {
    allocator: Allocator,
    root: *Node,
    tokens: []const Lexer.Token,
    tok_i: usize,

    fn parse(p: *Parser) !void {
        p.root = try p.parse_bp(0);
    }

    fn parse_bp(p: *Parser, bp_min: u8) !*Node {
        const cur_tok = p.tokens[p.tok_i];
        p.tok_i += 1;

        var root_ptr = try p.allocator.create(Node);
        errdefer p.allocator.destroy(root_ptr);

        root_ptr.* = switch (cur_tok) {
            .atom => |ch| Node{ .atom = ch },
            else => std.debug.panic("Expected atom. Got {?}\n", .{cur_tok}),
        };

        while (p.tok_i < p.tokens.len) {
            const op = switch (p.tokens[p.tok_i]) {
                .op => |op| op,
                .eof => {
                    assert(p.tok_i == p.tokens.len - 1);
                    break;
                },
                .atom => |atom| std.debug.panic("Expected op. Got {?}\n", .{atom}),
            };

            const bp = BindingPower.infix(op);
            if (bp.left < bp_min) break;

            p.tok_i += 1;

            const right_ptr = try p.parse_bp(bp.right);

            const cons_ptr = try p.allocator.create(Node);
            cons_ptr.* = Node{
                .cons = .{
                    .op = op,
                    .left = root_ptr,
                    .right = right_ptr,
                },
            };
            root_ptr = cons_ptr;
        }

        return root_ptr;
    }
};

pub const Ast = struct {
    root: *Node,

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

    fn deinit() void {}

    pub fn stringify(a: Ast, allocator: Allocator) ![]const u8 {
        var str = DynamicArray(u8){};
        errdefer str.deinit(allocator);
        try a.root.stringify(str.writer(allocator).any());
        return try str.toOwnedSlice(allocator);
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
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
// ArrayList is planned to be renamed as DynamicArray and Unmanaged as the default.
const DynamicArray = std.ArrayListUnmanaged;

const Lexer = @import("lexer.zig");
