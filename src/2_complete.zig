const Sexpr = union(enum) {
    atom: u8,
    cons: struct {
        op: u8,
        left: ?*Sexpr,
        middle: ?*Sexpr,
        right: ?*Sexpr,
    },

    comptime {
        assert(@sizeOf(Sexpr) == 40 or @sizeOf(20));
    }

    fn stringify(self: Sexpr, writer: std.io.AnyWriter) !void {
        switch (self) {
            .atom => |byte| try writer.writeByte(byte),
            .cons => |cons| {
                try writer.writeByte('(');
                try writer.writeByte(cons.op);
                if (self.cons.left) |left| {
                    try writer.writeByte(' ');
                    try left.stringify(writer);
                }
                if (self.cons.middle) |middle| {
                    try writer.writeByte(' ');
                    try middle.stringify(writer);
                }
                if (self.cons.right) |right| {
                    try writer.writeByte(' ');
                    try right.stringify(writer);
                }
                try writer.writeByte(')');
            },
        }
    }
};

const Parser = struct {
    allocator: Allocator,
    root: *Sexpr,
    tokens: []const Lexer.Token,
    tok_i: usize,

    fn parse(p: *Parser) !void {
        p.root = try p.parse_bp(0);
    }

    fn parse_bp(p: *Parser, bp_min: u8) !*Sexpr {
        assert(p.tok_i < p.tokens.len);

        var root_ptr = try p.allocator.create(Sexpr);
        errdefer p.allocator.destroy(root_ptr);

        const first_tk = p.tokens[p.tok_i];
        p.tok_i += 1;

        root_ptr.* = switch (first_tk) {
            .atom => |ch| Sexpr{ .atom = ch },
            .op => |op| root: {
                if (op == '(') {
                    const temp = try p.parse_bp(0);
                    assert(p.tokens[p.tok_i] == .op);
                    assert(p.tokens[p.tok_i].op == ')');
                    p.tok_i += 1;
                    break :root temp.*;
                } else {
                    const bp = BindingPower.prefix(op);
                    const right_ptr = try p.parse_bp(bp.right);

                    break :root Sexpr{ .cons = .{
                        .op = op,
                        .left = null,
                        .middle = null,
                        .right = right_ptr,
                    } };
                }
            },
            else => std.debug.panic("Expected atom. Got {?}\n", .{p.tok_i}),
        };

        while (true) {
            // The token list is .eof terminated.
            assert(p.tok_i < p.tokens.len);
            const op = switch (p.tokens[p.tok_i]) {
                .eof => break,
                .op => |op| op,
                .atom => |atom| std.debug.panic("Expected op. Got {?}\n", .{atom}),
            };

            if (BindingPower.postfix(op)) |bp| {
                if (bp.left < bp_min) break;
                p.tok_i += 1; // eat op

                var right_ptr: ?*Sexpr = null;
                if (op == '[') {
                    right_ptr = try p.parse_bp(bp.right);
                    assert(p.tokens[p.tok_i].op == ']');
                    p.tok_i += 1;
                }
                const cons_ptr = try p.allocator.create(Sexpr);
                cons_ptr.* = .{ .cons = .{
                    .op = op,
                    .left = root_ptr,
                    .middle = null,
                    .right = right_ptr,
                } };

                root_ptr = cons_ptr;
                continue;
            }

            if (BindingPower.infix(op)) |bp| {
                if (bp.left < bp_min) break;
                p.tok_i += 1; // eat op

                var middle_ptr: ?*Sexpr = null;
                if (op == '?') {
                    middle_ptr = try p.parse_bp(bp.right);
                    assert(p.tokens[p.tok_i].op == ':');
                    p.tok_i += 1;
                }

                const right_ptr = try p.parse_bp(bp.right);

                const cons_ptr = try p.allocator.create(Sexpr);
                cons_ptr.* = Sexpr{
                    .cons = .{
                        .op = op,
                        .left = root_ptr,
                        .middle = middle_ptr,
                        .right = right_ptr,
                    },
                };
                root_ptr = cons_ptr;
                continue;
            }
            break;
        }

        return root_ptr;
    }
};

pub const Ast = struct {
    root: *Sexpr,

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
        "-1",
        "(- 1)",
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
