const Sexpr = struct {
    tag: Tag,
    ch: u8,
    operands_start: Parser.Index,

    comptime {
        assert(@sizeOf(Sexpr) == 8);
    }

    const Tag = enum {
        atom,
        unary,
        binary,
        ternary,
    };
};

const Parser = struct {
    allocator: Allocator,
    tokens: []const Lexer.Token,
    tok_i: usize,
    sexprs: MultiDynamicArray(Sexpr),
    operands: DynamicArray(Index),

    const Index = u32;

    fn parse(p: *Parser) !void {
        _ = try p.parse_bp(0);
    }

    fn parse_bp(p: *Parser, bp_min: u8) !Index {
        const root_i = try p.sexprs_reserve();
        var root = switch (p.next_tk()) {
            .atom => |ch| Sexpr{ .ch = ch, .tag = .atom, .operands_start = undefined },
            .op => |operator| root: {
                if (operator == '(') {
                    p.sexprs.len -= 1;
                    const temp = try p.parse_bp(0);
                    assert(p.next_tk().op == ')');
                    break :root p.sexprs.get(temp);
                } else {
                    const bp = BindingPower.prefix(operator);
                    const start_i = try p.parse_bp(bp.right);
                    const start_i_i = try p.operands_append(start_i);
                    break :root Sexpr{ .tag = .unary, .ch = operator, .operands_start = start_i_i };
                }
            },
            else => std.debug.panic("Expected atom. Got {?}\n", .{p.tokens[p.tok_i]}),
        };
        while (p.peek_tk() != .eof) {
            const operator = p.peek_tk().op;
            if (BindingPower.postfix(operator)) |bp| {
                if (bp.left < bp_min) break;
                _ = p.next_tk();

                const start_i = try p.sexprs_append(root);
                var tag = Sexpr.Tag.unary;
                var last_i_maybe: ?Index = null;
                if (operator == '[') {
                    tag = .binary;
                    last_i_maybe = try p.parse_bp(bp.right);
                    assert(p.next_tk().op == ']');
                }

                const start_i_i = try p.operands_append(start_i);
                if (last_i_maybe) |last_i| _ = try p.operands_append(last_i);

                root = Sexpr{ .tag = tag, .ch = operator, .operands_start = start_i_i };
                continue;
            } else if (BindingPower.infix(operator)) |bp| {
                if (bp.left < bp_min) break;
                _ = p.next_tk();

                const start_i = try p.sexprs_append(root);
                var tag: Sexpr.Tag = .binary;
                var second_i_maybe: ?Index = null;
                if (operator == '?') {
                    tag = .ternary;
                    second_i_maybe = try p.parse_bp(0);
                    assert(p.next_tk().op == ':');
                }
                const last_i = try p.parse_bp(bp.right);

                const start_i_i = try p.operands_append(start_i);
                if (second_i_maybe) |second_i| _ = try p.operands_append(second_i);
                _ = try p.operands_append(last_i);

                root = Sexpr{ .tag = tag, .ch = operator, .operands_start = start_i_i };
                continue;
            }
            break;
        }
        p.sexprs.set(root_i, root);
        return root_i;
    }

    fn peek_tk(p: Parser) Lexer.Token {
        return p.tokens[p.tok_i];
    }

    fn next_tk(p: *Parser) Lexer.Token {
        p.tok_i += 1;
        return p.tokens[p.tok_i - 1];
    }

    fn sexprs_append(p: *Parser, s: Sexpr) !Index {
        try p.sexprs.append(p.allocator, s);
        return @intCast(p.sexprs.len - 1);
    }

    fn sexprs_reserve(p: *Parser) !Index {
        try p.sexprs.resize(p.allocator, p.sexprs.len + 1);
        return @intCast(p.sexprs.len - 1);
    }

    fn operands_append(p: *Parser, i: Index) !Index {
        try p.operands.append(p.allocator, i);
        return @intCast(p.operands.items.len - 1);
    }
};

pub const Ast = struct {
    sexprs: MultiDynamicArray(Sexpr).Slice,
    operands: []const Parser.Index,

    pub fn init(allocator: Allocator, input: []const u8) !Ast {
        var lexer = Lexer{ .source = input };
        var tokens = DynamicArray(Lexer.Token){};
        while (true) {
            const token = lexer.next();
            try tokens.append(allocator, token);
            if (token == .eof) break;
        }
        var parser = Parser{
            .allocator = allocator,
            .tokens = try tokens.toOwnedSlice(allocator),
            .tok_i = 0,
            .sexprs = .{},
            .operands = .{},
        };

        try parser.parse();

        return Ast{
            .sexprs = parser.sexprs.toOwnedSlice(),
            .operands = try parser.operands.toOwnedSlice(allocator),
        };
    }

    // We don't need to implement this because we reset the arena after parsing.
    fn deinit() void {}

    pub fn stringify(self: Ast, allocator: Allocator) ![]const u8 {
        var str = DynamicArray(u8){};
        errdefer str.deinit(allocator);
        try self.stringify_index(str.writer(allocator).any(), 0);
        return str.toOwnedSlice(allocator);
    }

    fn stringify_index(a: Ast, writer: std.io.AnyWriter, index: usize) !void {
        const root = a.sexprs.get(index);
        const operands = @intFromEnum(root.tag);
        if (operands == 0) {
            try writer.writeByte(root.ch);
        } else {
            try writer.writeByte('(');
            try writer.writeByte(root.ch);
            for (0..operands) |i| {
                const operand_i = a.operands[root.operands_start + i];
                assert(operand_i != index);
                try writer.writeByte(' ');
                try a.stringify_index(writer, operand_i);
            }
            try writer.writeByte(')');
        }
    }
};

fn test_ast_stringify(arena: Allocator, input: []const u8, expected: []const u8) !void {
    const ast = try Ast.init(arena, input);
    const actual = try ast.stringify(arena);

    try std.testing.expectEqualStrings(expected, actual);
}

test "expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    try test_ast_stringify(
        allocator,
        "1",
        "1",
    );

    try test_ast_stringify(
        allocator,
        "1 + 2 * 3",
        "(+ 1 (* 2 3))",
    );

    try test_ast_stringify(
        allocator,
        "a + b * c * d + e",
        "(+ (+ a (* (* b c) d)) e)",
    );

    try test_ast_stringify(
        allocator,
        "f . g . h",
        "(. f (. g h))",
    );

    try test_ast_stringify(
        allocator,
        " 1 + 2 + f . g . h * 3 * 4",
        "(+ (+ 1 2) (* (* (. f (. g h)) 3) 4))",
    );

    try test_ast_stringify(
        allocator,
        "--1 * 2",
        "(* (- (- 1)) 2)",
    );

    try test_ast_stringify(
        allocator,
        "--f . g",
        "(- (- (. f g)))",
    );

    try test_ast_stringify(
        allocator,
        "-9!",
        "(- (! 9))",
    );

    try test_ast_stringify(
        allocator,
        "f . g !",
        "(! (. f g))",
    );

    try test_ast_stringify(
        allocator,
        "(((0)))",
        "0",
    );

    try test_ast_stringify(
        allocator,
        "x[0][1]",
        "([ ([ x 0) 1)",
    );

    try test_ast_stringify(
        allocator,
        "a ? b : c ? d : e",
        "(? a b (? c d e))",
    );

    try test_ast_stringify(
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
const MultiDynamicArray = std.MultiArrayList;
const Lexer = @import("lexer.zig");
const BindingPower = @import("binding_power.zig");
