pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    var start: usize = 0;
    for (mock, 0..) |ch, i| {
        if (ch == '\n') {
            const input = mock[start..i];
            const ast = try Ast.init(allocator, input);
            _ = ast;
            start = i + 1;
        }
    }
}

const std = @import("std");
const mock = @import("mock.zig").mock;
// const Ast = @import("4_dod.zig").Ast;
// const Ast = @import("3_complete_slice.zig").Ast;
const Ast = @import("2_complete.zig").Ast;
