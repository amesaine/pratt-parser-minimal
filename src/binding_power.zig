left: u8,
right: u8,

pub fn prefix(op: u8) BindingPower {
    return switch (op) {
        '+', '-' => .{ .left = 0, .right = 9 },
        else => std.debug.panic("bad operator: {c}", .{op}),
    };
}

pub fn infix(op: u8) ?BindingPower {
    return switch (op) {
        '=' => .{ .left = 2, .right = 1 },
        '?' => .{ .left = 4, .right = 3 },
        '+', '-' => .{ .left = 5, .right = 6 },
        '*', '/' => .{ .left = 7, .right = 8 },
        '.' => .{ .left = 14, .right = 13 },
        else => null,
    };
}

pub fn postfix(op: u8) ?BindingPower {
    return switch (op) {
        '[', '!' => .{ .left = 11, .right = 0 },
        else => null,
    };
}

const std = @import("std");
const BindingPower = @This();
