const std = @import("std");
const bytecode = @import("bytecode.zig");

allocator: std.mem.Allocator,

src: []bytecode.Inst,
loc: usize,
stack: std.ArrayList(bytecode.Value),

const Self = @This();

pub fn make(
    allocator: std.mem.Allocator,
    src: []bytecode.Inst,
) Self {
    return Self{
        .allocator = allocator,
        .src = src,
        .loc = 0,
        .stack = .init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.stack.deinit();
}
