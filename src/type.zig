const std = @import("std");
const AST = @import("ast.zig");

pub const Ref = *Type;
pub const Type = union(enum) {
    Unknown,

    UnknownInt,
    UnknownFloat,

    Int: struct { size: i32, signed: bool },
    Float: struct { size: i32 },
    String,
    Bool,

    Array: struct { inner: Ref, size: usize },
    Fn: struct {
        args: []Ref,
        ret: Ref,
    },

    Pointer: struct { inner: Ref },
};

allocator: std.mem.Allocator,
root: ?Ref,

const Self = @This();

fn freeType(self: *Self, ty: Ref) void {
    switch (ty.*) {
        .Array => |v| self.freeType(v.inner),
        .Fn => |v| {
            self.freeType(v.ret);
        },

        .Pointer => |v| self.freeType(v.inner),

        .Int,
        .Float,
        .String,
        .Bool,
        => {},
    }

    self.allocator.destroy(ty);
}

pub fn deinit(self: *Self) void {
    if (self.root) |root| {
        self.freeType(root);
    }
}
