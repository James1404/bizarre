const std = @import("std");
const Allocator = std.mem.Allocator;

pub const TypeIndex = enum(usize) { _ };

pub const Type = union(enum) {
    int: struct {
        size: enum(u8) { @"8", @"16", @"32", @"64", ptrsize },
        signed: bool,
    },
    float: struct {
        size: enum { @"32", @"64" },
    },
    string,
    bool,
    type,
    any,
    @"fn": struct {
        args: Type,
        ret: Type,
    },
};

allocator: Allocator,
list: std.ArrayList(Type),
