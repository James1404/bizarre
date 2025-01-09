const std = @import("std");
const Allocator = std.mem.Allocator;
const BlockList = @import("blocklist.zig");

pub const Value = union(enum) {
    pub const Data = union(enum) {
        int: []const u8,
        float: []const u8,
        string: []const u8,
        bool: bool,
        ident: []const u8,
        @"fn": BlockList.Location,
        builtin: Builtin,
    };

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
    };

    untyped: Data,
    typed: struct { data: Data, type: Type },

    pub fn make_untyped(data: Data) @This() {
        return @This(){ .untyped = data };
    }
};

// Reference a value
pub const Ref = usize;

// Builtin values and types
pub const Builtin = enum(u32) {
    type_u8,
    type_u16,
    type_u32,
    type_u64,
    type_usize,

    type_i8,
    type_i16,
    type_i32,
    type_i64,
    type_isize,

    type_f32,
    type_f64,

    type_bool,
    type_string,

    type_any,
    type_type,

    bool_true,
    bool_false,
};

pub const Inst = union(enum) {
    add: struct { addr: Ref, lhs: Ref, rhs: Ref },
    sub: struct { addr: Ref, lhs: Ref, rhs: Ref },
    div: struct { addr: Ref, lhs: Ref, rhs: Ref },
    mul: struct { addr: Ref, lhs: Ref, rhs: Ref },

    cmp_lt: struct { addr: Ref, lhs: Ref, rhs: Ref },
    cmp_gt: struct { addr: Ref, lhs: Ref, rhs: Ref },
    cmp_le: struct { addr: Ref, lhs: Ref, rhs: Ref },
    cmp_ge: struct { addr: Ref, lhs: Ref, rhs: Ref },
    cmp_eq: struct { addr: Ref, lhs: Ref, rhs: Ref },
    cmp_ne: struct { addr: Ref, lhs: Ref, rhs: Ref },

    negate: struct { addr: Ref, value: Ref },

    cast: struct { addr: Ref, value: Ref, type: Ref },

    assign: struct { addr: Ref, value: Ref },

    push_param: Ref,
    pop_param: struct { addr: Ref },

    call: BlockList.Location,

    begin_function,
    end_function,
    @"return": Ref,

    goto: BlockList.Location,
    goto_if: struct { value: Ref, to: BlockList.Location },

    value: struct { addr: Ref, value: Value },
    builtin: Builtin,
};

pub const Fn = struct {
    const Param = struct {
        ty: Ref,
        name: []const u8,
    };

    params: std.ArrayList(Param),
    body: BlockList.Location,
};
