const std = @import("std");
const Allocator = std.mem.Allocator;

allocator: Allocator,
functions: std.ArrayList(Fn),

const Self = @This();

pub const Ref = enum(usize) {
    _,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("Ref.%{d}", .{@intFromEnum(self)});
    }
};

pub const Chunk = struct {
    instructions: std.ArrayList(Instruction),

    pub fn make(allocator: Allocator) Chunk {
        return Chunk{
            .instructions = .init(allocator),
        };
    }

    pub fn deinit(chunk: *Chunk) void {
        chunk.instructions.deinit();
    }
};

pub const Instruction = union(enum) {
    add: struct { dest: Ref, l: Ref, r: Ref },
    sub: struct { dest: Ref, l: Ref, r: Ref },
    mul: struct { dest: Ref, l: Ref, r: Ref },
    div: struct { dest: Ref, l: Ref, r: Ref },

    cmp_lt: struct { dest: Ref, l: Ref, r: Ref },
    cmp_gt: struct { dest: Ref, l: Ref, r: Ref },
    cmp_le: struct { dest: Ref, l: Ref, r: Ref },
    cmp_ge: struct { dest: Ref, l: Ref, r: Ref },
    cmp_eq: struct { dest: Ref, l: Ref, r: Ref },
    cmp_ne: struct { dest: Ref, l: Ref, r: Ref },

    negate: struct { dest: Ref, value: Ref },
    not: struct { dest: Ref, value: Ref },

    cast: struct { dest: Ref, value: Ref, ty: Ref },

    move: struct { dest: Ref, value: Ref },

    load, // A = env[string[B..B + C]]
    store, // env[A] = B

    push, // push(A)
    pop, // A = pop()

    call, // A = B(No. C args)

    jump, // jump A
    jump_if, // if A jump B
};

pub const Type = enum(u8) {
    void,
    string,
    bool,

    i8,
    i16,
    i32,
    i64,
    isize,

    u8,
    u16,
    u32,
    u64,
    usize,

    f32,
    f64,

    ptr,
};

pub const StructDecl = struct {};

pub const Fn = struct {
    return_type: Type,
    args: std.ArrayList(Type),
    body: Chunk,
};
