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
    instructions: std.ArrayList(Inst),

    pub fn make(allocator: Allocator) Chunk {
        return Chunk{
            .instructions = .init(allocator),
        };
    }

    pub fn deinit(chunk: *Chunk) void {
        chunk.instructions.deinit();
    }
};

pub const OpCode = enum(u8) {
    // Layout
    // Op A B C

    add, // A = B + C
    sub, // A = B - C
    mul, // A = B * C
    div, // A = B / C

    cmp_lt, // A = B < C
    cmp_gt, // A = B > C
    cmp_le, // A = B <= C
    cmp_ge, // A = B >= C
    cmp_eq, // A = B == C
    cmp_ne, // A = B != C

    negate, // A = !B

    cast, // A = B as C

    move, // A = B

    load, // A = env[string[B..B + C]]
    store, // env[A] = B

    push, // push(A)
    pop, // A = pop()

    call, // A = B(No. C args)

    jump, // jump A
    jump_if, // if A jump B
};

pub const Inst = packed struct {
    op: OpCode,
    a: u32,
    b: u32,
    c: u32,
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
    body: Chunk,
};
