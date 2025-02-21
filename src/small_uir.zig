const std = @import("std");
const Allocator = std.mem.Allocator;

allocator: Allocator,
instructions: std.ArrayList(Instruction),
constants: std.ArrayList(Constant),

const Self = @This();

pub fn make(allocator: Allocator) Self {
    return Self{
        .allocator = allocator,
        .instructions = .init(allocator),
        .constants = .init(allocator),
        .functions = .init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.instructions.deinit();
    self.constants.deinit();
    self.functions.deinit();
}

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

pub const Ref = enum(u32) {
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

pub const StringInteringPool = struct {
    data: std.ArrayList(u8),

    pub fn make(allocator: Allocator) StringInteringPool {
        return StringInteringPool{
            .data = .init(allocator),
        };
    }

    pub fn deinit(pool: *StringInteringPool) void {
        pool.data.deinit();
    }

    pub fn insert(pool: *StringInteringPool, str: []const u8) []const u8 {
        const index = pool.data.items.len;
        pool.data.appendSlice(str) catch unreachable;

        return pool.data.items[index .. index + str.len];
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

    argc, // A = No. of args
    load_arg, // A = arg(B)
    load_constant, // A = constant(B)

    load, // A = env[string[B..B + C]]
    store, // env[A] = B

    define_var, // create_var string[A..A + B] with type C
    define_const, // create_const string[A..A + B] with type C
    define_field, // field A: B = C
    define_arg, // arg: A: B

    define_struct, // A = create struct from next B instructions
    define_interface, // A = create interface from next B instructions
    define_fn, // A = create fn from next B instructions

    push, // push(A)
    pop, // A = pop()

    inline_block, // A = block of size B

    call, // A = B(No. C args)

    // terminators

    goto, // goto A
    @"if", // if A { goto B } else { goto C }
    @"return", // return A
    return_nothing, // return_nothing
    return_inline, // return_inline A
};

pub const Instruction = packed struct {
    op: OpCode,
    a: u32,
    b: u32,
    c: u32,
};

pub const Constant = union(enum) {
    pub const Index = enum(usize) {
        _,

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            try writer.print("Const.%{d}", .{@intFromEnum(self)});
        }
    };

    type: union(enum) {
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

        ptr: Ref,
        array: struct { len: usize, type: Ref },
        slice: struct { type: Ref },

        any,
        type,
    },

    int: []const u8,
    float: []const u8,
    string: []const u8,
    bool: bool,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .int => |v| try writer.print("int({s})", .{v}),
            .float => |v| try writer.print("float({s})", .{v}),
            .string => |v| try writer.print("string(\"{s}\")", .{v}),
            .bool => |v| try writer.print("bool({s})", .{if (v) "true" else "false"}),
            else => {},
        }
    }
};
