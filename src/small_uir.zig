const std = @import("std");
const Allocator = std.mem.Allocator;

allocator: Allocator,
bytes: std.ArrayList(u8).Slice,
constants: std.ArrayList(Constant).Slice,
functions: std.ArrayList(Fn).Slice,

const Self = @This();

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

pub fn fetch(self: *Self, pc: usize) []u8 {
    return self.code.bytes[pc .. pc + @sizeOf(Instruction)];
}

pub fn decode(_: *Self, bytes: []u8) Instruction {
    return std.mem.bytesAsValue(Instruction, bytes);
}

pub fn len(self: *Self) usize {
    return self.bytes.len;
}

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

    load_arg, // A = arg(B)
    load_constant, // A = constant(B)

    load, // A = env[string[B..B + C]]
    store, // env[A] = B

    define_var, // create_var string[A..A + B] with type C
    define_const, // create_const string[A..A + B] with type C
    define_field, // field A: B = C
    define_arg, // arg: A: B

    define_struct, // A = create struct of size B,
    define_interface, // A = create interface of size B,

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
    fn_ref: Fn.Index,

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
            .fn_ref => |v| try writer.print("{s}", .{v}),
            else => {},
        }
    }
};

pub const Fn = struct {
    return_type: []u8,
    body_bytes: []u8,

    pub const Index = enum(usize) {
        _,

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            try writer.print("fn({d})", .{@intFromEnum(self)});
        }
    };
    pub const Param = struct {
        name: []const u8,
        ty: []u8,
    };
};
