const std = @import("std");
const Allocator = std.mem.Allocator;

allocator: Allocator,
declarations: std.ArrayList(Decl),
entry_point: ?Decl.Index = null,

const Self = @This();

pub fn make(allocator: Allocator) Self {
    return Self{
        .allocator = allocator,
        .declarations = .init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.declarations.deinit();
}

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

    call: struct { @"fn": Decl.Index, args: std.ArrayList(Ref) },
};

pub const Type = union(enum) {
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

    ptr: *Type,

    @"struct": struct { index: Decl.Index },
};

pub const Decl = union(enum) {
    Func: struct {
        body: Chunk,
        args: std.ArrayList(Type),
        ret_ty: Type,
    },
    Struct: struct {
        fields: std.ArrayList(Type),
    },

    pub const Index = enum(usize) {
        _,

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            try writer.print("Decl.{d}", .{@intFromEnum(self)});
        }
    };
};

pub const Constants = struct {
    list: std.ArrayList(Value),

    pub fn make(allocator: Allocator) Constants {
        return Constants{
            .list = .init(allocator),
        };
    }

    pub fn deinit(constants: *Constants) void {
        constants.list.deinit();
    }

    pub fn append(constants: *Constants, value: Value) Value.Index {
        const index = constants.list.items.len;
        constants.list.append(value) catch unreachable;
        return @enumFromInt(index);
    }

    pub fn get(constants: *Constants, index: Value.Index) Value {
        return constants.list.items[@intFromEnum(index)];
    }

    pub const Value = union(enum) {
        pub const Index = enum(u32) {
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

        i8: i8,
        i16: i16,
        i32: i32,
        i64: i64,
        u8: u8,
        u16: u16,
        u32: u32,
        u64: u64,

        f32: f32,
        f64: f64,

        string: []const u8,
        bool: bool,

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            switch (self) {
                inline else => |v, tag| try writer.print(
                    "{s}({s})",
                    .{ @tagName(tag), v },
                ),
            }
        }
    };
};
