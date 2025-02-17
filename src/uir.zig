const std = @import("std");
const Allocator = std.mem.Allocator;

functions: std.ArrayList(Fn),
chunks: std.ArrayList(Chunk),
constants: std.ArrayList(Constant),

// TODO: Add primitives to Ref, like: true, false, u8, u32, bool, null, etc...
// TODO: Allow refs to reference constants
// TODO: Add some kind of constant table
// TODO: Add a string interning pool

const Self = @This();

// A reference to a register
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

pub const Block = struct {
    pub const Index = enum(usize) {
        _,

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            try writer.print("Block.%{d}", .{@intFromEnum(self)});
        }
    };

    instructions: std.ArrayList(Inst),
    terminator: ?Terminator,

    pub fn deinit(chunk: *Block) void {
        for (chunk.instructions.items) |*inst| {
            inst.deinit();
        }
        chunk.instructions.deinit();

        if (chunk.terminator) |*term| {
            term.deinit();
        }
    }

    pub fn append(chunk: *Block, inst: Inst) Ref {
        const index = chunk.instructions.items.len;
        chunk.instructions.append(inst) catch unreachable;
        return @enumFromInt(index);
    }

    pub fn get(chunk: *Block, ref: Ref) *Inst {
        return &chunk.instructions.items[@intFromEnum(ref)];
    }
};

pub const CFG = struct {
    blocks: std.ArrayList(Block),
};

pub const Chunk = struct {
    pub const Index = enum(usize) {
        _,

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            try writer.print("Chunk.%{d}", .{@intFromEnum(self)});
        }
    };

    instructions: std.ArrayList(Inst),
    terminator: ?Terminator,

    pub fn deinit(chunk: *Chunk) void {
        for (chunk.instructions.items) |*inst| {
            inst.deinit();
        }
        chunk.instructions.deinit();

        if (chunk.terminator) |*term| {
            term.deinit();
        }
    }

    pub fn append(chunk: *Chunk, inst: Inst) Ref {
        const index = chunk.instructions.items.len;
        chunk.instructions.append(inst) catch unreachable;
        return @enumFromInt(index);
    }

    pub fn get(chunk: *Chunk, ref: Ref) *Inst {
        return &chunk.instructions.items[@intFromEnum(ref)];
    }
};

pub const Terminator = union(enum) {
    match: struct {
        const Pattern = struct {
            value: Ref,
            branch: Chunk.Index,
        };

        value: Ref,
        patterns: std.ArrayList(Pattern),
        @"else": ?Chunk.Index,
    },
    @"if": struct {
        cond: Ref,
        true: Chunk.Index,
        false: Chunk.Index,
    },
    goto: Chunk.Index,
    @"return": struct { value: Ref },
    return_nothing,
    inline_return: struct { value: Ref },

    pub fn deinit(term: *Terminator) void {
        switch (term.*) {
            .match => |n| {
                n.patterns.deinit();
            },
            else => {},
        }
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .@"if" => |v| try writer.print(
                "if({s}, {d}, {d})",
                .{ v.cond, v.true_len, v.false_len },
            ),
            .goto => |v| try writer.print("goto({d})", .{v}),
            .@"return" => |n| try writer.print("return({s})", .{n.value}),
            .return_nothing => try writer.print("return_nothing", .{}),
            .inline_return => |n| try writer.print("inline_return({s})", .{n.value}),
        }
    }
};

pub const Fn = struct {
    // An index into the function list
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
    pub const Param = struct { name: []const u8, ty: Chunk.Index };

    return_type: Chunk.Index,
    parameters: std.ArrayList(Param),
    body: Chunk.Index,

    pub fn deinit(@"fn": *Fn) void {
        @"fn".parameters.deinit();
    }
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

pub const Inst = union(enum) {
    nop: struct {},

    add: struct { lhs: Ref, rhs: Ref },
    sub: struct { lhs: Ref, rhs: Ref },
    div: struct { lhs: Ref, rhs: Ref },
    mul: struct { lhs: Ref, rhs: Ref },

    cmp_lt: struct { lhs: Ref, rhs: Ref },
    cmp_gt: struct { lhs: Ref, rhs: Ref },
    cmp_le: struct { lhs: Ref, rhs: Ref },
    cmp_ge: struct { lhs: Ref, rhs: Ref },
    cmp_eq: struct { lhs: Ref, rhs: Ref },
    cmp_ne: struct { lhs: Ref, rhs: Ref },

    negate: struct { value: Ref },

    cast: struct { value: Ref, type: Ref },
    typeof: struct { value: Ref },

    move: struct { value: Ref },

    load_arg: struct { identifier: []const u8 },
    load_constant: struct { constant: Constant.Index },

    define_var: struct { identifier: []const u8, type: Ref },
    define_const: struct { identifier: []const u8, type: Ref },
    load: struct { identifier: []const u8 },
    store: struct { identifier: []const u8, value: Ref },

    namespace: struct {
        decls: std.StringHashMap(Chunk.Index),
    },
    @"struct": struct {
        fields: std.StringHashMap(Chunk.Index),
        decls: std.StringHashMap(Chunk.Index),
    },
    interface: struct {
        decls: std.StringHashMap(Chunk.Index),
    },

    block: struct { chunk: Chunk.Index },
    comptime_block: struct { chunk: Chunk.Index },

    call: struct {
        @"fn": Ref,
        args: std.ArrayList(Ref),
    },

    todo,

    pub fn deinit(inst: *Inst) void {
        switch (inst.*) {
            .namespace => |*n| {
                n.decls.deinit();
            },
            .@"struct" => |*n| {
                n.decls.deinit();
                n.fields.deinit();
            },
            .interface => |*n| {
                n.decls.deinit();
            },
            else => {},
        }
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .nop => try writer.print("nop", .{}),

            .add => |v| try writer.print("{s} + {s}", .{ v.lhs, v.rhs }),
            .sub => |v| try writer.print("{s} - {s}", .{ v.lhs, v.rhs }),
            .mul => |v| try writer.print("{s} * {s}", .{ v.lhs, v.rhs }),
            .div => |v| try writer.print("{s} / {s}", .{ v.lhs, v.rhs }),

            .cmp_lt => |v| try writer.print("{s} < {s}", .{ v.lhs, v.rhs }),
            .cmp_gt => |v| try writer.print("{s} > {s}", .{ v.lhs, v.rhs }),
            .cmp_le => |v| try writer.print("{s} <= {s}", .{ v.lhs, v.rhs }),
            .cmp_ge => |v| try writer.print("{s} >= {s}", .{ v.lhs, v.rhs }),
            .cmp_eq => |v| try writer.print("{s} == {s}", .{ v.lhs, v.rhs }),
            .cmp_ne => |v| try writer.print("{s} != {s}", .{ v.lhs, v.rhs }),

            .negate => |v| try writer.print("-{s}", .{v.value}),

            .cast => |v| try writer.print("cast({s}, {s})", .{ v.value, v.type }),
            .typeof => |v| try writer.print("typeof({s})", .{v.value}),

            .move => |v| try writer.print("move({s})", .{v.value}),

            .load_arg => |v| try writer.print("load_arg({s})", .{v.identifier}),
            .load_constant => |v| try writer.print("load_constant({s})", .{v.constant}),

            .define_var => |v| try writer.print("define_var({s}, {s})", .{ v.identifier, v.type }),
            .define_const => |v| try writer.print("define_const({s}, {s})", .{ v.identifier, v.type }),

            .load => |v| try writer.print("load({s})", .{v.identifier}),
            .store => |v| try writer.print("store({s}, {s})", .{ v.identifier, v.value }),

            .namespace => |n| {
                try writer.print("namespace(", .{});

                var iter = n.decls.iterator();
                while (iter.next()) |kv| {
                    try writer.print("{{{s}}}", .{kv.value_ptr.*});
                }

                try writer.print(")", .{});
            },
            .@"struct" => |n| {
                try writer.print("struct(", .{});

                var iter = n.decls.iterator();
                while (iter.next()) |kv| {
                    try writer.print("{s} = {s},", .{ kv.key_ptr.*, kv.value_ptr.* });
                }

                try writer.print(")", .{});
            },
            .interface => |n| {
                try writer.print("interface(", .{});

                var iter = n.decls.iterator();
                while (iter.next()) |kv| {
                    try writer.print("{s} = {s},", .{ kv.key_ptr.*, kv.value_ptr.* });
                }

                try writer.print(")", .{});
            },

            .block => |v| try writer.print("block({s})", .{v}),
            .comptime_block => |v| try writer.print("comptime_block({s})", .{v}),

            .call => |n| {
                try writer.print("call({s}, {{", .{n.@"fn"});
                for (n.args.items) |arg| {
                    try writer.print("{s},", .{arg});
                }
                try writer.print("}})", .{});
            },

            .todo => try writer.print("todo", .{}),
        }
    }
};

pub fn make(allocator: Allocator) Self {
    return Self{
        .chunks = .init(allocator),
        .functions = .init(allocator),
        .constants = .init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    for (self.chunks.items) |*chunk| {
        chunk.deinit();
    }
    self.chunks.deinit();

    for (self.functions.items) |@"fn"| {
        @"fn".parameters.deinit();
    }
    self.functions.deinit();

    self.constants.deinit();
}

pub fn append(self: *Self, chunk: Chunk) Chunk.Index {
    const index = self.chunks.items.len;
    self.chunks.append(chunk) catch unreachable;
    return @enumFromInt(index);
}

pub fn get(self: *Self, index: Chunk.Index) *Chunk {
    std.log.info("Get {s}, chunks len: {d}", .{
        index,
        self.chunks.items.len,
    });
    return &self.chunks.items[@intFromEnum(index)];
}

pub fn append_fn(self: *Self, @"fn": Fn) Fn.Index {
    const index = self.functions.items.len;
    self.functions.append(@"fn") catch unreachable;
    return @enumFromInt(index);
}

pub fn get_fn(self: *Self, index: Fn.Index) *Fn {
    return &self.functions.items[@intFromEnum(index)];
}

pub fn append_constant(self: *Self, @"const": Constant) Constant.Index {
    const index = self.constants.items.len;
    self.chunks.append(@"const") catch unreachable;
    return @enumFromInt(index);
}

pub fn get_constant(self: *Self, index: Constant.Index) Constant {
    return self.constants.items[@intFromEnum(index)];
}
