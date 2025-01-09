const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("ast.zig");

pub const Value = union(enum) {
    pub const Data = union(enum) {
        int: []const u8,
        float: []const u8,
        string: []const u8,
        bool: bool,
        ident: []const u8,
        @"fn": Location,
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
pub const Ref = enum(usize) { _ };

// Builtin values and types
pub const Builtin = enum(usize) {
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

    call: Location,

    begin_function,
    end_function,
    @"return": Ref,

    goto: Location,
    goto_if: struct { value: Ref, to: Location },

    value: struct { addr: Ref, value: Value },
    builtin: Builtin,
};

pub const Fn = struct {
    const Param = struct {
        ty: Ref,
        name: []const u8,
    };

    params: std.ArrayList(Param),
    ret: Value,
    cfg: CFG,
};

pub const Location = enum(usize) { _ };

pub const Block = struct {
    instructions: std.ArrayList(Inst),
    terminator: ?Terminator = null,

    const Self = @This();

    pub fn make(allocator: Allocator) @This() {
        return @This(){
            .instructions = .init(allocator),
        };
    }

    pub fn deinit(self: *@This()) void {
        self.instructions.deinit();
        self.terminator = null;
    }

    pub fn append(self: *@This(), inst: Inst) void {
        self.instructions.append(inst) catch |err| {
            @panic(@errorName(err));
        };
    }
};

pub const Terminator = union(enum) {
    @"if": struct { cond: Ref, true: Location, false: Location },
    @"switch": struct {
        value: Ref,
        patterns: std.ArrayList(Value),
    },
    goto: Location,
};

pub const CFG = struct {
    root: ?Location,
    blocks: std.ArrayList(Block),

    pub fn make(allocator: Allocator) CFG {
        return CFG{
            .root = null,
            .blocks = .init(allocator),
        };
    }

    pub fn deinit(this: *CFG) void {
        this.root = null;
        this.blocks.deinit();
    }
};

pub const FnIndex = enum(usize) { _ };
pub const ScopeIndex = enum(usize) { _ };

pub const Scope = struct {
    parent: ?ScopeIndex = null,
    children: std.ArrayList(ScopeIndex),

    symbols: std.StringHashMap(void),

    pub fn make(allocator: Allocator) Scope {
        return Scope{
            .parent = null,
            .children = .init(allocator),
            .symbols = .init(allocator),
        };
    }

    pub fn makeWithParent(allocator: Allocator, parent: ScopeIndex) Scope {
        return Scope{
            .parent = parent,
            .children = .init(allocator),
            .symbols = .init(allocator),
        };
    }

    pub fn deinit(this: *Scope) void {
        this.parent = null;
        this.children.deinit();
        this.symbols.deinit();
    }
};

allocator: Allocator,
functions: std.ArrayList(Fn),
scopes: std.ArrayList(Scope),
current_scope: ScopeIndex,

const Self = @This();

pub fn make(allocator: Allocator) Self {
    const self = Self{
        .allocator = allocator,
        .functions = .init(allocator),
        .scopes = .init(allocator),
        .current_scope = @enumFromInt(0),
    };

    const toplevel = Scope.make(self.allocator);
    const toplevel_idx = self.scopes.items.len;

    self.scopes.append(toplevel) catch |err| {
        @panic(@errorName(err));
    };

    self.current_scope = @enumFromInt(toplevel_idx);

    return self;
}

pub fn down(self: *Self) void {
    const current = self.get_scope(self.current_scope);

    const child = Scope.makeWithParent(self.allocator, self.current_scope);
    const child_idx = self.scopes.items.len;

    self.scopes.append(child) catch |err| {
        @panic(@errorName(err));
    };

    current.children.append(child_idx) catch |err| {
        @panic(@errorName(err));
    };

    self.current_scope = child_idx;
}

pub fn up(self: *Self) void {
    const current = self.get_scope(self.current_scope);

    if (current.parent) |parent| {
        self.current_scope = parent;
    }
}

pub fn get_scope(self: *Self, idx: ScopeIndex) *Scope {
    return &self.scopes.items[idx];
}
