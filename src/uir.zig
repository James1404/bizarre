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
        ref: Ref,
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
        @"fn": struct {
            args: Type,
            ret: Type,
        },
    };

    untyped: Data,
    typed: struct { data: Data, type: Type },

    pub fn make_untyped(data: Data) @This() {
        return @This(){ .untyped = data };
    }
};

// Reference a value

pub const Ref = union(enum) {
    local: usize,
    ident: []const u8,
    @"return",
};

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
    add: struct { addr: Ref, lhs: Value, rhs: Value },
    sub: struct { addr: Ref, lhs: Value, rhs: Value },
    div: struct { addr: Ref, lhs: Value, rhs: Value },
    mul: struct { addr: Ref, lhs: Value, rhs: Value },

    cmp_lt: struct { addr: Ref, lhs: Value, rhs: Value },
    cmp_gt: struct { addr: Ref, lhs: Value, rhs: Value },
    cmp_le: struct { addr: Ref, lhs: Value, rhs: Value },
    cmp_ge: struct { addr: Ref, lhs: Value, rhs: Value },
    cmp_eq: struct { addr: Ref, lhs: Value, rhs: Value },
    cmp_ne: struct { addr: Ref, lhs: Value, rhs: Value },

    negate: struct { addr: Ref, value: Value },

    cast: struct { addr: Ref, value: Value, type: Value },

    set: struct { addr: Ref, value: Value },

    push_param: Ref,
    pop_param: struct { addr: Ref },

    call: Ref,
};

pub const Terminator = union(enum) {
    @"if": struct { cond: Ref, true: Location, false: Location },
    @"switch": struct {
        value: Ref,
        patterns: std.ArrayList(Value),
    },
    goto: Location,
    @"return",
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
    counter: usize = 1,

    parent: ?Location,

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

    pub fn temp(self: *@This()) Ref {
        const idx = self.counter;
        self.counter += 1;
        return Ref{ .local = idx };
    }

    pub fn append(self: *@This(), inst: Inst) void {
        self.instructions.append(inst) catch |err| {
            @panic(@errorName(err));
        };
    }

    pub fn emit_temp_value(self: *@This(), value: Value.Data) void {
        const addr = self.temp();
        self.append(.{ .set = .{
            .addr = addr,
            .value = .make_untyped(value),
        } });
        return addr;
    }
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

    pub fn get(self: *@This(), loc: Location) *Block {
        return &self.blocks.items[@intFromEnum(loc)];
    }
};

pub const FnIndex = enum(usize) { _ };
pub const ScopeIndex = enum(usize) { _ };

pub const Scope = struct {
    parent: ?ScopeIndex = null,
    children: std.ArrayList(ScopeIndex),

    symbols: std.StringHashMap(Value),

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

    pub fn register(this: *Scope, name: []const u8, value: Value) void {
        this.symbols.put(name, value) catch |err| {
            @panic(@errorName(err));
        };
    }

    pub fn get(this: *Scope, name: []const u8) ?Value {
        return this.symbols.get(name);
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
ast: AST,

const Self = @This();

pub fn make(allocator: Allocator, ast: AST) Self {
    var self = Self{
        .allocator = allocator,
        .functions = .init(allocator),
        .scopes = .init(allocator),
        .current_scope = @enumFromInt(0),
        .ast = ast,
    };

    const toplevel = Scope.make(self.allocator);
    const toplevel_idx = self.scopes.items.len;

    self.scopes.append(toplevel) catch |err| {
        @panic(@errorName(err));
    };

    self.current_scope = @enumFromInt(toplevel_idx);

    return self;
}

pub fn deinit(self: *Self) void {
    self.functions.deinit();
    self.scopes.deinit();
}

fn down(self: *Self) void {
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

fn up(self: *Self) void {
    const current = self.get_scope(self.current_scope);

    if (current.parent) |parent| {
        self.current_scope = parent;
    }
}

fn get_scope(self: *Self, idx: ScopeIndex) *Scope {
    return &self.scopes.items[@intFromEnum(idx)];
}

fn get_fn(self: *Self, idx: FnIndex) *Fn {
    return &self.functions.items[@intFromEnum(idx)];
}

fn emitExpression(
    self: *Self,
    cfg: *CFG,
    loc: Location,
    node: AST.NodeRef,
) Ref {
    const block = cfg.get(loc);

    return switch (node.*) {
        .Unary => |n| node: {
            const value = Value.make_untyped(block.emit_temp_value(n.node));
            const addr = self.temp();

            switch (n.Op.ty) {
                .Minus => block.append(.{ .negate = .{
                    .addr = addr,
                    .value = value,
                } }),
                else => unreachable,
            }

            break :node addr;
        },
        .Binary => |n| node: {
            const lhs = Value.make_untyped(block.emit_temp_value(n.lhs));
            const rhs = Value.make_untyped(block.emit_temp_value(n.rhs));
            const addr = self.temp();

            switch (n.Op.ty) {
                .Plus => block.append(.{ .add = .{
                    .addr = addr,
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Minus => block.append(.{ .sub = .{
                    .addr = addr,
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Asterix => block.append(.{ .mul = .{
                    .addr = addr,
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Slash => block.append(.{ .div = .{
                    .addr = addr,
                    .lhs = lhs,
                    .rhs = rhs,
                } }),

                .Less => block.append(.{ .cmp_lt = .{
                    .addr = addr,
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .LessEq => block.append(.{ .cmp_le = .{
                    .addr = addr,
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Greater => block.append(.{ .cmp_gt = .{
                    .addr = addr,
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .GreaterEq => block.append(.{ .cmp_ge = .{
                    .addr = addr,
                    .lhs = lhs,
                    .rhs = rhs,
                } }),

                else => unreachable,
            }

            break :node addr;
        },
        .String => |v| block.emit_temp_value(.{ .string = v }),
        .Int => |v| block.emit_temp_value(.{ .int = v }),
        .Float => |v| block.emit_temp_value(.{ .float = v }),
        .Bool => |v| block.emit_temp_value(.{ .bool = v }),
        .Ident => |v| block.emit_temp_value(.{ .ident = v }),
        .FnCall => |n| node: {
            for (n.args.items) |arg| {
                const ref = self.emitExpression(cfg, loc, arg);
                block.append(.{ .push_param = ref });
            }

            const @"fn" = self.emitExpression(n.@"fn");
            break :node block.append(.{ .call = @"fn" });
        },
        else => unreachable,
    };
}

fn getIdent(_: *Self, node: AST.NodeRef) ?[]const u8 {
    return switch (node.*) {
        .Ident => |tok| tok.text,
        else => null,
    };
}

fn emitFnStmt(
    self: *Self,
    cfg: *CFG,
    loc: Location,
    node: AST.NodeRef,
) void {
    const block = cfg.get(loc);

    switch (node.*) {
        .ConstDecl => |n| {
            const ident = self.getIdent(n.ident) orelse "Unknown identifier";
            const value = self.emitExpression(cfg, loc, n.value);

            block.append(.{ .set = .{
                .addr = .{ .ident = ident },
                .value = value,
            } });
        },
        .VarDecl => |n| {
            const ident = self.getIdent(n.ident) orelse "Unknown identifier";
            const value = self.emitExpression(cfg, loc, n.value);

            block.append(.{ .set = .{
                .addr = .{ .ident = ident },
                .value = value,
            } });
        },
        .Assignment => |n| {
            const ident = self.getIdent(n.ident) orelse "Unknown identifier";
            const value = self.emitExpression(cfg, loc, n.value);

            block.append(.{ .set = .{
                .addr = .{ .ident = ident },
                .value = value,
            } });
        },
        else => {},
    }
}

fn emitTopLevelStmt(self: *Self, node: AST.NodeRef) void {
    switch (node.*) {
        .ConstDecl => |n| {
            const ident = self.getIdent(n.ident);
        },
        .VarDecl => |n| {},
        .Comptime => |n| {},
        else => @panic("Invalid top level statement"),
    }
}

pub fn run(self: *Self) void {
    if (self.ast.root) |root| {
        switch (root.*) {
            .TopLevelScope => |lst| {
                for (lst.items) |n| {
                    self.emitTopLevelStmt(n);
                }
            },
            else => unreachable,
        }
    }
}
