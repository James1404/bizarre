const std = @import("std");
const Allocator = std.mem.Allocator;

const UIR = @import("uir.zig");
const AST = @import("ast.zig");

allocator: Allocator,
register_count: u32 = 0,
code: UIR,
scopes: Scopes,
ast: AST,

const Self = @This();

pub fn make(allocator: Allocator, ast: AST) Self {
    return Self{
        .allocator = allocator,
        .code = .make(allocator),
        .scopes = .make(allocator),
        .ast = ast,
    };
}

pub fn deinit(self: *Self) void {
    self.code.deinit();
    self.scopes.deinit();
}

fn getIdent(node: AST.NodeRef) []const u8 {
    return switch (node.*) {
        .Ident => |tok| tok.text,
        else => unreachable,
    };
}

fn emit_as_constant(self: *Self, value: UIR.Constants.Value) u32 {
    const index = self.code.constants.append(value);
    return self.append_return(
        .load_constant,
        @intFromEnum(index),
        0,
    );
}

fn infer(self: *Self, ref: u32) u32 {
    return self.append_return(.typeof, ref, 0);
}

fn temp(self: *Self) u32 {
    const idx = self.register_count;
    self.register_count += 1;
    return idx;
}

fn append(
    self: *Self,
    op: UIR.OpCode,
    a: u32,
    b: u32,
    c: u32,
) void {
    self.code.instructions.append(.{
        .op = op,
        .a = a,
        .b = b,
        .c = c,
    }) catch unreachable;
}

fn append_return(
    self: *Self,
    op: UIR.OpCode,
    b: u32,
    c: u32,
) u32 {
    const idx = self.temp();

    self.append(op, idx, b, c);

    return idx;
}

fn emit_as_block(self: *Self, node: AST.NodeRef) u32 {
    const idx = self.code.len();
    const block = self.append_return(.block, 0, 0);

    const start = self.code.len();

    _ = self.emit(node);

    self.code.get(idx).b = @intCast(self.code.len() - start);

    return block;
}

fn emit_as_comptime_block(self: *Self, node: AST.NodeRef) u32 {
    const idx = self.code.len();
    const block = self.append_return(.comptime_block, 0, 0);

    const start = self.code.len();

    _ = self.emit(node);

    self.code.get(idx).b = @intCast(self.code.len() - start);

    return block;
}

fn emit(
    self: *Self,
    node: AST.NodeRef,
) u32 {
    return switch (node.*) {
        .Unary => |n| node: {
            const value = self.emit(n.node);
            break :node switch (n.Op.ty) {
                .Minus => self.append_return(.negative, value, 0),
                else => unreachable,
            };
        },
        .Binary => |n| node: {
            const lhs = self.emit(n.lhs);
            const rhs = self.emit(n.rhs);

            break :node switch (n.Op.ty) {
                .Plus => self.append_return(.add, lhs, rhs),
                .Minus => self.append_return(.sub, lhs, rhs),
                .Asterix => self.append_return(.mul, lhs, rhs),
                .Slash => self.append_return(.div, lhs, rhs),

                .Less => self.append_return(.cmp_lt, lhs, rhs),
                .LessEq => self.append_return(.cmp_le, lhs, rhs),
                .Greater => self.append_return(.cmp_gt, lhs, rhs),
                .GreaterEq => self.append_return(.cmp_ge, lhs, rhs),

                .EqualEqual => self.append_return(.cmp_eq, lhs, rhs),
                .NotEqual => self.append_return(.cmp_ne, lhs, rhs),

                else => unreachable,
            };
        },
        .String => |v| self.emit_as_constant(.{ .string = v }),
        .Int => |v| self.emit_as_constant(.{ .int = v }),
        .Float => |v| self.emit_as_constant(.{ .float = v }),
        .Bool => |v| self.emit_as_constant(.{ .bool = v }),
        .Ident => |v| node: {
            const index = self.scopes.get(v.text) orelse 0;
            break :node self.append_return(.load, index, 0);
        },
        .FnCall => |n| node: {
            const @"fn" = self.emit(n.@"fn");

            const argc = n.args.items.len;
            for (n.args.items, 0..) |arg, idx| {
                const value = self.emit(arg);

                _ = self.append_return(.set_arg, @intCast(idx), value);
            }

            break :node self.append_return(.call, @"fn", @intCast(argc));
        },
        .FnDecl => |n| node: {
            const @"fn" = self.append_return(
                .fn_decl,
                @intCast(n.params.ParamaterList.items.len),
                0,
            );

            const start = self.code.len();

            for (n.params.ParamaterList.items, 0..) |param, idx| {
                const param_idx = self.scopes.register(getIdent(param.Paramater.ident));
                const param_ty = self.emit(param.Paramater.type);

                const arg_load = self.append_return(
                    .load_arg,
                    @intCast(idx),
                    0,
                );

                self.append(.create_const, param_idx, param_ty, 0);
                self.append(.store, param_idx, arg_load, 0);
            }

            const return_type = self.emit(n.ret);
            self.append(.set_return_type, return_type, 0, 0);

            _ = self.emit(n.block);
            self.code.get(@"fn").c = @intCast(self.code.len() - start);

            break :node @"fn";
        },
        .Struct => |n| node: {
            const @"struct" = self.append_return(.struct_decl, 0, 0);
            const start = self.code.len();

            for (n.fields.items) |field| _ = switch (field.*) {
                .Field => |f| {
                    const ident = self.scopes.register(getIdent(f.ident));
                    const ty = self.emit(f.type orelse unreachable);

                    self.append(.create_field, ident, ty, 0);
                },
                .ConstDecl => self.emit(field),
                else => |f| @panic(@tagName(f)),
            };

            self.code.get(@"struct").b = @intCast(self.code.len() - start);

            break :node @"struct";
        },
        .Comptime => |n| self.emit_as_comptime_block(n),

        .ConstDecl => |n| node: {
            const ident = self.scopes.register(getIdent(n.ident));
            const value = self.emit(n.value);
            const @"type" = if (n.type) |ty| self.emit(ty) else self.infer(value);

            self.append(.create_const, ident, @"type", 0);
            self.append(.store, ident, value, 0);

            break :node self.append_return(.load, ident, 0);
        },
        .VarDecl => |n| node: {
            const ident = self.scopes.register(getIdent(n.ident));
            const value = self.emit(n.value);
            const @"type" = if (n.type) |ty| self.emit(ty) else self.infer(value);

            self.append(.create_var, ident, @"type", 0);
            self.append(.store, ident, value, 0);

            break :node self.append_return(.load, ident, 0);
        },
        .Assignment => |n| node: {
            const ident = self.scopes.get(getIdent(n.ident)) orelse unreachable;
            const value = self.emit(n.value);

            self.append(.store, ident, value, 0);
            break :node self.append_return(.load, ident, 0);
        },

        .Scope => |lst| node: {
            const block = self.append_return(.block, 0, 0);

            const start = self.code.len();

            for (lst.items) |n| _ = self.emit(n);

            self.code.get(block).b = @intCast(self.code.len() - start);

            break :node block;
        },
        .Return => |n| self.append_return(.return_fn, self.emit(n), 0),
        .ImplicitReturn => |n| self.append_return(.return_fn, self.emit(n), 0),

        else => @panic(@tagName(node.*)),
    };
}

pub fn run(self: *Self) void {
    if (self.ast.root) |root| {
        switch (root.*) {
            .TopLevelScope => |lst| {
                const namespace = self.append_return(.namespace_decl, 0, 0);

                const start = self.code.len();

                for (lst.items) |n| _ = self.emit(n);

                self.code.get(namespace).b = @intCast(self.code.len() - start);
            },
            else => unreachable,
        }
    }
}

pub fn format(
    self: Self,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    try writer.print("len: {d}\n", .{self.code.instructions.items.len});
    for (self.code.instructions.items) |inst| {
        try writer.print("{s}\n", .{inst});
    }
}

pub fn print(self: *Self) void {
    const out = std.debug.print;
    out("{s}\n", .{self.*});
}

pub const Scopes = struct {
    allocator: Allocator,
    list: std.ArrayList(Node),
    current: ?Index = null,
    depth: usize = 0,
    counter: u32 = 0,

    pub fn make(allocator: Allocator) Scopes {
        return Scopes{
            .allocator = allocator,
            .list = .init(allocator),
        };
    }

    pub fn deinit(self: *Scopes) void {
        defer self.list.deinit();
        for (self.list.items) |*scope| {
            scope.deinit();
        }
    }

    pub fn get_scope(self: *Scopes, idx: Index) *Node {
        return &self.list.items[@intFromEnum(idx)];
    }

    pub fn get_current(self: *Scopes) ?*Node {
        return if (self.current) |idx| self.get_scope(idx) else null;
    }

    pub fn down(self: *Scopes) void {
        const current = self.get_scope(self.current_scope);

        self.depth += 1;

        const child = Node.makeWithParent(self.allocator, self.depth, self.current_scope);
        const child_idx = self.list.items.len;

        self.list.append(child) catch unreachable;
        current.children.append(child_idx) catch unreachable;

        self.current_scope = child_idx;
    }

    pub fn up(self: *Scopes) void {
        const current = self.get_scope(self.current_scope);

        if (current.parent) |parent| {
            self.depth -= 1;
            self.current_scope = parent;
        }
    }

    pub fn register(scopes: *Scopes, ident: []const u8) u32 {
        const idx = scopes.counter;
        scopes.counter += 1;
        if (scopes.get_current()) |current| {
            current.symbols.put(ident, idx) catch unreachable;
        }
        return idx;
    }

    pub fn get(scopes: *Scopes, ident: []const u8) ?u32 {
        var current_scope = scopes.get_current();

        while (current_scope) |scope| {
            if (scope.symbols.get(ident)) |idx| {
                return idx;
            }

            current_scope = if (scope.parent) |parent|
                scopes.get_scope(parent)
            else
                null;
        }

        return null;
    }

    pub const Node = struct {
        parent: ?Index = null,
        depth: usize,

        children: std.ArrayList(Index),
        symbols: std.StringHashMap(u32),

        pub fn make(allocator: Allocator, depth: usize) Node {
            return Node{
                .parent = null,
                .depth = depth,
                .children = .init(allocator),
                .symbols = .init(allocator),
            };
        }

        pub fn makeWithParent(allocator: Allocator, depth: usize, parent: Index) Node {
            return Node{
                .parent = parent,
                .depth = depth,
                .children = .init(allocator),
                .symbols = .init(allocator),
            };
        }

        pub fn deinit(this: *Node) void {
            this.parent = null;
            this.children.deinit();
            this.symbols.deinit();
        }
    };

    pub const Index = enum(usize) { _ };
};
