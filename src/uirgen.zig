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

fn emit_as_block(self: *Self, node: AST.NodeRef) u32 {
    const reg = self.temp();

    _ = self.code.encode_op(.block);
    _ = self.code.encode_u32(reg);
    const count = self.code.encode_u32(0);

    const start = self.code.len();

    _ = self.emit(node);

    self.code.decode_u32(count).* = @intCast(self.code.len() - start);

    return reg;
}

fn emit_as_comptime_block(self: *Self, node: AST.NodeRef) u32 {
    const reg = self.temp();

    _ = self.code.encode_op(.comptime_block);
    _ = self.code.encode_u32(reg);
    const count = self.code.encode_u32(0);

    const start = self.code.len();

    _ = self.emit(node);

    self.code.decode_u32(count).* = @intCast(self.code.len() - start);

    return reg;
}

fn append_tac(self: *Self, op: UIR.OpCode, a: u32, b: u32, c: u32) u32 {
    const idx = self.code.encode_op(op);
    _ = self.code.encode_u32(a);
    _ = self.code.encode_u32(b);
    _ = self.code.encode_u32(c);
    return @intCast(idx);
}

fn append_return(self: *Self, op: UIR.OpCode, b: u32, c: u32) u32 {
    const reg = self.temp();

    _ = self.code.encode_op(op);
    _ = self.code.encode_u32(reg);
    _ = self.code.encode_u32(b);
    _ = self.code.encode_u32(c);

    return reg;
}

fn append_var_decl(
    self: *Self,
    mode: enum { Var, Const },
    ident_node: AST.NodeRef,
    value_node: AST.NodeRef,
    type_node: ?AST.NodeRef,
) u32 {
    const ident = self.scopes.register(getIdent(ident_node));
    const value = self.emit(value_node);
    const @"type" = if (type_node) |ty| self.emit(ty) else self.infer(value);

    _ = self.code.encode_op(switch (mode) {
        .Const => .create_const,
        .Var => .create_var,
    });
    _ = self.code.encode_u32(ident);
    _ = self.code.encode_u32(@"type");

    _ = self.code.encode_op(.store);
    _ = self.code.encode_u32(ident);
    _ = self.code.encode_u32(value);

    const load_reg = self.temp();
    _ = self.code.encode_op(.load);
    _ = self.code.encode_u32(load_reg);
    _ = self.code.encode_u32(ident);

    return load_reg;
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

            var args: std.ArrayList(u32) = .init(self.allocator);
            defer args.deinit();

            for (n.args.items) |arg| {
                args.append(self.emit(arg)) catch unreachable;
            }

            const reg = self.temp();
            _ = self.code.encode_op(.call);
            _ = self.code.encode_u32(@"fn");
            _ = self.code.encode_u32(@intCast(n.args.items.len));

            for (args.items) |arg| {
                _ = self.code.encode_u32(arg);
            }

            break :node reg;
        },
        .FnDecl => |n| node: {
            const reg = self.temp();
            _ = self.code.encode_op(.fn_decl);
            _ = self.code.encode_u32(@intCast(n.params.ParamaterList.items.len));
            const count = self.code.encode_u32(0);

            const start = self.code.len();

            for (n.params.ParamaterList.items, 0..) |param, idx| {
                const param_idx = self.scopes.register(getIdent(param.Paramater.ident));
                const param_ty = self.emit(param.Paramater.type);

                const arg_load = self.append_return(
                    .load_arg,
                    @intCast(idx),
                    0,
                );

                _ = self.code.encode_op(.create_const);
                _ = self.code.encode_u32(param_idx);
                _ = self.code.encode_u32(param_ty);

                _ = self.code.encode_op(.store);
                _ = self.code.encode_u32(param_idx);
                _ = self.code.encode_u32(arg_load);

                const load_reg = self.temp();
                _ = self.code.encode_op(.load);
                _ = self.code.encode_u32(load_reg);
                _ = self.code.encode_u32(param_idx);
            }

            const return_type = self.emit(n.ret);
            _ = self.code.encode_op(.set_return_type);
            _ = self.code.encode_u32(return_type);

            _ = self.emit(n.block);
            self.code.decode_u32(count).* = @intCast(self.code.len() - start);

            break :node reg;
        },
        .Struct => |n| node: {
            const reg = self.temp();
            _ = self.code.encode_op(.struct_decl);
            const count = self.code.encode_u32(0);

            const start = self.code.len();

            for (n.fields.items) |field| _ = switch (field.*) {
                .Field => |f| {
                    const ident = self.scopes.register(getIdent(f.ident));
                    const ty = self.emit(f.type orelse unreachable);

                    _ = self.code.encode_op(.create_field);
                    _ = self.code.encode_u32(ident);
                    _ = self.code.encode_u32(ty);
                },
                .ConstDecl => self.emit(field),
                else => |f| @panic(@tagName(f)),
            };

            self.code.decode_u32(count).* = @intCast(self.code.len() - start);

            break :node reg;
        },
        .Comptime => |n| self.emit_as_comptime_block(n),

        .ConstDecl => |n| self.append_var_decl(.Const, n.ident, n.value, n.type),
        .VarDecl => |n| self.append_var_decl(.Var, n.ident, n.value, n.type),
        .Assignment => |n| node: {
            const ident = self.scopes.get(getIdent(n.ident)) orelse unreachable;
            const value = self.emit(n.value);

            _ = self.code.encode_op(.store);
            _ = self.code.encode_u32(ident);
            _ = self.code.encode_u32(value);

            const load_reg = self.temp();
            _ = self.code.encode_op(.load);
            _ = self.code.encode_u32(load_reg);
            _ = self.code.encode_u32(ident);

            break :node ident;
        },

        .Scope => |lst| node: {
            const reg = self.temp();
            _ = self.code.encode_op(.block);
            _ = self.code.encode_u32(reg);
            const count = self.code.encode_u32(0);

            const start = self.code.len();

            for (lst.items) |n| _ = self.emit(n);

            self.code.decode_u32(count).* = @intCast(self.code.len() - start);

            break :node reg;
        },
        .Return => |n| reg: {
            const reg = self.temp();

            const value = self.emit(n);

            _ = self.code.encode_op(.return_fn);
            _ = self.code.encode_u32(value);

            break :reg reg;
        },
        .ImplicitReturn => |n| reg: {
            const reg = self.temp();

            const value = self.emit(n);

            _ = self.code.encode_op(.return_fn);
            _ = self.code.encode_u32(value);

            break :reg reg;
        },

        else => @panic(@tagName(node.*)),
    };
}

pub fn run(self: *Self) void {
    if (self.ast.root) |root| {
        switch (root.*) {
            .TopLevelScope => |lst| {
                const reg = self.temp();

                _ = self.code.encode_op(.namespace_decl);
                _ = self.code.encode_u32(reg);
                const count = self.code.encode_u32(0);

                const start = self.code.len();

                for (lst.items) |n| _ = self.emit(n);

                self.code.decode_u32(count).* = @intCast(self.code.len() - start);
            },
            else => unreachable,
        }
    }
}

pub fn print(
    self: *Self,
) !void {
    const writer = std.io.getStdOut().writer();

    try writer.print("len: {d}\n", .{self.code.len()});

    var pc: usize = 0;

    while (pc < self.code.len()) {
        const op = self.code.inc_decode_op(&pc);
        switch (op) {
            .nop => try writer.print("nop", .{}),

            .add => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);
                const c = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = {d} + {d}", .{ a, b, c });
            },
            .sub => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);
                const c = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = {d} - {d}", .{ a, b, c });
            },
            .div => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);
                const c = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = {d} / {d}", .{ a, b, c });
            },
            .mul => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);
                const c = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = {d} * {d}", .{ a, b, c });
            },

            .cmp_lt => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);
                const c = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = {d} < {d}", .{ a, b, c });
            },
            .cmp_gt => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);
                const c = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = {d} > {d}", .{ a, b, c });
            },
            .cmp_le => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);
                const c = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = {d} <= {d}", .{ a, b, c });
            },
            .cmp_ge => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);
                const c = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = {d} >= {d}", .{ a, b, c });
            },
            .cmp_eq => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);
                const c = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = {d} == {d}", .{ a, b, c });
            },
            .cmp_ne => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);
                const c = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = {d} != {d}", .{ a, b, c });
            },

            .negative => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = -{d}", .{ a, b });
            },
            .not => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = !{d}", .{ a, b });
            },

            .cast => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);
                const c = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = {d} as {d}", .{ a, b, c });
            },
            .typeof => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = typeof({d})", .{ a, b });
            },

            .move => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = {d}", .{ a, b });
            },

            .load_constant => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = load_constant({d})", .{ a, b });
            },

            .load => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = load({d})", .{ a, b });
            },
            .store => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("store({d}, {d})", .{ a, b });
            },

            .create_var => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("create_var({d} typeof {d})", .{ a, b });
            },
            .create_const => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("create_const({d} typeof {d})", .{ a, b });
            },

            .struct_decl => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = struct_decl(len: {d})", .{ a, b });
            },
            .interface_decl => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = interface_decl(len: {d})", .{ a, b });
            },
            .create_field => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("create_field({d} typeof {d})", .{ a, b });
            },

            .fn_decl => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);
                const c = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = fn_decl(argc: {d}, len: {d})", .{ a, b, c });
            },
            .namespace_decl => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = namespace_decl(len: {d})", .{ a, b });
            },

            .argc => {
                const a = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = argc()", .{a});
            },
            .load_arg => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = load_arg({d})", .{ a, b });
            },
            .set_return_type => {
                const a = self.code.inc_decode_u32(&pc);

                try writer.print("set_return_type({d})", .{a});
            },

            .call => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);
                const c = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = call({d}, args: [", .{ a, b });
                for (0..c) |_| {
                    try writer.print("{d}, ", .{self.code.inc_decode_u32(&pc)});
                }
                try writer.print("])", .{});
            },

            .block => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = block(len: {d})", .{ a, b });
            },
            .comptime_block => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);

                try writer.print("{d} = comptime_block(len: {d})", .{ a, b });
            },

            .goto => {
                const a = self.code.inc_decode_u32(&pc);

                try writer.print("goto {d}", .{a});
            },

            .loop => {
                const a = self.code.inc_decode_u32(&pc);

                try writer.print("loop(len: {d})", .{a});
            },
            .repeat => try writer.print("repeat", .{}),
            .@"break" => try writer.print("break", .{}),

            .@"if" => {
                const a = self.code.inc_decode_u32(&pc);
                const b = self.code.inc_decode_u32(&pc);
                const c = self.code.inc_decode_u32(&pc);
                const d = self.code.inc_decode_u32(&pc);

                try writer.print(
                    "{d} = if({d}) {{ len: {d} }} else {{ len: {d} }}",
                    .{ a, b, c, d },
                );
            },
            .return_fn => {
                const a = self.code.inc_decode_u32(&pc);

                try writer.print("return_fn({d})", .{a});
            },
            .return_block => {
                const a = self.code.inc_decode_u32(&pc);

                try writer.print("return_block({d})", .{a});
            },
        }

        try writer.print("\n", .{});
    }
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
