const std = @import("std");
const Allocator = std.mem.Allocator;

const UIR = @import("uir.zig");
const AST = @import("ast.zig");

allocator: Allocator,
ast: AST,

code: UIR,
scopes: Scopes,

const Self = @This();

pub fn make(allocator: Allocator, ast: AST) Self {
    return Self{
        .allocator = allocator,
        .ast = ast,
        .code = .make(allocator),
        .scopes = .make(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.code.deinit();
    self.scopes.deinit();
}

fn register_local(
    self: *Self,
    cfg: *CFGGen,
    ident: []const u8,
) UIR.Ref {
    const reg = cfg.reg();
    _ = self.scopes.add(ident, .{ .ref = reg });
    return reg;
}

fn register_decl(
    self: *Self,
    ident: []const u8,
    index: UIR.Decl.Index,
) void {
    _ = self.scopes.add(ident, .{ .decl = index });
}

fn get_local(self: *Self, ident: []const u8) UIR.Ref {
    if (self.scopes.get(ident)) |local| {
        switch (local.value) {
            .ref => |ref| return ref,
            else => {},
        }
    }

    std.log.err("Variable \"{s}\" is not defined", .{ident});
    unreachable;
}

fn load_variable(
    self: *Self,
    cfg: *CFGGen,
    ident: []const u8,
    out: UIR.Ref,
) void {
    if (self.scopes.get(ident)) |variable| {
        switch (variable.value) {
            .ref => |ref| cfg.append(.{ .load = .{
                .dest = out,
                .ref = ref,
            } }),
            .decl => |decl| cfg.append(.{ .load_decl = .{
                .dest = out,
                .index = decl,
            } }),
        }
        return;
    }

    std.log.err("Variable \"{s}\" is not defined", .{ident});
    unreachable;
}

fn set_variable(
    self: *Self,
    cfg: *CFGGen,
    ident: []const u8,
    value: UIR.Ref,
) void {
    if (self.scopes.get(ident)) |variable| {
        switch (variable.value) {
            .ref => |ref| cfg.append(.{ .store = .{
                .ref = ref,
                .value = value,
            } }),
            .decl => |decl| cfg.append(.{ .store_decl = .{
                .index = decl,
                .value = value,
            } }),
        }

        return;
    }

    std.log.err("Variable \"{s}\" is not defined", .{ident});
    unreachable;
}

fn getIdent(node: AST.NodeRef) []const u8 {
    return switch (node.*) {
        .Ident => |tok| tok.text,
        else => unreachable,
    };
}

fn emit_as_constant(
    self: *Self,
    cfg: *CFGGen,
    value: UIR.Constants.Value,
) UIR.Ref {
    const reg = cfg.reg();
    const index = self.code.constants.append(value);

    cfg.append(.{ .load_constant = .{
        .dest = reg,
        .index = index,
    } });

    return reg;
}

fn infer(_: *Self, cfg: *CFGGen, ref: UIR.Ref) UIR.Ref {
    const reg = cfg.reg();
    cfg.append(.{ .typeof = .{ .dest = reg, .value = ref } });
    return reg;
}

fn emit_as_block(self: *Self, cfg: *CFGGen, node: AST.NodeRef) UIR.Ref {
    const reg = cfg.reg();

    const blockLoc = cfg.branch(false);

    cfg.set_terminator(.{ .goto = blockLoc });
    cfg.set_current(blockLoc);

    _ = self.emit_expr(cfg, node);

    const afterLoc = cfg.branch(false);
    cfg.set_terminator(.{ .goto = afterLoc });
    cfg.set_current(afterLoc);

    return reg;
}

fn emit_as_comptime_block(self: *Self, cfg: *CFGGen, node: AST.NodeRef) UIR.Ref {
    const reg = cfg.reg();

    const blockLoc = cfg.branch(true);

    cfg.set_terminator(.{ .goto = blockLoc });
    cfg.set_current(blockLoc);

    _ = self.emit_expr(cfg, node);

    const afterLoc = cfg.branch(true);
    cfg.set_terminator(.{ .goto = afterLoc });
    cfg.set_current(afterLoc);

    return reg;
}

fn emit_statement(self: *Self, cfg: *CFGGen, node: AST.NodeRef) void {
    switch (node.*) {
        .ConstDecl => |n| {
            const ident = self.register_local(cfg, getIdent(n.ident));

            const value = self.emit_expr(cfg, n.value);
            const @"type" = if (n.type) |ty|
                self.emit_expr(cfg, ty)
            else
                self.infer(cfg, value);

            cfg.append(.{ .typed_value = .{
                .dest = ident,
                .value = value,
                .ty = @"type",
            } });
        },
        .VarDecl => |n| {
            const ident = self.register_local(cfg, getIdent(n.ident));

            const value = self.emit_expr(cfg, n.value);
            const @"type" = if (n.type) |ty|
                self.emit_expr(cfg, ty)
            else
                self.infer(cfg, value);

            cfg.append(.{ .typed_value = .{
                .dest = ident,
                .value = value,
                .ty = @"type",
            } });
        },
        .Assignment => |n| {
            const value = self.emit_expr(cfg, n.value);
            self.set_variable(cfg, getIdent(n.ident), value);
        },

        .Return => |n| {
            cfg.set_terminator(.{ .@"return" = .{
                .value = self.emit_expr(cfg, n),
            } });
        },
        .ImplicitReturn => |n| {
            cfg.append(.{ .store = .{
                .ref = cfg.pop_return_stack(),
                .value = self.emit_expr(cfg, n),
            } });
        },
        .Scope => |lst| {
            self.scopes.down();

            for (lst.items) |n| self.emit_statement(cfg, n);

            self.scopes.up();
        },
        .If => |n| {
            const cond = self.emit_expr(cfg, n.cond);

            const true_branch = cfg.branch(false);
            const false_branch = cfg.branch(false);
            const after_branch = cfg.branch(false);
            cfg.set_terminator(.{ .@"if" = .{
                .cond = cond,
                .true = true_branch,
                .false = false_branch,
            } });

            {
                cfg.set_current(true_branch);
                self.scopes.down();

                for (n.true.Scope.items) |item| {
                    self.emit_statement(cfg, item);
                }

                cfg.set_terminator(.{ .goto = after_branch });
                self.scopes.up();
            }

            {
                cfg.set_current(false_branch);
                self.scopes.down();

                for (n.true.Scope.items) |item| {
                    self.emit_statement(cfg, item);
                }

                cfg.set_terminator(.{ .goto = after_branch });
                self.scopes.up();
            }
        },

        else => _ = self.emit_expr(cfg, node),
    }
}

fn emit_global_statement(self: *Self, node: AST.NodeRef) void {
    var cfg = CFGGen.make(self.allocator);

    switch (node.*) {
        .ConstDecl => |n| {
            switch (n.value.*) {
                .FnDecl => {
                    const decl = self.code.add_decl(.{
                        .cfg = undefined,
                        .mode = .Fn,
                    });
                    self.register_decl(getIdent(n.ident), decl);
                    self.emit_fn_decl(&cfg, n.value);

                    self.code.get_decl(decl).cfg = cfg.graph;

                    if (std.mem.eql(u8, getIdent(n.ident), "main")) {
                        self.code.entry_point = decl;
                    }
                },
                else => {
                    const value = self.emit_expr(&cfg, n.value);

                    const @"type" = if (n.type) |ty|
                        self.emit_expr(&cfg, ty)
                    else
                        self.infer(&cfg, value);

                    const ret = cfg.reg();
                    cfg.append(.{ .typed_value = .{
                        .dest = ret,
                        .value = value,
                        .ty = @"type",
                    } });

                    cfg.set_terminator(.{ .@"return" = .{ .value = ret } });

                    const decl = self.code.add_decl(.{
                        .cfg = cfg.graph,
                        .mode = .Const,
                    });
                    self.register_decl(getIdent(n.ident), decl);
                },
            }
        },
        .VarDecl => |n| {
            switch (n.value.*) {
                .FnDecl => {
                    const decl = self.code.add_decl(.{
                        .cfg = cfg.graph,
                        .mode = .Fn,
                    });
                    self.register_decl(getIdent(n.ident), decl);
                    self.emit_fn_decl(&cfg, n.value);
                },
                else => {
                    const value = self.emit_expr(&cfg, n.value);

                    const @"type" = if (n.type) |ty|
                        self.emit_expr(&cfg, ty)
                    else
                        self.infer(&cfg, value);

                    const ret = cfg.reg();
                    cfg.append(.{ .typed_value = .{
                        .dest = ret,
                        .value = value,
                        .ty = @"type",
                    } });
                    cfg.set_terminator(.{ .@"return" = .{ .value = ret } });

                    const decl = self.code.add_decl(.{
                        .cfg = cfg.graph,
                        .mode = .Var,
                    });
                    self.register_decl(getIdent(n.ident), decl);
                },
            }
        },
        else => std.debug.panic(
            "Invalid Toplevel statement: {s}",
            .{@tagName(node.*)},
        ),
    }
}

fn emit_fn_decl(
    self: *Self,
    cfg: *CFGGen,
    node: AST.NodeRef,
) void {
    const n = node.FnDecl;
    self.scopes.down();

    cfg.append(.{ .set_argc = .{
        .len = n.params.ParamaterList.items.len,
    } });

    for (n.params.ParamaterList.items, 0..) |param, idx| {
        const arg_reg = cfg.reg();
        cfg.append(.{ .load_arg = .{
            .dest = arg_reg,
            .arg = idx,
        } });

        const param_ty = self.emit_expr(cfg, param.Paramater.type);

        const ident = getIdent(param.Paramater.ident);
        const param_reg = self.register_local(cfg, ident);
        cfg.append(.{ .typed_value = .{
            .dest = param_reg,
            .value = arg_reg,
            .ty = param_ty,
        } });
    }

    const return_type = self.emit_expr(cfg, n.ret);
    cfg.append(.{ .set_return_type = .{ .ty = return_type } });

    switch (n.block.*) {
        .Scope => |lst| {
            for (lst.items) |item| {
                self.emit_statement(cfg, item);
            }
        },
        else => unreachable,
    }

    self.scopes.up();
}

fn emit_expr(
    self: *Self,
    cfg: *CFGGen,
    node: AST.NodeRef,
) UIR.Ref {
    return switch (node.*) {
        .Unary => |n| reg: {
            const value = self.emit_expr(cfg, n.node);
            const reg = cfg.reg();
            _ = switch (n.Op.ty) {
                .Minus => cfg.append(.{ .negative = .{ .dest = reg, .value = value } }),
                else => unreachable,
            };
            break :reg reg;
        },
        .Binary => |n| reg: {
            const lhs = self.emit_expr(cfg, n.lhs);
            const rhs = self.emit_expr(cfg, n.rhs);
            const reg = cfg.reg();

            _ = switch (n.Op.ty) {
                .Plus => cfg.append(.{ .add = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .Minus => cfg.append(.{ .sub = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .Asterix => cfg.append(.{ .mul = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .Slash => cfg.append(.{ .div = .{ .dest = reg, .l = lhs, .r = rhs } }),

                .Less => cfg.append(.{ .cmp_lt = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .LessEq => cfg.append(.{ .cmp_le = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .Greater => cfg.append(.{ .cmp_gt = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .GreaterEq => cfg.append(.{ .cmp_ge = .{ .dest = reg, .l = lhs, .r = rhs } }),

                .EqualEqual => cfg.append(.{ .cmp_eq = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .NotEqual => cfg.append(.{ .cmp_ne = .{ .dest = reg, .l = lhs, .r = rhs } }),

                else => unreachable,
            };
            break :reg reg;
        },
        .String => |v| self.emit_as_constant(cfg, .{ .string = v }),
        .Int => |v| self.emit_as_constant(cfg, .{ .int = v }),
        .Float => |v| self.emit_as_constant(cfg, .{ .float = v }),
        .Bool => |v| self.emit_as_constant(cfg, .{ .bool = v }),
        .Ident => |v| reg: {
            const reg = cfg.reg();

            if (UIR.Builtin.fromString(v.text)) |builtin| {
                cfg.append(.{ .load_builtin = .{
                    .dest = reg,
                    .builtin = builtin,
                } });
            } else {
                self.load_variable(cfg, v.text, reg);
            }

            break :reg reg;
        },
        .FnCall => |n| reg: {
            const @"fn" = self.emit_expr(cfg, n.@"fn");

            var args: std.ArrayList(UIR.Ref) = .init(self.allocator);

            for (n.args.items) |arg| {
                args.append(self.emit_expr(cfg, arg)) catch unreachable;
            }

            const reg = cfg.reg();
            cfg.append(.{ .call = .{
                .dest = reg,
                .@"fn" = @"fn",
                .args = args,
            } });

            break :reg reg;
        },
        .FnDecl => |_| reg: {
            var bodycfg = CFGGen.make(self.allocator);

            self.emit_fn_decl(&bodycfg, node);

            const index = self.code.add_decl(.{
                .cfg = bodycfg.graph,
                .mode = .Fn,
            });

            const reg = cfg.reg();
            cfg.append(.{ .load_decl = .{
                .dest = reg,
                .index = index,
            } });

            break :reg reg;
        },
        .Struct => |n| reg: {
            const reg = cfg.reg();
            cfg.append(.{ .create_struct = .{ .dest = reg } });

            for (n.fields.items) |field| _ = switch (field.*) {
                .Field => |f| {
                    const ident = getIdent(f.ident);
                    const ty = self.emit_expr(cfg, f.type orelse unreachable);
                    cfg.append(.{ .append_field = .{
                        .decl = reg,
                        .name = ident,
                        .ty = ty,
                    } });
                },
                .ConstDecl => self.emit_statement(cfg, field),
                else => |f| @panic(@tagName(f)),
            };

            break :reg reg;
        },
        .Comptime => |n| self.emit_as_comptime_block(cfg, n),

        .Scope => |lst| reg: {
            const reg = cfg.reg();
            cfg.push_return_stack(reg);

            const block = cfg.branch(false);

            cfg.set_terminator(.{ .goto = block });
            cfg.set_current(block);

            self.scopes.down();

            for (lst.items) |n| self.emit_statement(cfg, n);

            self.scopes.up();

            const after = cfg.branch(false);
            cfg.set_terminator(.{ .goto = after });
            cfg.set_current(after);

            break :reg reg;
        },

        .If => |n| reg: {
            const cond = self.emit_expr(cfg, n.cond);

            const reg = cfg.reg();
            const inst = cfg.append(.{ .if_expr = .{
                .dest = reg,
                .cond = cond,
                .true_len = 0,
                .false_len = 0,
            } });

            {
                const start = cfg.len();
                self.scopes.down();

                for (n.true.Scope.items) |item| {
                    self.emit_statement(cfg, item);
                }

                self.scopes.up();
                cfg.get(inst).if_expr.true_len = cfg.len() - start;
            }

            {
                const start = cfg.len();
                self.scopes.down();

                for (n.false.Scope.items) |item| {
                    self.emit_statement(cfg, item);
                }

                self.scopes.up();
                cfg.get(inst).if_expr.false_len = cfg.len() - start;
            }

            break :reg reg;
        },

        else => @panic(@tagName(node.*)),
    };
}

pub fn global_pass(_: *Self) void {}

pub fn run(self: *Self) void {
    if (self.ast.root) |root| {
        switch (root.*) {
            .TopLevelScope => |lst| {
                for (lst.items) |n| self.emit_global_statement(n);
            },
            else => unreachable,
        }
    }
}

pub fn print(
    self: *Self,
) !void {
    const writer = std.io.getStdOut().writer();
    try self.code.print(writer);
}

pub const Scopes = struct {
    stack: std.ArrayList(Binding),
    depth: usize = 0,

    pub const Binding = struct {
        name: []const u8,
        depth: usize,
        value: Value,

        pub const Value = union(enum) {
            ref: UIR.Ref,
            decl: UIR.Decl.Index,
        };

        pub const Index = enum(usize) { _ };
    };

    pub fn make(allocator: Allocator) Scopes {
        return Scopes{
            .stack = .init(allocator),
        };
    }

    pub fn deinit(scopes: *Scopes) void {
        scopes.stack.deinit();
    }

    pub fn get(scopes: *Scopes, name: []const u8) ?Binding {
        const len = scopes.stack.items.len;
        if (len == 0) return null;

        var i: i32 = @intCast(len - 1);

        while (i >= 0) : (i -= 1) {
            const local = scopes.stack.items[@intCast(i)];

            if (std.mem.eql(u8, name, local.name)) {
                return local;
            }
        }

        return null;
    }

    pub fn add(
        scopes: *Scopes,
        name: []const u8,
        value: Binding.Value,
    ) Binding.Index {
        const index = scopes.stack.items.len;
        scopes.stack.append(.{
            .name = name,
            .value = value,
            .depth = scopes.depth,
        }) catch unreachable;
        return @enumFromInt(index);
    }

    pub fn down(scopes: *Scopes) void {
        scopes.depth += 1;
    }

    pub fn up(scopes: *Scopes) void {
        scopes.depth -= 1;

        while (scopes.stack.items.len > 0 and
            scopes.stack.getLast().depth > scopes.depth)
        {
            _ = scopes.stack.pop();
        }
    }
};

const CFGGen = struct {
    graph: UIR.CFG,
    register_count: usize = 0,
    current_block: UIR.Loc,
    implicit_return_stack: std.ArrayList(UIR.Ref),

    fn make(allocator: Allocator) CFGGen {
        var graph = UIR.CFG.make(allocator);

        return CFGGen{
            .graph = graph,
            .current_block = graph.append(false),
            .implicit_return_stack = .init(allocator),
        };
    }

    pub fn deinit(gen: *CFGGen) void {
        gen.implicit_return_stack.deinit();
    }

    fn reg(gen: *CFGGen) UIR.Ref {
        const idx = gen.register_count;
        gen.register_count += 1;
        return @enumFromInt(idx);
    }

    fn push_return_stack(gen: *CFGGen, ref: UIR.Ref) void {
        gen.implicit_return_stack.append(ref) catch unreachable;
    }

    fn pop_return_stack(gen: *CFGGen) UIR.Ref {
        return gen.implicit_return_stack.pop();
    }

    fn set_current(gen: *CFGGen, loc: UIR.Loc) void {
        gen.current_block = loc;
    }

    fn get_current(gen: *CFGGen) *UIR.BasicBlock {
        return gen.graph.get(gen.current_block);
    }

    fn branch(gen: *CFGGen, @"comptime": bool) UIR.Loc {
        return gen.graph.append(@"comptime");
    }

    fn set_terminator(gen: *CFGGen, term: UIR.Terminator) void {
        if (gen.get_current().terminator != null) {
            gen.get_current().terminator = term;
        }
    }

    fn append(gen: *CFGGen, inst: UIR.Instruction) void {
        gen.get_current().append(inst);
    }
};
