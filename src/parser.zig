const std = @import("std");

const AST = @import("ast.zig");
const Token = @import("token.zig");

allocator: std.mem.Allocator,

src: Token.List,
pos: usize,
ast: AST,

const Self = @This();

pub fn make(
    allocator: std.mem.Allocator,
    src: Token.List,
) Self {
    return Self{
        .allocator = allocator,

        .src = src,
        .pos = 0,
        .ast = .make(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.ast.deinit();
}

fn alloc(self: *Self, node: AST.Node) AST.NodeRef {
    switch (node) {
        .Error => |err| {
            std.debug.panic("Error [{d}, {d}] \"{s}\": {s}", .{
                err.token.line,
                err.token.offset,
                err.token.text,
                err.msg,
            });
        },
        else => {},
    }

    return self.ast.alloc(node);
}

fn eof(self: Self) bool {
    return self.pos >= self.src.items.len;
}

fn advance(self: *Self) void {
    self.pos += 1;
}

fn getCurrent(self: *Self) Token {
    return self.src.items[self.pos];
}

fn getNext(self: *Self) ?Token {
    const idx = self.pos + 1;
    return if (idx > self.src.items.len)
        null
    else
        self.src.items[idx];
}

fn match(self: *Self, expected: Token.Ty) bool {
    return self.getCurrent().ty == expected;
}

fn matchNext(self: *Self, expected: Token.Ty) bool {
    const next = self.getNext();
    return if (next) |tok| tok.ty == expected else false;
}

fn advanceIf(self: *Self, expected: Token.Ty) ?Token {
    if (self.match(expected)) {
        const tok = self.getCurrent();
        self.advance();
        return tok;
    }

    return null;
}

// fn parse_if(self: *Self) AST.NodeRef {
//     const start = self.getCurrent();

fn parse_fnDecl(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.Fn)) |_| {
        if (self.advanceIf(.LParen)) |_| {
            var params = std.ArrayList(AST.NodeRef).init(self.allocator);

            while (true) {
                if (self.match(.RParen)) break;

                const ident = self.parse_ident();

                if (self.advanceIf(.Colon) == null) return self.alloc(.{ .Error = .{
                    .msg = "Argument needs a type with ':'",
                    .token = start,
                } });

                const @"type" = self.parse_expr();

                params.append(self.alloc(.{ .Paramater = .{
                    .ident = ident,
                    .type = @"type",
                } })) catch |err| {
                    @panic(@errorName(err));
                };

                if (self.advanceIf(.Colon) == null) break;
            }

            if (self.advanceIf(.RParen) == null) {
                return self.alloc(.{ .Error = .{
                    .msg = "Function paramater list requires closing brace",
                    .token = start,
                } });
            }

            const ret = self.parse_expr();

            return if (self.match(.LBrace)) node: {
                const block = self.parse_scope();

                break :node self.alloc(.{ .FnDecl = .{
                    .params = self.alloc(.{
                        .ParamaterList = params,
                    }),
                    .ret = ret,
                    .block = block,
                } });
            } else self.alloc(.{ .FnType = .{
                .params = self.alloc(.{ .ParamaterList = params }),
                .ret = ret,
            } });
        }
    }

    return self.alloc(.{ .Error = .{
        .msg = "Couldnt parse function decleration",
        .token = start,
    } });
}

fn parse_ident(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (start.ty == .Ident) {
        self.advance();

        return self.alloc(.{
            .Ident = start,
        });
    }

    return self.alloc(.{ .Error = .{
        .msg = "Unable to parse identifier",
        .token = start,
    } });
}

fn advanceAlloc(self: *Self, node: AST.Node) AST.NodeRef {
    self.advance();
    return self.alloc(node);
}

fn parse_value(self: *Self) AST.NodeRef {
    const tok = self.getCurrent();

    // TODO: Implement unary operators
    // TODO: Implement array indexing
    // TODO: Implement ranges [0..12]
    // TODO: Implement pointer types *i32
    // TODO: Implement referencing and dereferencing
    // e.g. 25.& get pointer,
    // const x = malloc(i32);
    // x.* = 13;

    const value = switch (tok.ty) {
        .String => self.advanceAlloc(.{ .String = tok.text }),
        .Float => self.advanceAlloc(.{ .Float = tok.text }),
        .Int => self.advanceAlloc(.{ .Int = tok.text }),
        .True => self.advanceAlloc(.{ .Bool = true }),
        .False => self.advanceAlloc(.{ .Bool = false }),
        .Ident => self.parse_ident(),
        .Fn => self.parse_fnDecl(),
        .If => self.parse_if(),
        .Comptime => node: {
            self.advance();
            break :node self.alloc(.{ .Comptime = self.parse_expr() });
        },
        .Extern => node: {
            self.advance();
            break :node self.alloc(.{ .Extern = self.parse_expr() });
        },
        .Struct => self.parse_struct(),
        .Distinct => node: {
            self.advance();
            break :node undefined; // TODO
            //break :node self.alloc(.{ .Distinct = self.parse_expr() });
        },
        .Asterix => node: {
            self.advance();
            break :node self.alloc(.{ .PtrType = self.parse_expr() });
        },

        .LParen => node: {
            self.advance();

            const expr = self.parse_expr();
            if (self.advanceIf(.RParen)) |_| {
                break :node expr;
            }

            break :node self.alloc(.{ .Error = .{
                .msg = "'(' Needs a closing ')'",
                .token = tok,
            } });
        },
        .LBrace => self.parse_scope(),
        else => return self.advanceAlloc(.{ .Error = .{
            .msg = "Unable to parse value",
            .token = tok,
        } }),
    };

    if (self.advanceIf(.LParen)) |_| {
        var args = std.ArrayList(AST.NodeRef).init(self.allocator);

        while (true) {
            if (self.match(.RParen)) break;

            const arg = self.parse_expr();

            args.append(arg) catch |err| {
                @panic(@errorName(err));
            };

            if (self.advanceIf(.Colon) == null) break;
        }

        if (self.advanceIf(.RParen) == null) {
            return self.alloc(.{ .Error = .{
                .msg = "Function call requires closing brace",
                .token = tok,
            } });
        }

        return self.alloc(.{ .FnCall = .{
            .@"fn" = value,
            .args = args,
        } });
    }

    return value;
}

fn expression(
    self: *Self,
    lhs: AST.NodeRef,
    min_precedence: i32,
) AST.NodeRef {
    var final = lhs;
    var current = self.getCurrent();

    while (Token.precendence(current) >= min_precedence) {
        const op = current;
        self.advance();

        var rhs = self.parse_value();

        current = self.getCurrent();

        while (Token.precendence(current) > Token.precendence(op)) {
            rhs = self.expression(
                rhs,
                Token.precendence(op) + @as(i32, if (Token.precendence(current) > Token.precendence(op))
                    1
                else
                    0),
            );
        }

        final = self.alloc(.{ .Binary = .{
            .lhs = final,
            .Op = op,
            .rhs = rhs,
        } });
    }
    return final;
}

fn parse_expr(self: *Self) AST.NodeRef {
    return self.expression(self.parse_value(), 0);
}

fn parse_field(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    const ident = self.parse_ident();
    const @"type": ?AST.NodeRef = if (self.advanceIf(.Colon)) |_|
        self.parse_value()
    else
        null;

    const default: ?AST.NodeRef = if (self.advanceIf(.Equal)) |_| node: {
        break :node self.parse_value();
    } else node: {
        if (@"type" == null) {
            return self.alloc(.{ .Error = .{
                .msg = "Field requires a default value or type signature",
                .token = start,
            } });
        }

        break :node null;
    };

    return self.alloc(.{ .Field = .{
        .ident = ident,
        .type = @"type",
        .default = default,
    } });
}

fn parse_interface(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.Interface)) |_| {
        const ident = self.parse_ident();

        if (self.advanceIf(.LBrace) == null) {
            return self.alloc(.{ .Error = .{
                .msg = "Interface expects braces",
                .token = start,
            } });
        }

        var fields = std.ArrayList(AST.NodeRef).init(self.allocator);

        while (self.advanceIf(.RBrace) == null) {
            const item = switch (self.getCurrent().ty) {
                .Fn => self.parse_fn_stmt(),
                .Const => self.parse_const(),
                .Var => self.parse_var(),
                else => self.parse_field(),
            };

            fields.append(item) catch unreachable;

            if (self.advanceIf(.Semicolon) == null) {
                return self.alloc(.{ .Error = .{
                    .msg = "Each field must end with a semicolon",
                    .token = start,
                } });
            }
        }

        return self.alloc(.{ .Interface = .{
            .ident = ident,
            .fields = fields,
        } });
    }

    return self.alloc(.{ .Error = .{
        .msg = "Unable to parse interface",
        .token = start,
    } });
}

fn parse_struct(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.Struct)) |_| {
        if (self.advanceIf(.LBrace) == null) {
            return self.alloc(.{ .Error = .{
                .msg = "Interface expects braces",
                .token = start,
            } });
        }

        var fields = std.ArrayList(AST.NodeRef).init(self.allocator);

        while (self.advanceIf(.RBrace) == null) {
            const item = switch (self.getCurrent().ty) {
                .Fn => self.parse_fn_stmt(),
                .Const => self.parse_const(),
                .Var => self.parse_var(),
                else => self.parse_field(),
            };

            fields.append(item) catch unreachable;

            if (self.advanceIf(.Semicolon) == null) {
                return self.alloc(.{ .Error = .{
                    .msg = "Each field must end with a semicolon",
                    .token = start,
                } });
            }
        }

        return self.alloc(.{ .Struct = .{
            .fields = fields,
        } });
    }

    return self.alloc(.{ .Error = .{
        .msg = "Unable to parse struct",
        .token = start,
    } });
}

fn parse_if(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.If)) |_| {
        const cond = self.parse_expr();
        const @"true" = self.parse_scope();

        if (self.advanceIf(.Else)) |_| {
            const @"false" = self.parse_scope();

            return self.alloc(.{ .If = .{
                .cond = cond,
                .false = @"false",
                .true = @"true",
            } });
        }
    }

    return self.alloc(.{ .Error = .{
        .msg = "Failed to parse if statement",
        .token = start,
    } });
}

fn parse_const(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.Const)) |_| {
        const ident = self.parse_ident();
        const @"type": ?AST.NodeRef = if (self.advanceIf(.Colon)) |_|
            self.parse_expr()
        else
            null;

        if (self.advanceIf(.Equal)) |_| {
            const value = self.parse_expr();

            return self.alloc(.{ .ConstDecl = .{
                .ident = ident,
                .type = @"type",
                .value = value,
            } });
        }
    }

    return self.alloc(.{ .Error = .{
        .msg = "Failed to parse const statement",
        .token = start,
    } });
}

fn parse_var(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.Var)) |_| {
        const ident = self.parse_ident();
        const @"type": ?AST.NodeRef = if (self.advanceIf(.Colon)) |_|
            self.parse_expr()
        else
            null;

        if (self.advanceIf(.Equal)) |_| {
            const value = self.parse_expr();

            return self.alloc(.{ .VarDecl = .{
                .ident = ident,
                .type = @"type",
                .value = value,
            } });
        }
    }

    return self.alloc(.{ .Error = .{
        .msg = "Failed to parse var statement",
        .token = start,
    } });
}

fn parse_fn_stmt(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.Fn)) |_| {
        const ident = self.parse_ident();

        if (self.advanceIf(.LParen)) |_| {
            var params = std.ArrayList(AST.NodeRef).init(self.allocator);

            while (true) {
                if (self.match(.RParen)) break;

                const param_ident = self.parse_ident();

                if (self.advanceIf(.Colon) == null) return self.alloc(.{ .Error = .{
                    .msg = "Argument needs a type with ':'",
                    .token = start,
                } });

                const @"type" = self.parse_expr();

                params.append(self.alloc(.{ .Paramater = .{
                    .ident = param_ident,
                    .type = @"type",
                } })) catch |err| {
                    @panic(@errorName(err));
                };

                if (self.advanceIf(.Colon) == null) break;
            }

            if (self.advanceIf(.RParen) == null) {
                return self.alloc(.{ .Error = .{
                    .msg = "Function paramater list requires closing brace",
                    .token = start,
                } });
            }

            const ret = self.parse_expr();

            if (self.match(.LBrace)) {
                const block = self.parse_scope();

                const @"fn" = self.alloc(.{ .FnDecl = .{
                    .params = self.alloc(.{
                        .ParamaterList = params,
                    }),
                    .ret = ret,
                    .block = block,
                } });

                return self.alloc(.{ .ConstDecl = .{
                    .ident = ident,
                    .type = null,
                    .value = @"fn",
                } });
            } else {
                const @"fn" = self.alloc(.{ .FnType = .{
                    .params = self.alloc(.{ .ParamaterList = params }),
                    .ret = ret,
                } });

                return self.alloc(.{ .ConstDecl = .{
                    .ident = ident,
                    .type = null,
                    .value = @"fn",
                } });
            }
        }
    }

    return self.alloc(.{ .Error = .{
        .msg = "Failed to parse fn statement",
        .token = start,
    } });
}

fn parse_stmt(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    return switch (start.ty) {
        .Const => self.parse_const(),
        .Var => self.parse_var(),
        .Interface => self.parse_interface(),
        .Return => node: {
            self.advance(); // TODO: Breaks if not returning value
            break :node self.alloc(.{
                .Return = self.parse_expr(),
            });
        },
        .Defer => node: {
            self.advance();
            break :node self.alloc(.{
                .Defer = self.parse_expr(),
            });
        },
        .Fn => self.parse_fn_stmt(),
        else => if (self.match(.Ident) and self.matchNext(.Equal)) node: {
            const ident = self.parse_ident();
            self.advance();

            const value = self.parse_expr();

            break :node self.alloc(.{ .Assignment = .{
                .ident = ident,
                .value = value,
            } });
        } else self.parse_expr(),
    };
}

fn parse_toplevel_stmt(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    return switch (start.ty) {
        .Const => self.parse_const(),
        .Var => self.parse_var(),
        .Interface => self.parse_interface(),
        .Comptime => node: {
            self.advance();
            break :node self.alloc(.{
                .Comptime = self.parse_expr(),
            });
        },
        .Fn => self.parse_fn_stmt(),
        else => self.alloc(.{ .Error = .{
            .msg = "Invalid toplevel statement",
            .token = start,
        } }),
    };
}

fn parse_scope(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.LBrace) == null) {
        return self.alloc(.{ .Error = .{
            .msg = "Block needs opening brace",
            .token = start,
        } });
    }

    var list = std.ArrayList(AST.NodeRef).init(self.allocator);
    while (!self.match(.RBrace)) {
        const stmt = self.parse_stmt();

        if (self.advanceIf(.Semicolon) == null) {
            list.append(self.alloc(.{ .ImplicitReturn = stmt })) catch |err| {
                @panic(@errorName(err));
            };
            break;
        } else {
            list.append(stmt) catch |err| {
                @panic(@errorName(err));
            };
        }
    }

    if (self.advanceIf(.RBrace) == null) {
        return self.alloc(.{ .Error = .{
            .msg = "Expect closing brace at end of statement",
            .token = start,
        } });
    }

    return self.alloc(.{ .Scope = list });
}

fn parse_toplevel(self: *Self) AST.NodeRef {
    var list = std.ArrayList(AST.NodeRef).init(self.allocator);
    while (!self.eof()) {
        const start = self.getCurrent();

        list.append(self.parse_toplevel_stmt()) catch |err| {
            @panic(@errorName(err));
        };

        if (self.advanceIf(.Semicolon) == null) {
            return self.alloc(.{ .Error = .{
                .msg = "Expect semicolon after each statement",
                .token = start,
            } });
        }
    }

    return self.alloc(.{ .TopLevelScope = list });
}

pub fn run(self: *Self) void {
    self.ast.root = self.parse_toplevel();
}
