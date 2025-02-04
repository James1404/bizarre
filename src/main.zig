const std = @import("std");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const UIRGen = @import("uirgen.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const src = try std.fs.cwd().readFileAlloc(
        allocator,
        "test.biz",
        std.math.maxInt(usize),
    );
    defer allocator.free(src);

    var lexer = try Lexer.make(allocator, src);
    defer lexer.deinit();

    const tokens = lexer.run();
    defer tokens.deinit();

    std.debug.print("--- Tokens: len {d}\n", .{tokens.items.len});
    for (tokens.items) |token| {
        std.debug.print("- [{d}, {d}] {s} :: \"{s}\"\n", .{
            token.line,
            token.offset,
            @tagName(token.ty),
            token.text,
        });
    }

    var parser = Parser.make(allocator, tokens);
    defer parser.deinit();

    parser.run();

    parser.ast.print();

    var uirgen = UIRGen.make(allocator, parser.ast);
    defer uirgen.deinit();

    uirgen.run();

    uirgen.print();
}

test {
    std.testing.refAllDecls(@This());
}
