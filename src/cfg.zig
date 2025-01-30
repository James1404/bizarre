const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn create(comptime Inst: type) type {
    return struct {
        const CFG = @This();

        const Constants = @import("constants.zig");

        pub const Ref = enum(usize) {
            @"return" = 0,
            _,
        };

        pub const Terminator = union(enum) {
            @"if": struct { cond: Ref, true: Location, false: Location },
            @"switch": struct {
                value: Ref,
                patterns: std.ArrayList(Constants.Index),
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
            ret: Constants.Index,
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

            pub fn emit_temp_value(self: *@This(), value: Constants.Index.Data) void {
                const addr = self.temp();
                self.append(.{ .set = .{
                    .addr = addr,
                    .value = .make_untyped(value),
                } });
                return addr;
            }
        };

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
}
