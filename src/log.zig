const std = @import("std");

const debug = true;

pub fn info(comptime fmt: []const u8, args: anytype) void {
    if (debug) std.log.info(fmt, args);
}

pub fn warn(comptime fmt: []const u8, args: anytype) void {
    if (debug) std.log.warn(fmt, args);
}

pub fn err(comptime fmt: []const u8, args: anytype) void {
    if (debug) std.log.err(fmt, args);
}
