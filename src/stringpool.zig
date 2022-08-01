const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

pub fn StringPool(comptime buffer_size: usize, comptime max_string_length: usize) type {
    return struct {
        pub const Index = std.math.IntFittingRange(0, buffer_size - 1);
        pub const StringLength = std.math.IntFittingRange(0, max_string_length - 1);

        buffer: [buffer_size]u8 = undefined,
        len: usize = 0,

        const Self = @This();
        pub fn dupe(self: *Self, text: []const u8) ![]const u8 {
            return self.get(try self.insert(text));
        }
        pub fn insert(self: *Self, text: []const u8) !Index {
            var iter = Iterator{};
            while (iter.next(self)) |index| {
                if (std.mem.eql(u8, self.get(index), text)) {
                    return index;
                }
            }
            return try self.insertUnique(text);
        }
        pub fn insertUnique(self: *Self, text: []const u8) !Index {
            if (text.len > max_string_length) return error.StringTooLong;
            const new_len = self.len + @sizeOf(StringLength) + text.len;
            if (new_len >= buffer_size) {
                return error.OutOfMemory;
            }
            std.mem.writeIntNative(
                StringLength,
                self.buffer[self.len..][0..@sizeOf(StringLength)],
                @intCast(StringLength, text.len),
            );
            std.mem.copy(u8, self.buffer[self.len + @sizeOf(StringLength) ..][0..text.len], text);
            const old_len = self.len;
            self.len = new_len;
            return @intCast(Index, old_len);
        }
        pub fn get(self: *Self, index: Index) []const u8 {
            const len = std.mem.readIntNative(
                StringLength,
                self.buffer[index..][0..@sizeOf(StringLength)],
            );
            return self.buffer[index + @sizeOf(StringLength) ..][0..len];
        }

        pub const Iterator = struct {
            index: usize = 0,

            pub fn next(iter: *Iterator, pool: *Self) ?Index {
                if (iter.index >= pool.len) return null;
                const old_index = iter.index;
                iter.index += std.mem.readIntNative(
                    StringLength,
                    pool.buffer[old_index..][0..@sizeOf(StringLength)],
                ) + @sizeOf(StringLength);
                return @intCast(Index, old_index);
            }
        };
    };
}
