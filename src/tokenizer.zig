const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;

pub const MaxNameLen = 0x40; // 64

pub fn readHexChar(c: u8) !u4 {
    if (std.ascii.isXDigit(c)) {
        if (std.ascii.isDigit(c)) {
            return @truncate(u4, c - '0');
        } else if (std.ascii.isLower(c)) {
            return @truncate(u4, c - 'a') + 10;
        }
    }
    return error.InvalidHexadecimalCharacter;
}
pub fn readHex(comptime T: type, data: []const u8) !T {
    const info = @typeInfo(T);
    assert(info == .Int and (info.Int.bits) % 4 == 0);
    const MaxLength = info.Int.bits / 4;
    const Index = std.math.IntFittingRange(0, MaxLength);
    const ShiftInt = std.math.Log2Int(T);

    if (data.len > MaxLength) return error.HexadecimalTooLong;
    var value: T = 0;
    const len = @intCast(Index, data.len);
    var i: Index = len;
    while (i > 0) {
        i -= 1;
        const next = try readHexChar(data[i]);
        value |= @as(T, next) << ((@intCast(ShiftInt, len - i) - 1) * 4);
    }
    return value;
}

pub const Op = enum(u5) {
    LIT = 0x00,
    INC = 0x01,
    POP = 0x02,
    NIP = 0x03,
    SWP = 0x04,
    ROT = 0x05,
    DUP = 0x06,
    OVR = 0x07,
    EQU = 0x08,
    NEQ = 0x09,
    GTH = 0x0a,
    LTH = 0x0b,
    JMP = 0x0c,
    JCN = 0x0d,
    JSR = 0x0e,
    STH = 0x0f,
    LDZ = 0x10,
    STZ = 0x11,
    LDR = 0x12,
    STR = 0x13,
    LDA = 0x14,
    STA = 0x15,
    DEI = 0x16,
    DEO = 0x17,
    ADD = 0x18,
    SUB = 0x19,
    MUL = 0x1a,
    DIV = 0x1b,
    AND = 0x1c,
    ORA = 0x1d,
    EOR = 0x1e,
    SFT = 0x1f,
};

pub const Opcode = packed struct {
    op: Op,
    short: bool = false,
    ret: bool = false,
    keep: bool = false,

    pub fn fromString(text: []const u8) !Opcode {
        if (text.len < 3) return error.OpcodeTooShort;
        var opcode: Opcode = .{
            .op = std.meta.stringToEnum(Op, text[0..3]) orelse {
                if (std.mem.eql(u8, text, "BRK")) {
                    return Opcode{ .op = .LIT };
                } else {
                    return error.UnknownOpcode;
                }
            },
        };
        if (opcode.op == .LIT) opcode.keep = true;
        for (text[3..]) |c| switch (c) {
            '2' => opcode.short = true,
            'r' => opcode.ret = true,
            'k' => opcode.keep = true,
            else => return error.UnknownOpcode,
        };
        return opcode;
    }
    pub fn format(
        value: Opcode,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        if (value.op == .LIT and !value.short and !value.ret and !value.keep) {
            try writer.writeAll("BRK");
        } else {
            try writer.writeAll(@tagName(value.op));
            if (value.short) try writer.writeByte('2');
            if (value.keep) try writer.writeByte('k');
            if (value.ret) try writer.writeByte('r');
        }
    }
};
pub const Tokenizer = struct {
    buffer: std.BoundedArray(u8, MaxNameLen) = .{},
    last_line_number: usize = 1,
    last_column_number: usize = 0,
    line_number: usize = 1,
    column_number: usize = 0,

    /// Any value with a string is backed by the internal buffer
    /// and will be overwritten the next time `nextItem` is called
    pub fn nextItem(self: *Tokenizer, reader: anytype) !?[]const u8 {
        // we basically have the equivalent to `Reader.readUntilDelimiter` here, but
        // there are multiple possible delimiters.
        self.buffer = .{};
        var next_byte: u8 = undefined;
        while (true) {
            next_byte = reader.readByte() catch |e| {
                if (e == error.EndOfStream) return null else return e;
            };
            self.column_number += 1;
            if (!std.ascii.isSpace(next_byte)) {
                if (next_byte == '(') {
                    var comment_level: usize = 1;
                    while (comment_level > 0) {
                        next_byte = reader.readByte() catch |e| {
                            if (e == error.EndOfStream) return null else return e;
                        };
                        switch (next_byte) {
                            '(' => comment_level += 1,
                            ')' => comment_level -= 1,
                            '\n' => {
                                self.line_number += 1;
                                self.column_number = 0;
                            },
                            else => {},
                        }
                    }
                } else {
                    self.buffer.appendAssumeCapacity(next_byte);
                    break;
                }
            } else if (next_byte == '\n') {
                self.line_number += 1;
                self.column_number = 0;
            }
        }
        self.last_line_number = self.line_number;
        self.last_column_number = self.column_number;
        while (true) {
            next_byte = reader.readByte() catch |e| {
                if (e == error.EndOfStream) break else return e;
            };
            self.column_number += 1;
            if (std.ascii.isSpace(next_byte)) {
                if (next_byte == '\n') {
                    self.line_number += 1;
                    self.column_number = 0;
                }
                break;
            }
            try self.buffer.append(next_byte);
        }
        return self.buffer.slice();
    }

    pub const Token = union(enum) {
        include: []const u8,
        macro_definition: []const u8,
        macro_begin: void,
        macro_end: void,
        pad_absolute: u16,
        pad_relative: u16,
        label: []const u8,
        sublabel: []const u8,
        literal_hex_byte: u8,
        literal_hex_short: u16,
        literal_zeropage_byte: []const u8,
        literal_relative_byte: []const u8,
        literal_absolute_short: []const u8,
        raw_absolute_short: []const u8,
        raw_string: []const u8,
        raw_byte: u8,
        raw_short: u16,
        opcode: Opcode,
        macro_call: []const u8,
        bracket: void,

        pub fn cloneWithPool(token: Token, pool: anytype) !Token {
            return switch (token) {
                .include => |val| Token{ .include = try pool.dupe(val) },
                .macro_definition => |val| Token{ .macro_definition = try pool.dupe(val) },
                .label => |val| Token{ .label = try pool.dupe(val) },
                .sublabel => |val| Token{ .sublabel = try pool.dupe(val) },
                .literal_zeropage_byte => |val| Token{ .literal_zeropage_byte = try pool.dupe(val) },
                .literal_relative_byte => |val| Token{ .literal_relative_byte = try pool.dupe(val) },
                .literal_absolute_short => |val| Token{ .literal_absolute_short = try pool.dupe(val) },
                .raw_absolute_short => |val| Token{ .raw_absolute_short = try pool.dupe(val) },
                .raw_string => |val| Token{ .raw_string = try pool.dupe(val) },
                .macro_call => |val| Token{ .macro_call = try pool.dupe(val) },
                else => token,
            };
        }
        pub fn clone(token: Token, alloc: std.mem.Allocator) !Token {
            return switch (token) {
                .include => |val| Token{ .include = try alloc.dupe(u8, val) },
                .macro_definition => |val| Token{ .macro_definition = try alloc.dupe(u8, val) },
                .label => |val| Token{ .label = try alloc.dupe(u8, val) },
                .sublabel => |val| Token{ .sublabel = try alloc.dupe(u8, val) },
                .literal_zeropage_byte => |val| Token{ .literal_zeropage_byte = try alloc.dupe(u8, val) },
                .literal_relative_byte => |val| Token{ .literal_relative_byte = try alloc.dupe(u8, val) },
                .literal_absolute_short => |val| Token{ .literal_absolute_short = try alloc.dupe(u8, val) },
                .raw_absolute_short => |val| Token{ .raw_absolute_short = try alloc.dupe(u8, val) },
                .raw_string => |val| Token{ .raw_string = try alloc.dupe(u8, val) },
                .macro_call => |val| Token{ .macro_call = try alloc.dupe(u8, val) },
                else => token,
            };
        }
        pub fn format(
            value: Token,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            //_ = fmt;
            //_ = options;
            const info = @typeInfo(Token).Union;
            const TagType = info.tag_type.?;
            const tag = @as(TagType, value);
            try writer.writeAll(@tagName(tag));
            try writer.writeAll("{");
            inline for (info.fields) |field| {
                if (@field(TagType, field.name) == tag) {
                    const data = @field(value, field.name);
                    switch (field.field_type) {
                        []const u8 => try writer.print("\"{s}\"", .{data}),
                        u8 => try writer.print("{x:0>2}", .{data}),
                        u16 => try writer.print("{x:0>4}", .{data}),
                        void => {},
                        Opcode => try Opcode.format(data, fmt, options, writer),
                        else => unreachable,
                    }
                }
            }
            try writer.writeAll("}");
        }
    };
    pub fn nextToken(self: *Tokenizer, reader: anytype) !?Token {
        const item = (try self.nextItem(reader)) orelse return null;
        assert(item.len > 0);
        switch (item[0]) {
            '~' => return Token{ .include = item[1..] },
            '%' => return Token{ .macro_definition = item[1..] },
            '{' => return Token.macro_begin,
            '}' => return Token.macro_end,
            '|' => return Token{ .pad_absolute = try readHex(u16, item[1..]) },
            '$' => return Token{ .pad_relative = try readHex(u16, item[1..]) },
            '@' => return Token{ .label = item[1..] },
            '&' => return Token{ .sublabel = item[1..] },
            '#' => {
                if (item.len == 1) return error.InvalidToken;
                const hex_text = item[1..];
                if (hex_text.len <= 2) {
                    return Token{ .literal_hex_byte = try readHex(u8, hex_text) };
                } else {
                    return Token{ .literal_hex_short = try readHex(u16, hex_text) };
                }
            },
            '.' => return Token{ .literal_zeropage_byte = item[1..] },
            ',' => return Token{ .literal_relative_byte = item[1..] },
            ';' => return Token{ .literal_absolute_short = item[1..] },
            ':' => return Token{ .raw_absolute_short = item[1..] },
            '\'' => {
                if (item.len != 2) return error.InvalidToken;
                return Token{ .raw_byte = item[1] };
            },
            '"' => return Token{ .raw_string = item[1..] },
            else => {
                if (item[0] == '[' or item[0] == ']') {
                    if (item.len == 1) return Token.bracket;
                }
                if (readHex(u8, item)) |value| {
                    return Token{ .raw_byte = value };
                } else |e| {
                    if (e == error.HexadecimalTooLong) {
                        if (readHex(u16, item)) |value| {
                            return Token{ .raw_short = value };
                        } else |_| {}
                    }
                }
                if (Opcode.fromString(item)) |opcode| {
                    return Token{ .opcode = opcode };
                } else |_| {}
                return Token{ .macro_call = item };
            },
        }
    }
};

test "hex parse" {
    try testing.expectEqual(try readHex(u16, "0100"), 256);
    try testing.expectEqual(try readHex(u16, "100"), 256);
    try testing.expectEqual(try readHex(u16, "1"), 1);
    try testing.expectEqual(try readHex(u16, "FF"), 255);
}

test "tokenizer" {
    {
        var tokens = Tokenizer{};
        const short_text = ",&byte";
        var stream = std.io.fixedBufferStream(short_text);
        const result = (try tokens.nextToken(stream.reader())).?;
        try testing.expectEqualSlices(
            u8,
            "&byte",
            result.literal_relative_byte,
        );
    }
    {
        var tokens = Tokenizer{};
        const long_text =
            \\( this is a comment )
            \\|10 @Console &vector $2 &read $1 &pad $5 &write $1 &error $1
            \\
            \\|0100 ( -> )
            \\    ;on-console .Console/vector DEO2
            \\BRK
            \\
            \\@print ( short* -- )
            \\
            \\    SWP ,&byte JSR
            \\    &byte ( byte -- ) DUP #04 SFT ,&char JSR
            \\    &char ( char -- ) #0f AND DUP #09 GTH #27 MUL ADD #30 ADD #18 DEO
            \\
            \\JMP2r
            \\
        ;
        var stream = std.io.fixedBufferStream(long_text);
        while (try tokens.nextToken(stream.reader())) |token| {
            std.debug.print("{any}\n", .{token});
        }
    }
}
