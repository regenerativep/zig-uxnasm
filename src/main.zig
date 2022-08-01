const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const tokenizer = @import("tokenizer.zig");
const Tokenizer = tokenizer.Tokenizer;
const Opcode = tokenizer.Opcode;
const MaxNameLen = tokenizer.MaxNameLen;
const StringPool = @import("stringpool.zig").StringPool;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var alloc = gpa.allocator();

    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();
    _ = args.skip();
    const input_filename = args.next() orelse return std.log.err("no input file specified", .{});
    const output_filename = args.next() orelse blk: {
        std.log.info("no output filename specified, outputting to \"output.rom\"", .{});
        break :blk "output.rom";
    };

    const rom = assemble(alloc, input_filename) catch {
        return std.log.err("Assembler failure", .{});
    };
    defer alloc.free(rom);
    var output_file = try std.fs.cwd().createFile(output_filename, .{});
    defer output_file.close();
    try output_file.writeAll(rom);
    std.log.info("assembled \"{s}\". {} bytes written", .{ output_filename, rom.len });
}

const Pool = StringPool(0x100 * 0x100, 255); // 64KiB

const IncludedFile = struct {
    const Reader = std.io.BufferedReader(512, std.fs.File.Reader);
    filename: Pool.Index,
    file: std.fs.File,
    reader: Reader,
    tokenizer: Tokenizer = .{},

    const Self = @This();
    pub fn init(filename: []const u8, pool: *Pool) !Self {
        const index = try pool.insert(filename);
        var file = try std.fs.cwd().openFile(pool.get(index), .{});
        return Self{
            .file = file,
            .reader = .{ .unbuffered_reader = file.reader() },
            .filename = index,
        };
    }
    pub fn deinit(self: *Self) void {
        self.file.close();
        self.* = undefined;
    }
};
const TokenList = std.TailQueue(Tokenizer.Token);
const Macro = struct {
    name: Pool.Index,
    tokens: TokenList = .{},
};
const Output = struct {
    const ProgramBegin = 0x100;
    const RomSize = 0x10000;

    buffer: [RomSize]u8 = [_]u8{0} ** RomSize,
    ptr: u17 = 0,
    written: std.StaticBitSet(RomSize) = std.StaticBitSet(RomSize).initEmpty(),
    lit_last: bool = false,
    jsr_last: u2 = 0,

    pub fn writeByte(self: *Output, b: u8) !void {
        if (self.ptr < ProgramBegin) return error.WriteInZeropage;
        if (self.ptr >= RomSize) return error.WriteOutsideRom;
        if (self.written.isSet(self.ptr)) return error.Overwrite;
        self.buffer[self.ptr] = b;
        self.written.set(self.ptr);
        self.ptr += 1;
        self.lit_last = false;
        self.jsr_last = 0;
    }
    pub fn writeShort(self: *Output, s: u16, comptime is_literal: bool) !void {
        if (is_literal) try self.writeByte(@bitCast(u8, Opcode{ .op = .LIT, .short = true, .keep = true }));
        try self.writeByte(@truncate(u8, s >> 8));
        try self.writeByte(@truncate(u8, s));
    }
    pub fn writeByteLiteral(self: *Output, b: u8) !void {
        if (self.lit_last) {
            // literal optimization thing
            // https://git.sr.ht/~rabbits/uxn/tree/main/item/src/uxnasm.c#L243
            self.ptr -= 2;
            self.written.setRangeValue(.{ .start = self.ptr, .end = self.ptr + 2 }, false);
            try self.writeShort((@as(u16, self.buffer[self.ptr + 1]) << 8) | b, true);
        } else {
            try self.writeByte(@bitCast(u8, Opcode{ .op = .LIT, .keep = true }));
            try self.writeByte(b);
            self.lit_last = true;
        }
    }
    pub fn writeOpcode(self: *Output, opcode: Opcode) !void {
        if (self.jsr_last != 0 and
            opcode.op == .JMP and
            opcode.short and
            opcode.ret)
        {
            // tail call optimization thing
            // https://git.sr.ht/~rabbits/uxn/tree/main/item/src/uxnasm.c#L219
            self.buffer[self.ptr - 1] = @bitCast(u8, Opcode{
                .op = .JMP,
                .short = self.jsr_last == 2,
            });
            self.jsr_last = 0;
        } else {
            try self.writeByte(@bitCast(u8, opcode));
            if (opcode.op == .JMP) {
                self.jsr_last = @as(u2, 1) + @boolToInt(opcode.short);
            }
        }
    }
};

const NameArray = std.BoundedArray(u8, MaxNameLen);

pub fn sublabel(buf: []u8, name: []const u8, scope: []const u8) []const u8 {
    const combined_len = name.len + scope.len + 1;
    std.mem.copy(u8, buf, scope);
    buf[scope.len] = '/';
    std.mem.copy(u8, buf[scope.len + 1 .. combined_len], name);
    return buf[0..combined_len];
}
pub fn fullLabel(buf: []u8, name: []const u8, scope: []const u8) ![]const u8 {
    if (name.len == 0) return error.InvalidName;
    if (name[0] != '&') return name;
    const combined_len = name[1..].len + scope.len + 1;
    if (buf.len < combined_len) return error.NameTooLarge;
    return sublabel(buf, name[1..], scope);
}

test "full label" {
    var buf: [64]u8 = undefined;
    try testing.expectEqualSlices(u8, "ghjk/asdf", try fullLabel(&buf, "&asdf", "ghjk"));
    try testing.expectEqualSlices(u8, "asdf", try fullLabel(&buf, "asdf", "ghjk"));
}

const Reference = struct {
    pub const Rune = enum {
        zeropage_byte,
        relative_byte,
        literal_short,
        raw_short,
    };
    name: Pool.Index,
    address: u16,
    rune: Rune,
    column_number: usize,
    line_number: usize,
    filename: Pool.Index,
};
pub fn makeReference(
    pool: *Pool,
    references: *std.ArrayList(Reference),
    ptr: u17,
    rune: Reference.Rune,
    name: []const u8,
    scope: []const u8,
    included_file: *IncludedFile,
) !void {
    var buf: [MaxNameLen]u8 = undefined;
    const label_index = try pool.insert(try fullLabel(&buf, name, scope));
    try references.append(Reference{
        .name = label_index,
        .address = @intCast(u16, ptr),
        .rune = rune,
        .filename = included_file.filename,
        .column_number = included_file.tokenizer.last_column_number,
        .line_number = included_file.tokenizer.last_line_number,
    });
}

pub fn freeMacro(tokens: TokenList, alloc: Allocator) void {
    var current_node: ?*TokenList.Node = tokens.first;
    while (current_node) |node| {
        const next_node = node.next;
        defer current_node = next_node;
        alloc.destroy(node);
    }
}

pub fn assemble(gpa: Allocator, main_filename: []const u8) ![]u8 {
    var pool = Pool{};

    var includes = std.BoundedArray(IncludedFile, 16){};
    defer {
        var i: usize = includes.len;
        while (i > 0) {
            i -= 1;
            includes.buffer[i].deinit();
        }
    }
    includes.appendAssumeCapacity(try IncludedFile.init(main_filename, &pool));
    std.log.info("opened file {s}", .{main_filename});

    var macros = std.StringHashMap(TokenList).init(gpa);
    defer {
        var iter = macros.iterator();
        while (iter.next()) |tokens| freeMacro(tokens.value_ptr.*, gpa);
        macros.deinit();
    }

    var current_scope: []const u8 = try pool.dupe("on-reset");
    var labels = std.StringHashMap(u16).init(gpa);
    defer labels.deinit();
    var references = std.ArrayList(Reference).init(gpa);
    defer references.deinit();

    var current_macro: ?Macro = null;
    defer if (current_macro) |macro| freeMacro(macro.tokens, gpa);
    var macro_call_stack: std.BoundedArray(*TokenList.Node, 32) = .{};

    var output = Output{};

    while (includes.len > 0) {
        const current_file = &includes.buffer[includes.len - 1];
        errdefer |e| std.log.err("{s}:{}:{} {any} \"{s}\"", .{
            pool.get(current_file.filename),
            current_file.tokenizer.last_line_number,
            current_file.tokenizer.last_column_number,
            e,
            current_file.tokenizer.buffer.slice(),
        });
        var reader = current_file.reader.reader();
        while (true) {
            const token = blk: {
                if (macro_call_stack.len > 0) {
                    const top = macro_call_stack.buffer[macro_call_stack.len - 1];
                    const token = top.data;
                    if (top.next) |next_node| {
                        macro_call_stack.buffer[macro_call_stack.len - 1] = next_node;
                    } else {
                        macro_call_stack.len -= 1;
                    }
                    break :blk token;
                }
                const token = (try current_file.tokenizer.nextToken(reader)) orelse {
                    current_file.deinit();
                    includes.len -= 1;
                    break;
                };
                break :blk token;
            };
            errdefer std.log.info("on token: {any}", .{token});
            if (current_macro) |*macro| {
                if (token == .macro_end) {
                    try macros.put(pool.get(macro.name), macro.tokens);
                    current_macro = null;
                } else if (token != .macro_begin) {
                    const node = try gpa.create(TokenList.Node);
                    errdefer gpa.destroy(node);
                    node.* = TokenList.Node{ .data = try token.cloneWithPool(&pool) };
                    macro.tokens.append(node);
                }
                continue;
            }
            switch (token) {
                .include => |filename| {
                    var included_file = try IncludedFile.init(filename, &pool);
                    errdefer included_file.deinit();
                    try includes.append(included_file);
                    std.log.info("opened file {s}", .{pool.get(included_file.filename)});
                    break;
                },
                .macro_definition => |name| {
                    current_macro = Macro{
                        .name = try pool.insert(name),
                    };
                },
                .macro_begin, .macro_end => return error.MissingMacro,
                .pad_absolute => |amount| {
                    output.ptr = amount;
                    output.lit_last = false;
                    output.jsr_last = 0;
                },
                .pad_relative => |amount| {
                    output.ptr += amount;
                    output.lit_last = false;
                    output.jsr_last = 0;
                },
                .label => |name| {
                    const cloned_name = try pool.dupe(name);
                    current_scope = cloned_name;
                    const label_ptr = std.math.cast(u16, output.ptr) orelse {
                        return error.LabelOutsideRom;
                    };
                    try labels.put(cloned_name, label_ptr);
                    output.lit_last = false;
                    output.jsr_last = 0;
                },
                .sublabel => |name| {
                    var buf: [MaxNameLen]u8 = undefined;
                    const full_name = try pool.dupe(sublabel(&buf, name, current_scope));
                    const label_ptr = if (output.ptr > Output.RomSize)
                        return error.LabelOutsideRom
                    else
                        @intCast(u16, output.ptr);
                    try labels.put(full_name, label_ptr);
                    output.lit_last = false;
                    output.jsr_last = 0;
                },
                .literal_hex_byte => |b| try output.writeByteLiteral(b),
                .literal_hex_short => |s| try output.writeShort(s, true),
                .literal_zeropage_byte => |name| {
                    try output.writeByteLiteral(0xff); // placeholder
                    try makeReference(
                        &pool,
                        &references,
                        output.ptr - 1,
                        .zeropage_byte,
                        name,
                        current_scope,
                        current_file,
                    );
                },
                .literal_relative_byte => |name| {
                    try output.writeByteLiteral(0xff);
                    try makeReference(
                        &pool,
                        &references,
                        output.ptr - 1,
                        .relative_byte,
                        name,
                        current_scope,
                        current_file,
                    );
                },
                .literal_absolute_short => |name| {
                    try output.writeShort(0xffff, true);
                    try makeReference(
                        &pool,
                        &references,
                        output.ptr - 2,
                        .literal_short,
                        name,
                        current_scope,
                        current_file,
                    );
                },
                .raw_absolute_short => |name| {
                    try output.writeShort(0xffff, false);
                    try makeReference(
                        &pool,
                        &references,
                        output.ptr - 2,
                        .raw_short,
                        name,
                        current_scope,
                        current_file,
                    );
                },
                .raw_string => |val| for (val) |c| try output.writeByte(c),
                .raw_byte => |b| try output.writeByte(b),
                .raw_short => |s| try output.writeShort(s, false),
                .bracket => continue,
                .opcode => |opcode| try output.writeOpcode(opcode),
                .macro_call => |name| if (macros.get(name)) |list| {
                    try macro_call_stack.append(list.first.?);
                } else {
                    return error.UnknownMacro;
                },
            }
        }
    }

    for (references.items) |reference| {
        const reference_name = pool.get(reference.name);
        errdefer |e| std.log.err("{s}:{}:{} {any} \"{s}\"({})", .{
            pool.get(reference.filename),
            reference.line_number,
            reference.column_number,
            e,
            reference_name,
            reference_name.len,
        });
        if (labels.get(pool.get(reference.name))) |target| {
            switch (reference.rune) {
                .zeropage_byte => output.buffer[reference.address] = @truncate(u8, target),
                .relative_byte => {
                    const rel_addr = std.math.cast(
                        i8,
                        @intCast(i17, target) - @intCast(i17, reference.address) - 2,
                    ) orelse return error.RelativeReferenceTooFar;
                    output.buffer[reference.address] = @bitCast(u8, rel_addr);
                },
                .literal_short, .raw_short => {
                    std.mem.writeIntBig(u16, output.buffer[reference.address..][0..2], target);
                },
            }
        } else {
            return error.UnknownReference;
        }
    }

    const last_written = output.written.iterator(.{ .kind = .set, .direction = .reverse }).next().? + 1;
    return try gpa.dupe(u8, output.buffer[Output.ProgramBegin..last_written]);
}
