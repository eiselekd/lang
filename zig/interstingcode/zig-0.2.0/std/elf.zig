const builtin = @import("builtin");
const std = @import("index.zig");
const io = std.io;
const os = std.os;
const math = std.math;
const mem = std.mem;
const debug = std.debug;
const InStream = std.stream.InStream;

pub const SHT_NULL = 0;
pub const SHT_PROGBITS = 1;
pub const SHT_SYMTAB = 2;
pub const SHT_STRTAB = 3;
pub const SHT_RELA = 4;
pub const SHT_HASH = 5;
pub const SHT_DYNAMIC = 6;
pub const SHT_NOTE = 7;
pub const SHT_NOBITS = 8;
pub const SHT_REL = 9;
pub const SHT_SHLIB = 10;
pub const SHT_DYNSYM = 11;
pub const SHT_INIT_ARRAY = 14;
pub const SHT_FINI_ARRAY = 15;
pub const SHT_PREINIT_ARRAY = 16;
pub const SHT_GROUP = 17;
pub const SHT_SYMTAB_SHNDX = 18;
pub const SHT_LOOS = 0x60000000;
pub const SHT_HIOS = 0x6fffffff;
pub const SHT_LOPROC = 0x70000000;
pub const SHT_HIPROC = 0x7fffffff;
pub const SHT_LOUSER = 0x80000000;
pub const SHT_HIUSER = 0xffffffff;

pub const FileType = enum {
    Relocatable,
    Executable,
    Shared,
    Core,
};

pub const Arch = enum {
    Sparc,
    x86,
    Mips,
    PowerPc,
    Arm,
    SuperH,
    IA_64,
    x86_64,
    AArch64,
};

pub const SectionHeader = struct {
    name: u32,
    sh_type: u32,
    flags: u64,
    addr: u64,
    offset: u64,
    size: u64,
    link: u32,
    info: u32,
    addr_align: u64,
    ent_size: u64,
};

pub const Elf = struct {
    in_file: &os.File,
    auto_close_stream: bool,
    is_64: bool,
    endian: builtin.Endian,
    file_type: FileType,
    arch: Arch,
    entry_addr: u64,
    program_header_offset: u64,
    section_header_offset: u64,
    string_section_index: u64,
    string_section: &SectionHeader,
    section_headers: []SectionHeader,
    allocator: &mem.Allocator,
    prealloc_file: os.File,

    /// Call close when done.
    pub fn openPath(elf: &Elf, allocator: &mem.Allocator, path: []const u8) !void {
        try elf.prealloc_file.open(path);
        try elf.openFile(allocator, &elf.prealloc_file);
        elf.auto_close_stream = true;
    }

    /// Call close when done.
    pub fn openFile(elf: &Elf, allocator: &mem.Allocator, file: &os.File) !void {
        elf.allocator = allocator;
        elf.in_file = file;
        elf.auto_close_stream = false;

        var file_stream = io.FileInStream.init(elf.in_file);
        const in = &file_stream.stream;

        var magic: [4]u8 = undefined;
        try in.readNoEof(magic[0..]);
        if (!mem.eql(u8, magic, "\x7fELF")) return error.InvalidFormat;

        elf.is_64 = switch (try in.readByte()) {
            1 => false,
            2 => true,
            else => return error.InvalidFormat,
        };

        elf.endian = switch (try in.readByte()) {
            1 => builtin.Endian.Little,
            2 => builtin.Endian.Big,
            else => return error.InvalidFormat,
        };

        const version_byte = try in.readByte();
        if (version_byte != 1) return error.InvalidFormat;

        // skip over padding
        try elf.in_file.seekForward(9);

        elf.file_type = switch (try in.readInt(elf.endian, u16)) {
            1 => FileType.Relocatable,
            2 => FileType.Executable,
            3 => FileType.Shared,
            4 => FileType.Core,
            else => return error.InvalidFormat,
        };

        elf.arch = switch (try in.readInt(elf.endian, u16)) {
            0x02 => Arch.Sparc,
            0x03 => Arch.x86,
            0x08 => Arch.Mips,
            0x14 => Arch.PowerPc,
            0x28 => Arch.Arm,
            0x2A => Arch.SuperH,
            0x32 => Arch.IA_64,
            0x3E => Arch.x86_64,
            0xb7 => Arch.AArch64,
            else => return error.InvalidFormat,
        };

        const elf_version = try in.readInt(elf.endian, u32);
        if (elf_version != 1) return error.InvalidFormat;

        if (elf.is_64) {
            elf.entry_addr = try in.readInt(elf.endian, u64);
            elf.program_header_offset = try in.readInt(elf.endian, u64);
            elf.section_header_offset = try in.readInt(elf.endian, u64);
        } else {
            elf.entry_addr = u64(try in.readInt(elf.endian, u32));
            elf.program_header_offset = u64(try in.readInt(elf.endian, u32));
            elf.section_header_offset = u64(try in.readInt(elf.endian, u32));
        }

        // skip over flags
        try elf.in_file.seekForward(4);

        const header_size = try in.readInt(elf.endian, u16);
        if ((elf.is_64 and header_size != 64) or
            (!elf.is_64 and header_size != 52))
        {
            return error.InvalidFormat;
        }

        const ph_entry_size = try in.readInt(elf.endian, u16);
        const ph_entry_count = try in.readInt(elf.endian, u16);
        const sh_entry_size = try in.readInt(elf.endian, u16);
        const sh_entry_count = try in.readInt(elf.endian, u16);
        elf.string_section_index = u64(try in.readInt(elf.endian, u16));

        if (elf.string_section_index >= sh_entry_count) return error.InvalidFormat;

        const sh_byte_count = u64(sh_entry_size) * u64(sh_entry_count);
        const end_sh = try math.add(u64, elf.section_header_offset, sh_byte_count);
        const ph_byte_count = u64(ph_entry_size) * u64(ph_entry_count);
        const end_ph = try math.add(u64, elf.program_header_offset, ph_byte_count);

        const stream_end = try elf.in_file.getEndPos();
        if (stream_end < end_sh or stream_end < end_ph) {
            return error.InvalidFormat;
        }

        try elf.in_file.seekTo(elf.section_header_offset);

        elf.section_headers = try elf.allocator.alloc(SectionHeader, sh_entry_count);
        errdefer elf.allocator.free(elf.section_headers);

        if (elf.is_64) {
            if (sh_entry_size != 64) return error.InvalidFormat;

            for (elf.section_headers) |*elf_section| {
                elf_section.name         = try in.readInt(elf.endian, u32);
                elf_section.sh_type      = try in.readInt(elf.endian, u32);
                elf_section.flags        = try in.readInt(elf.endian, u64);
                elf_section.addr         = try in.readInt(elf.endian, u64);
                elf_section.offset       = try in.readInt(elf.endian, u64);
                elf_section.size         = try in.readInt(elf.endian, u64);
                elf_section.link         = try in.readInt(elf.endian, u32);
                elf_section.info         = try in.readInt(elf.endian, u32);
                elf_section.addr_align   = try in.readInt(elf.endian, u64);
                elf_section.ent_size     = try in.readInt(elf.endian, u64);
            }
        } else {
            if (sh_entry_size != 40) return error.InvalidFormat;

            for (elf.section_headers) |*elf_section| {
                // TODO (multiple occurences) allow implicit cast from %u32 -> %u64 ?
                elf_section.name = try in.readInt(elf.endian, u32);
                elf_section.sh_type = try in.readInt(elf.endian, u32);
                elf_section.flags = u64(try in.readInt(elf.endian, u32));
                elf_section.addr = u64(try in.readInt(elf.endian, u32));
                elf_section.offset = u64(try in.readInt(elf.endian, u32));
                elf_section.size = u64(try in.readInt(elf.endian, u32));
                elf_section.link = try in.readInt(elf.endian, u32);
                elf_section.info = try in.readInt(elf.endian, u32);
                elf_section.addr_align = u64(try in.readInt(elf.endian, u32));
                elf_section.ent_size = u64(try in.readInt(elf.endian, u32));
            }
        }

        for (elf.section_headers) |*elf_section| {
            if (elf_section.sh_type != SHT_NOBITS) {
                const file_end_offset = try math.add(u64, elf_section.offset, elf_section.size);
                if (stream_end < file_end_offset) return error.InvalidFormat;
            }
        }

        elf.string_section = &elf.section_headers[elf.string_section_index];
        if (elf.string_section.sh_type != SHT_STRTAB) {
            // not a string table
            return error.InvalidFormat;
        }
    }

    pub fn close(elf: &Elf) void {
        elf.allocator.free(elf.section_headers);

        if (elf.auto_close_stream)
            elf.in_file.close();
    }

    pub fn findSection(elf: &Elf, name: []const u8) !?&SectionHeader {
        var file_stream = io.FileInStream.init(elf.in_file);
        const in = &file_stream.stream;

        section_loop: for (elf.section_headers) |*elf_section| {
            if (elf_section.sh_type == SHT_NULL) continue;

            const name_offset = elf.string_section.offset + elf_section.name;
            try elf.in_file.seekTo(name_offset);

            for (name) |expected_c| {
                const target_c = try in.readByte();
                if (target_c == 0 or expected_c != target_c) continue :section_loop;
            }

            {
                const null_byte = try in.readByte();
                if (null_byte == 0) return elf_section;
            }
        }

        return null;
    }

    pub fn seekToSection(elf: &Elf, elf_section: &SectionHeader) !void {
        try elf.in_file.seekTo(elf_section.offset);
    }
};
