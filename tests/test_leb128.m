%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023, Julien Fischer.
% See the file COPYING for license details.
%---------------------------------------------------------------------------%

:- module test_leb128.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module stream.
:- import_module string.

:- import_module byte_store.
:- import_module leb128.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.print_line("=== TESTING READ OF INT16 ====", !IO),
    io.nl(!IO),
    io.write_list(test_int16_bytes, "\n", test_read_int16, !IO),
    io.nl(!IO),
    io.print_line("=== TESTING READ OF UINT16 ===", !IO),
    io.nl(!IO),
    io.write_list(test_uint16_bytes, "\n", test_read_uint16, !IO),
    io.nl(!IO),
    io.print_line("=== TESTING READ OF INT32 ===", !IO),
    io.nl(!IO),
    io.write_list(test_int32_bytes, "\n", test_read_int32, !IO),
    io.nl(!IO),
    io.print_line("=== TESTING READ OF UINT32 ===", !IO),
    io.nl(!IO),
    io.write_list(test_uint32_bytes, "\n", test_read_uint32, !IO),
    io.nl(!IO),
    io.print_line("=== TESTING READ OF INT64 ===", !IO),
    io.nl(!IO),
    io.write_list(test_int64_bytes, "\n", test_read_int64, !IO),
    io.nl(!IO),
    io.print_line("=== TESTING READ OF UINT64 ===", !IO),
    io.nl(!IO),
    io.write_list(test_uint64_bytes, "\n", test_read_uint64, !IO),
    io.nl(!IO),
    io.print_line("=== TESTING READ/WRITE OF INT16 ===", !IO),
    io.nl(!IO),
    io.write_list(test_int16s, "\n", test_rw_int16, !IO),
    io.nl(!IO),
    io.print_line("=== TESTING READ/WRITE OF UINT16 ===", !IO),
    io.nl(!IO),
    io.write_list(test_uint16s, "\n", test_rw_uint16, !IO),
    io.nl(!IO),
    io.print_line("=== TESTING READ/WRITE OF INT32 ===", !IO),
    io.nl(!IO),
    io.write_list(test_int32s, "\n", test_rw_int32, !IO),
    io.nl(!IO),
    io.print_line("=== TESTING READ/WRITE OF UINT32 ===", !IO),
    io.nl(!IO),
    io.write_list(test_uint32s, "\n", test_rw_uint32, !IO),
    io.nl(!IO),
    io.print_line("=== TESTING READ/WRITE OF INT64 ===", !IO),
    io.nl(!IO),
    io.write_list(test_int64s, "\n", test_rw_int64, !IO),
    io.nl(!IO),
    io.print_line("=== TESTING READ/WRITE OF UINT64 ===", !IO),
    io.nl(!IO),
    io.write_list(test_uint64s, "\n", test_rw_uint64, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred test_read_int16(list(uint8)::in, io::di, io::uo) is det.

test_read_int16(LEB128, !IO) :-
    byte_store.init(Bytes, !IO),
    print_hex_bytes("Input: ", LEB128, "\n", !IO),
    byte_store.put_bytes(Bytes, LEB128, !IO),
    leb128.get_int16(Bytes, ReadResult, !IO),
    (
        ReadResult = ok(DecodedN),
        io.format("Decoding: %d (0x%x)\n", [i16(DecodedN), i16(DecodedN)], !IO)
    ;
        ReadResult = eof,
        io.print_line("Decoding: <<EOF>>", !IO)
    ;
        ReadResult = error(Error),
        io.format("Decoding: error(\"%s\")\n", [s(string(Error))], !IO)
    ).

:- pred test_read_uint16(list(uint8)::in, io::di, io::uo) is det.

test_read_uint16(LEB128, !IO) :-
    byte_store.init(Bytes, !IO),
    print_hex_bytes("Input: ", LEB128, "\n", !IO),
    byte_store.put_bytes(Bytes, LEB128, !IO),
    leb128.get_uint16(Bytes, ReadResult, !IO),
    (
        ReadResult = ok(DecodedN),
        io.format("Decoding: %u (0x%x)\n", [u16(DecodedN), u16(DecodedN)], !IO)
    ;
        ReadResult = eof,
        io.print_line("Decoding: <<EOF>>", !IO)
    ;
        ReadResult = error(Error),
        io.format("Decoding: error(\"%s\")\n", [s(string(Error))], !IO)
    ).

:- pred test_read_int32(list(uint8)::in, io::di, io::uo) is det.

test_read_int32(LEB128, !IO) :-
    byte_store.init(Bytes, !IO),
    print_hex_bytes("Input: ", LEB128, "\n", !IO),
    byte_store.put_bytes(Bytes, LEB128, !IO),
    leb128.get_int32(Bytes, ReadResult, !IO),
    (
        ReadResult = ok(DecodedN),
        io.format("Decoding: %d (0x%x)\n", [i32(DecodedN), i32(DecodedN)], !IO)
    ;
        ReadResult = eof,
        io.print_line("Decoding: <<EOF>>", !IO)
    ;
        ReadResult = error(Error),
        io.format("Decoding: error(\"%s\")\n", [s(string(Error))], !IO)
    ).

:- pred test_read_uint32(list(uint8)::in, io::di, io::uo) is det.

test_read_uint32(LEB128, !IO) :-
    byte_store.init(Bytes, !IO),
    print_hex_bytes("Input: ", LEB128, "\n", !IO),
    byte_store.put_bytes(Bytes, LEB128, !IO),
    leb128.get_uint32(Bytes, ReadResult, !IO),
    (
        ReadResult = ok(DecodedN),
        io.format("Decoding: %u (0x%x)\n", [u32(DecodedN), u32(DecodedN)], !IO)
    ;
        ReadResult = eof,
        io.print_line("Decoding: <<EOF>>", !IO)
    ;
        ReadResult = error(Error),
        io.format("Decoding: error(\"%s\")\n", [s(string(Error))], !IO)
    ).

:- pred test_read_int64(list(uint8)::in, io::di, io::uo) is det.

test_read_int64(LEB128, !IO) :-
    byte_store.init(Bytes, !IO),
    print_hex_bytes("Input: ", LEB128, "\n", !IO),
    byte_store.put_bytes(Bytes, LEB128, !IO),
    leb128.get_int64(Bytes, ReadResult, !IO),
    (
        ReadResult = ok(DecodedN),
        io.format("Decoding: %d (0x%x)\n", [i64(DecodedN), i64(DecodedN)], !IO)
    ;
        ReadResult = eof,
        io.print_line("Decoding: <<EOF>>", !IO)
    ;
        ReadResult = error(Error),
        io.format("Decoding: error(\"%s\")\n", [s(string(Error))], !IO)
    ).

:- pred test_read_uint64(list(uint8)::in, io::di, io::uo) is det.

test_read_uint64(LEB128, !IO) :-
    byte_store.init(Bytes, !IO),
    print_hex_bytes("Input: ", LEB128, "\n", !IO),
    byte_store.put_bytes(Bytes, LEB128, !IO),
    leb128.get_uint64(Bytes, ReadResult, !IO),
    (
        ReadResult = ok(DecodedN),
        io.format("Decoding: %u (0x%x)\n", [u64(DecodedN), u64(DecodedN)], !IO)
    ;
        ReadResult = eof,
        io.print_line("Decoding: <<EOF>>", !IO)
    ;
        ReadResult = error(Error),
        io.format("Decoding: error(\"%s\")\n", [s(string(Error))], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_rw_int16(int16::in, io::di, io::uo) is det.

test_rw_int16(N, !IO) :-
    byte_store.init(Bytes, !IO),
    io.format("Input: %d (0x%x)\n", [i16(N), i16(N)],  !IO),
    leb128.put_int16(Bytes, N, !IO),
    byte_store.get_bytes(Bytes, Encoding, !IO),
    print_hex_bytes("Encoding: ", Encoding, "\n", !IO),
    leb128.get_int16(Bytes, ReadResult, !IO),
    (
        ReadResult = ok(DecodedN),
        Warn = (if DecodedN = N then "" else " (!) "),
        io.format("Decoding: %d (0x%x)%s\n",
            [i16(DecodedN), i16(DecodedN), s(Warn)], !IO)
    ;
        ReadResult = eof,
        io.print_line("Decoding: <<EOF>>", !IO)
    ;
        ReadResult = error(Error),
        io.format("Decoding: error(\"%s\")\n", [s(string(Error))], !IO)
    ).

:- pred test_rw_int32(int32::in, io::di, io::uo) is det.

test_rw_int32(N, !IO) :-
    byte_store.init(Bytes, !IO),
    io.format("Input: %d (0x%x)\n", [i32(N), i32(N)],  !IO),
    leb128.put_int32(Bytes, N, !IO),
    byte_store.get_bytes(Bytes, Encoding, !IO),
    print_hex_bytes("Encoding: ", Encoding, "\n", !IO),
    leb128.get_int32(Bytes, ReadResult, !IO),
    (
        ReadResult = ok(DecodedN),
        Warn = (if DecodedN = N then "" else " (!) "),
        io.format("Decoding: %d (0x%x)%s\n",
            [i32(DecodedN), i32(DecodedN), s(Warn)], !IO)
    ;
        ReadResult = eof,
        io.print_line("Decoding: <<EOF>>", !IO)
    ;
        ReadResult = error(Error),
        io.format("Decoding: error(\"%s\")\n", [s(string(Error))], !IO)
    ).

:- pred test_rw_int64(int64::in, io::di, io::uo) is det.

test_rw_int64(N, !IO) :-
    byte_store.init(Bytes, !IO),
    io.format("Input: %d (0x%x)\n", [i64(N), i64(N)],  !IO),
    leb128.put_int64(Bytes, N, !IO),
    byte_store.get_bytes(Bytes, Encoding, !IO),
    print_hex_bytes("Encoding: ", Encoding, "\n", !IO),
    ( if have_external_implementation then
        external_encode_int64(N, ExternalEncoding, !IO),
        ( if Encoding = ExternalEncoding then
            true
        else
            print_hex_bytes("External: ", ExternalEncoding, " (MISMATCH)\n",
                !IO)
        )
    else
        true
    ),
    leb128.get_int64(Bytes, ReadResult, !IO),
    (
        ReadResult = ok(DecodedN),
        Warn = (if DecodedN = N then "" else " (!) "),
        io.format("Decoding: %d (0x%x)%s\n",
            [i64(DecodedN), i64(DecodedN), s(Warn)], !IO)
    ;
        ReadResult = eof,
        io.print_line("Decoding: <<EOF>>", !IO)
    ;
        ReadResult = error(Error),
        io.format("Decoding: error(\"%s\")\n", [s(string(Error))], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_rw_uint16(uint16::in, io::di, io::uo) is det.

test_rw_uint16(N, !IO) :-
    byte_store.init(Bytes, !IO),
    io.format("Input: %u (0x%x)\n", [u16(N), u16(N)], !IO),
    leb128.put_uint16(Bytes, N, !IO),
    byte_store.get_bytes(Bytes, Encoding, !IO),
    print_hex_bytes("Encoding: ", Encoding, "\n", !IO),
    leb128.get_uint16(Bytes, ReadResult, !IO),
    (
        ReadResult = ok(DecodedN),
        Warn = (if DecodedN = N then "" else " (!) "),
        io.format("Decoding: %u (0x%x)%s\n",
            [u16(DecodedN), u16(DecodedN), s(Warn)], !IO)
    ;
        ReadResult = eof,
        io.print_line("Decoding: <<EOF>>", !IO)
    ;
        ReadResult = error(Error),
        io.format("Decoding: error(\"%s\")\n", [s(string(Error))], !IO)
    ).

:- pred test_rw_uint32(uint32::in, io::di, io::uo) is det.

test_rw_uint32(N, !IO) :-
    byte_store.init(Bytes, !IO),
    io.format("Input: %u (0x%x)\n", [u32(N), u32(N)], !IO),
    leb128.put_uint32(Bytes, N, !IO),
    byte_store.get_bytes(Bytes, Encoding, !IO),
    print_hex_bytes("Encoding: ", Encoding, "\n", !IO),
    leb128.get_uint32(Bytes, ReadResult, !IO),
    (
        ReadResult = ok(DecodedN),
        Warn = (if DecodedN = N then "" else " (!) "),
        io.format("Decoding: %u (0x%x)%s\n",
            [u32(DecodedN), u32(DecodedN), s(Warn)], !IO)
    ;
        ReadResult = eof,
        io.print_line("Decoding: <<EOF>>", !IO)
    ;
        ReadResult = error(Error),
        io.format("Decoding: error(\"%s\")\n", [s(string(Error))], !IO)
    ).

:- pred test_rw_uint64(uint64::in, io::di, io::uo) is det.

test_rw_uint64(N, !IO) :-
    byte_store.init(Bytes, !IO),
    io.format("Input: %u (0x%x)\n", [u64(N), u64(N)], !IO),
    leb128.put_uint64(Bytes, N, !IO),
    byte_store.get_bytes(Bytes, Encoding, !IO),
    print_hex_bytes("Encoding: ", Encoding, "\n", !IO),
    ( if have_external_implementation then
        external_encode_uint64(N, ExternalEncoding, !IO),
        ( if Encoding = ExternalEncoding then
            true
        else
            print_hex_bytes("External: ", ExternalEncoding, " (MISMATCH)\n",
                !IO)
        )
    else
        true
    ),
    leb128.get_uint64(Bytes, ReadResult, !IO),
    (
        ReadResult = ok(DecodedN),
        Warn = (if DecodedN = N then "" else " (!) "),
        io.format("Decoding: %u (0x%x)%s\n",
            [u64(DecodedN), u64(DecodedN), s(Warn)], !IO)
    ;
        ReadResult = eof,
        io.print_line("Decoding: <<EOF>>", !IO)
    ;
        ReadResult = error(Error),
        io.format("Decoding: error(\"%s\")\n", [s(string(Error))], !IO)
    ).

%---------------------------------------------------------------------------%

:- func test_int16s = list(int16).

test_int16s = [
    -32768_i16,
    -129_i16,
    -128_i16,
    -127_i16,
    -64_i16,
    -63_i16,
    -1_i16,
    0_i16,
    1_i16,
    63_i16,
    64_i16,
    71_i16,
    127_i16,
    128_i16,
    129_i16,
    32767_i16
].

:- func test_uint16s = list(uint16).

test_uint16s = [
    0_u16,
    1_u16,
    2_u16,
    127_u16,
    128_u16,
    255_u16,
    256_u16,
    32767_u16,
    65535_u16
].

:- func test_int16_bytes = list(list(uint8)).

test_int16_bytes = [
    [0x91_u8, 0x7f_u8], % -111
    [0x47_u8],          % -57
    [0x7f_u8],          % -1
    [0x0_u8],           % 0
    [0x1_u8],           % 1
    [0x3a_u8],          % 58
    [0xc7_u8, 0x00_u8], % 71

    [0x80_u8],
    [0x80_u8,0x80_u8,0x80_u8,0x40_u8]  % extra error bits
].

:- func test_uint16_bytes = list(list(uint8)).

test_uint16_bytes = [
    [0x0_u8],
    [0x1_u8],
    [0x3f_u8],
    [0x40_u8],
    [0x80_u8, 0x01_u8],
    [128_u8],
    [128u8, 128u8, 4u8],
    [255u8, 255u8, 3u8],
    [255u8, 255u8, 4u8],  % max_uint16 + 1
    [0x80_u8],
    [0x80_u8, 0x80_u8, 0x84_u8],
    [0x80_u8, 0x80_u8, 0x80_u8, 0x40_u8]
].

%---------------------------------------------------------------------------%

:- func test_int32s = list(int32).

test_int32s = [
    -2147483648_i32,
    -123456_i32,
    -129_i32,
    -128_i32,
    -127_i32,
    -63_i32,
    -64_i32,
    -1_i32,
    0_i32,
    1_i32,
    63_i32,
    64_i32,
    127_i32,
    128_i32,
    129_i32,
    32767_i32,
    65535_i32,
    2147483647_i32
].

:- func test_uint32s = list(uint32).

test_uint32s = [
    0_u32,
    1_u32,
    2_u32,
    8_u32,
    16_u32,
    32_u32,
    64_u32,
    127_u32,
    128_u32,
    120_u32,
    65534_u32,
    65535_u32,
    65536_u32,
    4294967294_u32,
    4294967295_u32
].

:- func test_int32_bytes = list(list(uint8)).

test_int32_bytes = [
    [0xc0_u8, 0xbb_u8, 0x78_u8],                   % -123456
    [0x7f_u8],                                     % -1
    [0x0_u8],                                      % 0
    [0x1_u8]                                       % 1
].

:- func test_uint32_bytes = list(list(uint8)).

test_uint32_bytes = [
    [0u8],
    [1u8],
    [128_u8],
    [255u8, 255u8, 255u8, 255u8, 15u8],
    [255u8, 255u8, 255u8, 255u8, 16u8],
    [0x80_u8, 0x80_u8, 0x80_u8, 0x80_u8, 0x90_u8]
].

%---------------------------------------------------------------------------%

:- func test_int64s = list(int64).

test_int64s = [
    -9223372036854775808_i64,
    -129_i64,
    -128_i64,
    -127_i64,
    -64_i64,
    -63_i64,
    -1_i64,
    0_i64,
    1_i64,
    63_i64,
    64_i64,
    127_i64,
    128_i64,
    129_i64,
    65534_i64,
    65535_i64,
    65536_i64,
    4294967294_i64,
    4294967295_i64,
    9223372036854775807_i64
].

:- func test_uint64s = list(uint64).

test_uint64s = [
    0_u64,
    1_u64,
    2_u64,
    8_u64,
    16_u64,
    32_u64,
    64_u64,
    127_u64,
    128_u64,
    129_u64,
    65534_u64,
    65535_u64,
    65536_u64,
    4294967295_u64,
    4294967296_u64,
    9223372036854775807_u64,
    9223372036854775808_u64,
    18446744073709551614_u64,
    18446744073709551615_u64
].

:- func test_int64_bytes = list(list(uint8)).

test_int64_bytes = [
    [0xc7_u8, 0x9f_u8, 0x7f_u8], % -12345
    [0x80_u8, 0x7f_u8],          % -128
    [0x81_u8, 0x7f_u8],          % -127
    [0x40_u8],                   % -64
    [0x41_u8],                   % -63
    [0x7f_u8],                   % -1
    [0x0_u8],                    % 0.
    [0x1_u8],                    % 1.
    [0x3f_u8],                   % 63
    [0xc0_u8, 0x00_u8],          % 64
    [0x80_u8, 0x01_u8],          % 128
    [0x81_u8, 0x01_u8],          % 129
    [0xff_u8, 0xff_u8, 0xff_u8, 0xff_u8, 0xff_u8, 0xff_u8, 0xff_u8, 0xff_u8, 0xff_u8, 0x00_u8], % 9223372036854775807
    [0x80_u8, 0x80_u8, 0x80_u8, 0x80_u8, 0x80_u8, 0x80_u8, 0x80_u8, 0x80_u8, 0x80_u8, 0x40_u8] % extra bits error
].

:- func test_uint64_bytes = list(list(uint8)).

test_uint64_bytes = [
    [0x0_u8],
    [0x1_u8],

    [0xff_u8, 0x0_u8],
    [0x80_u8, 0x80_u8, 0x0_u8],
    [0xff_u8, 0x0_u8],
    [0xff_u8, 0x80_u8, 0x0_u8],
    [0x80_u8, 0x81_u8, 0x0_u8],

    [128_u8],
    [0x80_u8,0x80_u8,0x80_u8,0x80_u8,0x80_u8,0x80_u8,0x80_u8,0x80_u8,0x80_u8,0x40_u8]
].

%---------------------------------------------------------------------------%

:- pred print_hex_bytes(string::in, list(uint8)::in, string::in,
     io::di, io::uo) is det.

print_hex_bytes(Prefix, Bytes, Suffix, !IO) :-
    io.write_string(Prefix, !IO),
    io.write_char('[', !IO),
    io.write_list(Bytes, ", ", write_hex_byte, !IO),
    io.write_char(']', !IO),
    io.write_string(Suffix, !IO).

:- pred write_hex_byte(uint8::in, io::di, io::uo) is det.

write_hex_byte(Byte, !IO) :-
    io.format("0x%02x", [u8(Byte)], !IO).

%---------------------------------------------------------------------------%

:- pred have_external_implementation is semidet.

:- pragma foreign_proc("C",
    have_external_implementation,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = MR_TRUE;
").

have_external_implementation :-
    semidet_false.

:- pragma foreign_import_module("C", byte_store).

:- pragma foreign_decl("C", "
    #include <stdint.h>
    #include <stdbool.h>
").

% The following LEB128 implementation is from lldb.
% Source: https://opensource.apple.com/source/lldb/lldb-179.1/llvm/include/llvm/Support/LEB128.h.auto.html
%
:- pred external_encode_int64(int64::in, list(uint8)::out, io::di, io::uo) is det.

external_encode_int64(I64, Bytes, !IO) :-
    byte_store.init(ByteStore, !IO),
    do_external_encode_int64(ByteStore, I64, !IO),
    byte_store.get_bytes(ByteStore, Bytes, !IO).

:- pred do_external_encode_int64(byte_store::in, int64::in,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_external_encode_int64(BS::in, Value::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    bool More;
    do {
        uint8_t Byte = Value & 0x7f;
        // NOTE: this assumes that this signed shift is an arithmetic right
        // shift.
        Value >>= 7;
        More = !((((Value == 0 ) && ((Byte & 0x40) == 0)) ||
                ((Value == -1) && ((Byte & 0x40) != 0))));
        if (More) {
            Byte |= 0x80;
        }
        BS_put(BS, Byte);
    } while (More);
").

:- pred external_encode_uint64(uint64::in, list(uint8)::out, io::di, io::uo)
    is det.

external_encode_uint64(U64, Bytes, !IO) :-
    byte_store.init(ByteStore, !IO),
    do_external_encode_uint64(ByteStore, U64, !IO),
    byte_store.get_bytes(ByteStore, Bytes, !IO).

:- pred do_external_encode_uint64(byte_store::in, uint64::in,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_external_encode_uint64(BS::in, Value::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    do {
        uint8_t Byte = Value & 0x7f;
        Value >>= 7;
        if (Value != 0) {
            Byte |= 0x80;
        }
        BS_put(BS, Byte);
    } while (Value != 0);
").

%---------------------------------------------------------------------------%
:- end_module test_leb128.
%---------------------------------------------------------------------------%
