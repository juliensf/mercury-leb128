%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury library for writing / reading Little Endian Base 128 (LEB128)
% encoded integers to / from byte (uint8) streams.
%
%-----------------------------------------------------------------------------%

:- module leb128.
:- interface.

:- import_module stream.

%---------------------------------------------------------------------------%
%
% LEB128 reader errors.
%

:- type leb128.error(Error)
    --->    too_many_bytes_error
            % A LEB128 encoding contains extra bytes that would cause the
            % target type to overflow.

    ;       extra_bits_error
            % A LEB128 encoding contains extra bits that would cause the
            % target type to overflow.

    ;       unexpected_eof_error
            % An unexpected end-of-stream was reached.

    ;       stream_error(Error).
            % An error has occurred in the underlying byte stream.

:- type leb128.result(Error) == stream.result(leb128.error(Error)).

:- type leb128.result(T, Error) == stream.result(T, leb128.error(Error)).

%---------------------------------------------------------------------------%

    % Write the bytes in the LEB128 encoding of a signed 16-bit integer
    % to the given byte stream.
    %
:- pred put_int16(Stream::in, int16::in, State::di, State::uo) is det
    <= stream.writer(Stream, uint8, State).

    % Read a LEB128 encoded signed 16-bit integer from the given byte
    % stream.
    %
:- pred get_int16(Stream::in, leb128.result(int16, Error)::out,
    State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

    % As above, but avoid memory allocation when there is no error.
    %
:- pred unboxed_get_int16(Stream::in, leb128.result(Error)::out,
    int16::out, State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

%---------------------------------------------------------------------------%

    % Write the bytes in the LEB128 encoding of an unsigned 16-bit integer
    % to the given byte stream.
    %
:- pred put_uint16(Stream::in, uint16::in, State::di, State::uo) is det
    <= stream.writer(Stream, uint8, State).

    % Read a LEB128 encoded unsigned 16-bit integer from the given byte
    % stream.
    %
:- pred get_uint16(Stream::in, leb128.result(uint16, Error)::out,
    State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

    % As above, but avoid memory allocation when there is no error.
    %
:- pred unboxed_get_uint16(Stream::in, leb128.result(Error)::out,
    uint16::out, State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

%---------------------------------------------------------------------------%

    % Write the bytes in the LEB128 encoding of a signed 32-bit integer
    % to the given byte stream.
    %
:- pred put_int32(Stream::in, int32::in, State::di, State::uo) is det
    <= stream.writer(Stream, uint8, State).

    % Read a LEB128 encoded signed 32-bit integer from the given byte
    % stream.
    %
:- pred get_int32(Stream::in, leb128.result(int32, Error)::out,
    State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

    % As above, but avoid memory allocation when there is no error.
    %
:- pred unboxed_get_int32(Stream::in, leb128.result(Error)::out,
    int32::out, State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

%---------------------------------------------------------------------------%

    % Write the bytes in the LEB128 encoding of an unsigned 32-bit integer
    % to the given byte stream.
    %
:- pred put_uint32(Stream::in, uint32::in, State::di, State::uo) is det
    <= stream.writer(Stream, uint8, State).

    % Read a LEB128 encoded unsigned 32-bit integer from the given byte
    % stream.
    %
:- pred get_uint32(Stream::in, leb128.result(uint32, Error)::out,
    State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

    % As above, but avoid memory allocation when there is no error.
    %
:- pred unboxed_get_uint32(Stream::in, leb128.result(Error)::out,
    uint32::out, State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

%---------------------------------------------------------------------------%

    % Write the bytes in the LEB128 encoding of a signed 64-bit integer
    % to the given byte stream.
    %
:- pred put_int64(Stream::in, int64::in, State::di, State::uo) is det
    <= stream.writer(Stream, uint8, State).

    % Read a LEB128 encoded signed 64-bit integer from the given byte
    % stream.
    %
:- pred get_int64(Stream::in, leb128.result(int64, Error)::out,
    State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

    % As above, but avoid memory allocation when there is no error.
    %
:- pred unboxed_get_int64(Stream::in, leb128.result(Error)::out,
    int64::out, State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

%---------------------------------------------------------------------------%

    % Write the bytes in the LEB128 encoding of an unsigned 64-bit integer
    % to the given byte stream.
    %
:- pred put_uint64(Stream::in, uint64::in, State::di, State::uo) is det
    <= stream.writer(Stream, uint8, State).

    % Read a LEB128 encoded unsigned 64-bit integer from the given byte
    % stream.
    %
:- pred get_uint64(Stream::in, leb128.result(uint64, Error)::out,
    State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

    % As above, but avoid memory allocation when there is no error.
    %
:- pred unboxed_get_uint64(Stream::in, leb128.result(Error)::out,
    uint64::out, State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.
:- import_module uint8.

%---------------------------------------------------------------------------%

put_int16(Stream, !.Value, !State) :-
    Byte0 = uint16.cast_to_uint8(uint16.cast_from_int16(!.Value /\ 0x7f_i16)),
    !:Value = !.Value >> 7,
    ( if
        (
            (!.Value = 0_i16, bit_is_clear(Byte0, 6u))
        ;
            (!.Value = -1_i16, bit_is_set(Byte0, 6u))
        )
    then
        stream.put(Stream, Byte0, !State)
    else
        Byte = uint8.set_bit(Byte0, 7u),
        stream.put(Stream, Byte, !State),
        put_int16(Stream, !.Value, !State)
    ).

%---------------------------------------------------------------------------%

put_uint16(Stream, !.Value, !State) :-
    Byte0 = uint16.cast_to_uint8(!.Value /\ 0x7f_u16),
    !:Value = !.Value >> 7,
    ( if !.Value \= 0_u16 then
        Byte = uint8.set_bit(Byte0, 7_u),
        stream.put(Stream, Byte, !State),
        put_uint16(Stream, !.Value, !State)
    else
        stream.put(Stream, Byte0, !State)
    ).

%---------------------------------------------------------------------------%

get_int16(Stream, Result, !State) :-
    unboxed_get_int16(Stream, Result0, Value, !State),
    (
        Result0 = ok,
        Result = ok(Value)
    ;
        Result0 = eof,
        Result = eof
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

unboxed_get_int16(Stream, Result, Value, !State) :-
    get_int16_loop(Stream, Result, 0_i16, Value, 0, 0, !State).

:- pred get_int16_loop(Stream::in, leb128.result(Error)::out,
    int16::in, int16::out, int::in, int::in, State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

get_int16_loop(Stream, Result, !Value, !.BytesRead, !.Shift, !State) :-
    stream.unboxed_get(Stream, GetResult, Byte, !State),
    (
        GetResult = ok,
        !:BytesRead = !.BytesRead + 1,
        compare(BytesReadResult, !.BytesRead, 3),
        ( if BytesReadResult = (>) then
            Result = error(too_many_bytes_error)
        else if
            BytesReadResult = (=),
            check_for_signed_extra_bits(Byte, 1)
        then
            Result = error(extra_bits_error)
        else
            !:Value = !.Value \/
                int16.cast_from_uint16(
                    uint16.cast_from_uint8(
                        uint8.clear_bit(Byte, 7u)) << !.Shift),
            !:Shift = !.Shift + 7,
            ( if uint8.bit_is_clear(Byte, 7u) then
                ( if !.Shift < 16, uint8.bit_is_set(Byte, 6u) then
                    !:Value = !.Value \/ (\ 0_i16 << !.Shift)
                else
                    true
                ),
                Result = ok
            else
                get_int16_loop(Stream, Result, !Value, !.BytesRead, !.Shift,
                    !State)
            )
        )
    ;
        GetResult = eof,
        ( if !.BytesRead > 0 then
            Result = error(unexpected_eof_error)
        else
            Result = eof
        )
    ;
        GetResult = error(Error),
        Result = error(stream_error(Error))
    ).

:- pred check_for_signed_extra_bits(uint8::in, int::in) is semidet.

check_for_signed_extra_bits(Byte, NumValidBits) :-
    ExtraBitsMask = (0xff_u8 << NumValidBits) /\ \ 0x80u8,
    ExtraBits = Byte /\ ExtraBitsMask,
    not (
        ExtraBits = 0_u8
    ;
        ExtraBits = ExtraBitsMask
    ).

%---------------------------------------------------------------------------%

get_uint16(Stream, Result, !State) :-
    unboxed_get_uint16(Stream, Result0, Value, !State),
    (
        Result0 = ok,
        Result = ok(Value)
    ;
        Result0 = eof,
        Result = eof
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

unboxed_get_uint16(Stream, Result, Value, !State) :-
    get_uint16_loop(Stream, Result, 0_u16, Value, 0, 0, !State).

:- pred get_uint16_loop(Stream::in, leb128.result(Error)::out,
    uint16::in, uint16::out, int::in, int::in, State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

get_uint16_loop(Stream, Result, !Value, !.BytesRead, !.Shift, !State) :-
    stream.unboxed_get(Stream, GetResult, Byte, !State),
    (
        GetResult = ok,
        !:BytesRead = !.BytesRead + 1,
        compare(BytesReadResult, !.BytesRead, 3),
        ( if BytesReadResult = (>) then
            Result = error(too_many_bytes_error)
        else if
            BytesReadResult = (=),
            check_for_extra_bits(Byte, 2)
        then
            Result = error(extra_bits_error)
        else
            !:Value = !.Value \/
                uint16.cast_from_uint8(uint8.clear_bit(Byte, 7u)) << !.Shift,
            ( if uint8.bit_is_clear(Byte, 7u) then
                Result = ok
            else
                !:Shift = !.Shift + 7,
                get_uint16_loop(Stream, Result, !Value, !.BytesRead, !.Shift,
                    !State)
            )
        )
    ;
        GetResult = eof,
        ( if !.BytesRead > 0 then
            Result = error(unexpected_eof_error)
        else
            Result = eof
        )
    ;
        GetResult = error(Error),
        Result = error(stream_error(Error))
    ).

:- pred check_for_extra_bits(uint8::in, int::in) is semidet.

check_for_extra_bits(Byte, NumValidBits) :-
    ExtraBitsMask = (0xff_u8 << NumValidBits) /\ \ 0x80u8,
    ExtraBits = Byte /\ ExtraBitsMask,
    ExtraBits \= 0_u8.

%---------------------------------------------------------------------------%

put_int32(Stream, !.Value, !State) :-
    Byte0 = uint8.cast_from_int8(int32.cast_to_int8(!.Value /\ 0x7f_i32)),
    !:Value = !.Value >> 7,
    ( if
        (
            (!.Value = 0_i32, bit_is_clear(Byte0, 6u))
        ;
            (!.Value = -1_i32, bit_is_set(Byte0, 6u))
        )
    then
        stream.put(Stream, Byte0, !State)
    else
        Byte = uint8.set_bit(Byte0, 7u),
        stream.put(Stream, Byte, !State),
        put_int32(Stream, !.Value, !State)
    ).

%---------------------------------------------------------------------------%

put_uint32(Stream, !.Value, !State) :-
    Byte0 = uint32.cast_to_uint8(!.Value /\ 0x7fu32),
    !:Value = !.Value >> 7,
    ( if !.Value \= 0u32 then
        Byte = uint8.set_bit(Byte0, 7u),
        stream.put(Stream, Byte, !State),
        put_uint32(Stream, !.Value, !State)
    else
        stream.put(Stream, Byte0, !State)
    ).

%---------------------------------------------------------------------------%

get_int32(Stream, Result, !State) :-
    unboxed_get_int32(Stream, Result0, Value, !State),
    (
        Result0 = ok,
        Result = ok(Value)
    ;
        Result0 = eof,
        Result = eof
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

unboxed_get_int32(Stream, Result, Value, !State) :-
    get_int32_loop(Stream, Result, 0_i32, Value, 0, 0, !State).

:- pred get_int32_loop(Stream::in, leb128.result(Error)::out,
    int32::in, int32::out, int::in, int::in, State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

get_int32_loop(Stream, Result, !Value, !.BytesRead, !.Shift, !State) :-
    stream.unboxed_get(Stream, GetResult, Byte, !State),
    (
        GetResult = ok,
        !:BytesRead = !.BytesRead + 1,
        compare(BytesReadResult, !.BytesRead, 5),
        ( if BytesReadResult = (>) then
            Result = error(too_many_bytes_error)
        else if
            BytesReadResult = (=),
            check_for_signed_extra_bits(Byte, 3)
        then
            Result = error(extra_bits_error)
        else
            !:Value = !.Value \/
                int32.cast_from_uint32(
                    uint32.cast_from_uint8(
                        uint8.clear_bit(Byte, 7u)) << !.Shift),
            !:Shift = !.Shift + 7,
            ( if uint8.bit_is_clear(Byte, 7u) then
                ( if !.Shift < 32, uint8.bit_is_set(Byte, 6u) then
                    !:Value = !.Value \/ (\ 0_i32 << !.Shift)
                else
                    true
                ),
                Result = ok
            else
                get_int32_loop(Stream, Result, !Value, !.BytesRead, !.Shift,
                    !State)
            )
        )
    ;
        GetResult = eof,
        ( if !.BytesRead > 0 then
            Result = error(unexpected_eof_error)
        else
            Result = eof
        )
    ;
        GetResult = error(Error),
        Result = error(stream_error(Error))
    ).

%---------------------------------------------------------------------------%

get_uint32(Stream, Result, !State) :-
    unboxed_get_uint32(Stream, Result0, Value, !State),
    (
        Result0 = ok,
        Result = ok(Value)
    ;
        Result0 = eof,
        Result = eof
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

unboxed_get_uint32(Stream, Result, Value, !State) :-
    get_uint32_loop(Stream, Result, 0u32, Value, 0, 0, !State).

:- pred get_uint32_loop(Stream::in, leb128.result(Error)::out,
    uint32::in, uint32::out, int::in, int::in, State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

get_uint32_loop(Stream, Result, !Value, !.BytesRead, !.Shift, !State) :-
    stream.unboxed_get(Stream, GetResult, Byte, !State),
    (
        GetResult = ok,
        !:BytesRead = !.BytesRead + 1,
        compare(BytesReadResult, !.BytesRead, 5),
        ( if BytesReadResult = (>) then
            Result = error(too_many_bytes_error)
        else if
            BytesReadResult = (=),
            check_for_extra_bits(Byte, 4)
        then
            Result = error(extra_bits_error)
        else
            !:Value = !.Value \/
                 cast_from_uint8(clear_bit(Byte, 7u)) << !.Shift,
            ( if bit_is_clear(Byte, 7u) then
                Result = ok
            else
                !:Shift = !.Shift + 7,
                get_uint32_loop(Stream, Result, !Value, !.BytesRead, !.Shift,
                    !State)
            )
        )
    ;
        GetResult = eof,
        ( if !.BytesRead > 0 then
            Result = error(unexpected_eof_error)
        else
            Result = eof
        )
    ;
        GetResult = error(Error),
        Result = error(stream_error(Error))
    ).

%---------------------------------------------------------------------------%

put_int64(Stream, !.Value, !State) :-
    Byte0 = uint64.cast_to_uint8(uint64.cast_from_int64(!.Value /\ 0x7f_i64)),
    !:Value = !.Value >> 7,
    ( if
        (
            (!.Value = 0_i64, bit_is_clear(Byte0, 6u))
        ;
            (!.Value = -1_i64, bit_is_set(Byte0, 6u))
        )
    then
        stream.put(Stream, Byte0, !State)
    else
        Byte = uint8.set_bit(Byte0, 7u),
        stream.put(Stream, Byte, !State),
        put_int64(Stream, !.Value, !State)
    ).

%---------------------------------------------------------------------------%

get_int64(Stream, Result, !State) :-
    unboxed_get_int64(Stream, Result0, Value, !State),
    (
        Result0 = ok,
        Result = ok(Value)
    ;
        Result0 = eof,
        Result = eof
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

unboxed_get_int64(Stream, Result, Value, !State) :-
    get_int64_loop(Stream, Result, 0_i64, Value, 0, 0, !State).

:- pred get_int64_loop(Stream::in, leb128.result(Error)::out,
    int64::in, int64::out, int::in, int::in, State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

get_int64_loop(Stream, Result, !Value, !.BytesRead, !.Shift, !State) :-
    stream.unboxed_get(Stream, GetResult, Byte, !State),
    (
        GetResult = ok,
        !:BytesRead = !.BytesRead + 1,
        compare(BytesReadResult, !.BytesRead, 10),
        ( if BytesReadResult = (>) then
            Result = error(too_many_bytes_error)
        else if
            BytesReadResult = (=),
            check_for_signed_extra_bits(Byte, 0)
        then
            Result = error(extra_bits_error)
        else
            !:Value = !.Value \/
                int64.cast_from_uint64(
                    uint64.cast_from_uint8(
                        uint8.clear_bit(Byte, 7u)) << !.Shift),
            !:Shift = !.Shift + 7,
            ( if uint8.bit_is_clear(Byte, 7u) then
                ( if !.Shift < 64, uint8.bit_is_set(Byte, 6u) then
                    !:Value = !.Value \/ (\ 0_i64 << !.Shift)
                else
                    true
                ),
                Result = ok
            else
                get_int64_loop(Stream, Result, !Value, !.BytesRead, !.Shift,
                    !State)
            )
        )
    ;
        GetResult = eof,
        ( if !.BytesRead > 0 then
            Result = error(unexpected_eof_error)
        else
            Result = eof
        )
    ;
        GetResult = error(Error),
        Result = error(stream_error(Error))
    ).

%---------------------------------------------------------------------------%

put_uint64(Stream, !.Value, !State) :-
    Byte0 = uint64.cast_to_uint8(!.Value /\ 0x7f_u64),
    !:Value = !.Value >> 7,
    ( if !.Value \= 0_u64 then
        Byte = uint8.set_bit(Byte0, 7_u),
        stream.put(Stream, Byte, !State),
        put_uint64(Stream, !.Value, !State)
    else
        stream.put(Stream, Byte0, !State)
    ).

%---------------------------------------------------------------------------%

get_uint64(Stream, Result, !State) :-
    unboxed_get_uint64(Stream, Result0, Value, !State),
    (
        Result0 = ok,
        Result = ok(Value)
    ;
        Result0 = eof,
        Result = eof
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

unboxed_get_uint64(Stream, Result, Value, !State) :-
    get_uint64_loop(Stream, Result, 0_u64, Value, 0, 0, !State).

:- pred get_uint64_loop(Stream::in, leb128.result(Error)::out,
    uint64::in, uint64::out, int::in, int::in, State::di, State::uo) is det
    <= stream.unboxed_reader(Stream, uint8, State, Error).

get_uint64_loop(Stream, Result, !Value, !.BytesRead, !.Shift, !State) :-
    stream.unboxed_get(Stream, GetResult, Byte, !State),
    (
        GetResult = ok,
        !:BytesRead = !.BytesRead + 1,
        compare(BytesReadResult, !.BytesRead, 10),
        ( if BytesReadResult = (>) then
            Result = error(too_many_bytes_error)
        else if
            BytesReadResult = (=),
            check_for_extra_bits(Byte, 1)
        then
            Result = error(extra_bits_error)
        else
            !:Value = !.Value \/
                 uint64.cast_from_uint8(uint8.clear_bit(Byte, 7_u)) << !.Shift,
            ( if bit_is_clear(Byte, 7_u) then
                Result = ok
            else
                !:Shift = !.Shift + 7,
                get_uint64_loop(Stream, Result, !Value, !.BytesRead, !.Shift,
                    !State)
            )
        )
    ;
        GetResult = eof,
        ( if !.BytesRead > 0 then
            Result = error(unexpected_eof_error)
        else
            Result = eof
        )
    ;
        GetResult = error(Error),
        Result = error(stream_error(Error))
    ).

%---------------------------------------------------------------------------%
:- end_module leb128.
%---------------------------------------------------------------------------%
