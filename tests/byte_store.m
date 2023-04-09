%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023, Julien Fischer.
% See the file COPYING for license details.
%---------------------------------------------------------------------------%

:- module byte_store.
:- interface.

:- import_module io.
:- import_module list.
:- import_module stream.

%---------------------------------------------------------------------------%

:- type byte_store.
:- type byte_store_error.

:- pred init(byte_store::out, io::di, io::uo) is det.

:- pred get_bytes(byte_store::in, list(uint8)::out, io::di, io::uo) is det.

:- pred put_bytes(byte_store::in, list(uint8)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- instance error(byte_store_error).
:- instance stream(byte_store, io).
:- instance input(byte_store, io).
:- instance reader(byte_store, uint8, io, byte_store_error).
:- instance unboxed_reader(byte_store, uint8, io, byte_store_error).
:- instance output(byte_store, io).
:- instance writer(byte_store, uint8, io).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module queue.
:- import_module store.

%---------------------------------------------------------------------------%

:- type byte_store_error
    --->    byte_store_error.

:- type byte_store
    --->    byte_store(io_mutvar(queue(uint8))).

init(ByteStore, !IO) :-
    queue.init(Bytes),
    store.new_mutvar(Bytes, MutVar, !IO),
    ByteStore = byte_store(MutVar).

:- pragma foreign_export("C", byte_store.put(in, in, di, uo), "BS_put").
:- pred put(byte_store::in, uint8::in, io::di, io::uo) is det.

put(ByteStore, Byte, !IO) :-
    ByteStore = byte_store(MutVar),
    get_mutvar(MutVar, Bytes0, !IO),
    queue.put(Byte, Bytes0, Bytes),
    set_mutvar(MutVar, Bytes, !IO).

:- pred get(byte_store::in, stream.result(uint8, byte_store_error)::out,
    io::di, io::uo) is det.

get(ByteStore, Result, !IO) :-
    ByteStore = byte_store(MutVar),
    get_mutvar(MutVar, Bytes0, !IO),
    ( if queue.get(Byte, Bytes0, Bytes) then
        set_mutvar(MutVar, Bytes, !IO),
        Result = ok(Byte)
    else
        Result = eof
    ).

:- pred unboxed_get(byte_store::in, stream.result(byte_store_error)::out,
    uint8::out, io::di, io::uo) is det.

unboxed_get(ByteStore, Result, Byte, !IO) :-
    ByteStore = byte_store(MutVar),
    get_mutvar(MutVar, Bytes0, !IO),
    ( if queue.get(Byte0, Bytes0, Bytes) then
        set_mutvar(MutVar, Bytes, !IO),
        Byte = Byte0,
        Result = ok
    else
        Result = eof,
        Byte = 0u8
    ).

get_bytes(ByteStore, ByteList, !IO) :-
    ByteStore = byte_store(MutVar),
    get_mutvar(MutVar, Bytes0, !IO),
    ByteList = queue.to_list(Bytes0).

put_bytes(ByteStore, BytesToAdd, !IO) :-
    ByteStore = byte_store(MutVar),
    get_mutvar(MutVar, Bytes0, !IO),
    queue.put_list(BytesToAdd, Bytes0, Bytes),
    set_mutvar(MutVar, Bytes, !IO).

%---------------------------------------------------------------------------%

:- instance error(byte_store_error) where [
    error_message(_) = "byte_store error"
].

:- instance stream(byte_store, io) where [
    name(_, "<<byte_store>>", !IO)
].

:- instance input(byte_store, io) where [].

:- instance reader(byte_store, uint8, io, byte_store_error) where [
    pred(get/4) is byte_store.get
].

:- instance unboxed_reader(byte_store, uint8, io, byte_store_error) where [
    pred(unboxed_get/5) is byte_store.unboxed_get
].

:- instance output(byte_store, io) where [
    flush(_, !IO)
].

:- instance writer(byte_store, uint8, io) where [
    pred(put/4) is byte_store.put
].

%---------------------------------------------------------------------------%
:- end_module byte_store.
%---------------------------------------------------------------------------%
