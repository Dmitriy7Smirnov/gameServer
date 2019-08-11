%% @author: Maxim Pushkar
%% @date: 09.08.2019

-module(proto).

%% Include files

%% Exported Functions

-export([
    parse/2,
    pack/2
]).

-record(testrecord,
    {
        id :: integer(),
        name :: string()
    }
).

%%%===================================================================
%%% API
%%%===================================================================
-spec parse(Type :: atom(), Data :: binary()) ->
    {Value :: any(), Rest :: binary()}.

parse(bool, <<0:8/little-signed-integer-unit:1, Rest/binary>>) -> {false, Rest};
parse(bool, <<1:8/little-signed-integer-unit:1, Rest/binary>>) -> {true, Rest};
parse(int8, <<Value:8/little-signed-integer-unit:1, Rest/binary>>) -> {Value, Rest};
parse(uint8, <<Value:8/little-unsigned-integer-unit:1, Rest/binary>>) -> {Value, Rest};
parse(int16, <<Value:16/little-signed-integer-unit:1, Rest/binary>>) -> {Value, Rest};
parse(uint16, <<Value:16/little-unsigned-integer-unit:1, Rest/binary>>) -> {Value, Rest};
parse(int32, <<Value:32/little-signed-integer-unit:1, Rest/binary>>) -> {Value, Rest};
parse(uint32, <<Value:32/little-unsigned-integer-unit:1, Rest/binary>>) -> {Value, Rest};
parse(int64, <<Value:64/little-signed-integer-unit:1, Rest/binary>>) -> {Value, Rest};
parse(uint64, <<Value:64/little-unsigned-integer-unit:1, Rest/binary>>) -> {Value, Rest};
parse(string, <<Length:4/little-unsigned-integer-unit:8, String:Length/binary, Rest/binary>>) -> {String, Rest};
parse(float, <<Value:8/little-signed-float-unit:8, Rest/binary>>) -> {Value, Rest};
parse(testrecord, <<Id:8/little-unsigned-integer-unit:1, 
                    Length:4/little-unsigned-integer-unit:8, String:Length/binary, Rest/binary>>) ->
                        {#testrecord{id = Id, name = String}, Rest}.

pack(bool, false) -> <<0:1/little-signed-integer-unit:8>>;
pack(bool, true) -> <<1:1/little-signed-integer-unit:8>>;
pack(int8, Int8) -> <<Int8:1/little-signed-integer-unit:8>>;
pack(uint8, UInt8) -> <<UInt8:1/little-unsigned-integer-unit:8>>;
pack(int16, Int16) -> <<Int16:2/little-signed-integer-unit:8>>;
pack(uint16, UInt16) -> <<UInt16:2/little-unsigned-integer-unit:8>>;
pack(int32, Int32) -> <<Int32:4/little-signed-integer-unit:8>>;
pack(uint32, UInt32) -> <<UInt32:4/little-unsigned-integer-unit:8>>;
pack(int64, Int64) -> <<Int64:8/little-signed-integer-unit:8>>;
pack(uint64, UInt64) -> <<UInt64:8/little-unsigned-integer-unit:8>>;
pack(string, String) -> <<(byte_size(String)):4/little-unsigned-integer-unit:8, String/binary>>;
pack(float, Float) -> <<Float:8/little-signed-float-unit:8>>;
pack(testrecord, #testrecord{id = Id, name = Name}) -> pack(uint8, Id),pack(string, Name).

%%%===================================================================
%%% Internal functions
%%%===================================================================





