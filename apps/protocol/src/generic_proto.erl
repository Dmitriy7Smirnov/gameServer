%% @author: Maxim Pushkar
%% @date: 09.08.2019

-module(generic_proto).

%% Include files

-include_lib("protocol/include/proto.hrl").

%% Exported Functions

-export([
    parse/2,
    pack/2
]).

      

%%%===================================================================
%%% API
%%%===================================================================
    
  


-spec parse(Type :: atom(), Data :: binary()) ->
    {Value :: any(), Rest :: binary()}.

                    
parse(player, <<Hp:8/little-unsigned-integer-unit:1, HpMax:8/little-unsigned-integer-unit:1,
    Id:8/little-unsigned-integer-unit:1, Length:4/little-unsigned-integer-unit:8, Username:Length/binary,
    Rest/binary>>) ->
        {#player{hp = Hp, hp_max = HpMax, id = Id, username = Username}, Rest};                     
                    
parse({list, Type}, <<Length:4/little-unsigned-integer-unit:8, List:Length/binary, Rest/binary>>) ->
    ParsedList = parse(list, Type, List, []),
    {ParsedList, Rest}.
                    
parse(list, _Type, <<>>, Acc) -> Acc;
parse(list, Type, Binary, Acc) -> 
    {Value, Rest} = proto:parse(Type, Binary),
    parse(list, Type, Rest, Acc ++ [Value]).
     


pack(player, #player{hp = Hp, hp_max = HpMax, id = Id, username = Username}) -> <<(proto:pack(uint8, Hp))/binary,
    (proto:pack(uint8, HpMax))/binary, (proto:pack(uint8, Id))/binary, (proto:pack(string, Username))/binary>>;

pack({list, Type}, List) -> 
    LengthInBytes = length(List)*maps:get(Type, type_size_map()),
    PackedItems = lists:foldl(fun(Item, Acc) -> <<Acc/binary, (proto:pack(Type, Item))/binary>> end, <<>>, List), 
    <<LengthInBytes:32/little-unsigned-integer, PackedItems/binary>>.

%%%===================================================================
%%% Internal functions
%%%===================================================================

type_size_map() -> #{
        bool => 1,
        int8 => 1,
        uint8 => 1,
        int16 => 2,
        uint16 => 2,
        int32 => 4,
        uint32 => 4,
        int64 => 8,
        uint64 => 8,
        string => 1,
        float => 4,
        double => 8
    }.



