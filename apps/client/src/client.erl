%% @author: Dima
%% @date: 08.08.2019

-module(client).

%% Include files
-include_lib("protocol/include/proto.hrl").

%% Exported Functions

-export([
    start/1,
    start/3,
    login/3,
    send_text/2,
    stop/1,
    client/2,
    find_opponent/2,
    attack/2
]).

start(ClientName) ->
    start(ClientName, "localhost", 1234),
    timer:sleep(infinity).

start(ClientName, Host, Port) ->
    Pid = spawn(?MODULE, client, [Host, Port]),
    register(ClientName, Pid).

login(ClientName, Username, Password) ->
    ClientName ! {login, Username, Password},
    ok.

find_opponent(ClientName, Arms) ->
    ClientName ! {find_opponent, Arms},
    ok.

attack(ClientName, Arms) ->
    ClientName ! {attack, Arms}, 
    ok.

send_text(ClientName, Text) ->
    ClientName ! {text, Text},
    ok.

stop(ClientName) ->
    ClientName ! stop,
    ok.

client(Host, Port) ->
    io:format("CLIENT: pid = ~p connects to ~s:~p~n", [self(), Host, Port]),
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, raw}]),
    loop(Socket, #player{socket = Socket}, #player{socket = Socket}).

loop(Socket, Player, Opponent) ->
    receive
        {login, Username, Password} ->
            ServiceByte = proto:pack(uint8, ?service_auth),
            SubserviceByte = proto:pack(uint8, ?cs_login),
            PackedUsername = proto:pack(string, Username),
            PackedPassword = proto:pack(string, Password),
            send(Socket, <<ServiceByte/binary, SubserviceByte/binary, PackedUsername/binary, PackedPassword/binary>>),
            io:format("CLIENT: trying to login as \"~s\" with password \"~s\"~n", [Username, Password]),
            loop(Socket, Player, Opponent);
        {find_opponent, Arms} ->
            ServiceByte = proto:pack(uint8, ?service_lobby),
            SubserviceByte = proto:pack(uint8, ?cs_find_opponent),
            PackedArms = proto:pack(string, Arms),
            send(Socket, <<ServiceByte/binary, SubserviceByte/binary, PackedArms/binary>>),
            io:format("CLIENT: sending start_battle with arms = ~s~n", [Arms]),    
            loop(Socket, Player, Opponent);
        {attack, Arms} ->
            ServiceByte = proto:pack(uint8, ?service_match),
            SubserviceByte = proto:pack(uint8, ?cs_attack),
            PackedArms = proto:pack(string, Arms),
            send(Socket, <<ServiceByte/binary, SubserviceByte/binary, PackedArms/binary>>),
            io:format("CLIENT: sending attack with arms = ~s~n", [Arms]),    
            loop(Socket, Player, Opponent);
        {text, Text} ->
            send(Socket, proto:pack(string, Text)),
            io:format("CLIENT: sending text ~s~n", [Text]),
            loop(Socket, Player, Opponent);
        {tcp, Socket, Data} ->
            Data1 = recv(Data),
            {ServiceByte, Rest} = proto:parse(uint8, Data1),
            {SubserviceByte, Rest1} = proto:parse(uint8, Rest),
            {ReplyStatusByte, Rest2} = proto:parse(uint8, Rest1),
            {ReplyMsg, Rest3} = proto:parse(string, Rest2),
            {Player1, Rest4} = generic_proto:parse(player, Rest3),
            {Opponent1, _Rest5} = generic_proto:parse(player, Rest4),


            analyze(ServiceByte, SubserviceByte, ReplyStatusByte, ReplyMsg, Player1, Opponent1),
            loop(Socket, Player1, Opponent1);
        stop ->
            io:format("CLIENT: close connection and stop~n", []),
            gen_tcp:close(Socket)
    after
        200 -> loop(Socket, Player, Opponent)
    end.

recv(<<DataSize:16/integer, Data:DataSize/binary>>) ->
%    {Text, _} = proto:parse(string, Data),
%    Text.
    Data.

send(Socket, Data) ->
    DataSize = byte_size(Data),
    Packet = <<DataSize:16/integer, Data/binary>>,
    gen_tcp:send(Socket, Packet).


analyze(_ServiceByte, _SubserviceByte, ?error, Msg, _Player, _Opponent) ->
    io:format("CLIENT: got error message: ~s~n", [Msg]);

analyze(?service_auth, ?sc_login_reply, ?ok, Msg, #player{username = Name, hp = Hp, hp_max = HpMax}, _Opponent) ->
    io:format("CLIENT: got message: ~s,~nyour name = ~s, your hp_max = ~p, your hp = ~p ~n", [Msg, Name, Hp, HpMax]);

analyze(?service_lobby, ?sc_find_opponent_reply, ?ok, Msg, _Player = #player{username = Name, hp = Hp, hp_max = HpMax},
    _Opponent = #player{username = Name1, hp = Hp1, hp_max = HpMax1}) ->
    io:format("CLIENT: got message: ~s~n", [Msg]),
    io:format("CLIENT: your name = ~s, your hp_max = ~p, your hp = ~p found opponent name = ~s, hp_max = ~p, hp = ~p~n", 
        [Name, HpMax, Hp, Name1, HpMax1, Hp1]);

analyze(?service_match, ?sc_attack_reply, ?ok, Msg, _Player = #player{username = Name, hp = Hp, hp_max = HpMax}, 
    _Opponent = #player{username = Name1, hp = Hp1, hp_max = HpMax1}) ->
    io:format("CLIENT: got message: ~s~n", [Msg]),
    io:format("CLIENT: your name = ~s, your hp_max = ~p, your hp = ~p, your opponent name = ~s, hp_max = ~p, hp = ~p~n", 
        [Name, HpMax, Hp, Name1, HpMax1, Hp1]).
