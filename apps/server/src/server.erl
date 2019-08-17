%% @author: Dima
%% @date: 08.08.2019

-module(server).

%% Include files
-record(player_db_entry, {
    id,
    username,
    password
}).

-record(player, {
    socket,
    id,
    username,
    hp,
    hp_max
}).

-record(lobby, {
    players :: list()
}).

%% Exported Functions
-export([
    start/0,
    start/1,
    server/1,
    accept/2
]).

start() ->
    start(1234),
    receive
        _Msg -> 77
    end.

start(Port) ->
    Pid = spawn(?MODULE, server, [Port]),
    register(manager, Pid),
    ok.

server(Port) ->
    io:format("SERVER: listening on port ~p...~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, raw}]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, 5)],
    manager(#lobby{players = []}),
    timer:sleep(infinity),
    ok.

manager(LobbyState = #lobby{players = Players}) ->
    receive
        {add, Player} -> 
            Players1 = Player ++ Players, 
            lists:map(fun(Elem = #player{username = Username}) -> 
                io:format("player ~p awaiting battle ~n", [Username]), Elem end, Players1),
            manager(#lobby{players = Players1})
    after
        200 -> manager(LobbyState)
    end.

accept(Id, ListenSocket) ->
    io:format("SERVER: socket #~p waiting for client~n", [Id]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("SERVER: socket #~p, session started~n", [Id]),
    handle_connection(#player{socket = Socket}),
    % io:format("SERVER: socket #~p, session closed~n", [Id]),
    accept(Id, ListenSocket).

recv(Socket) ->
    case gen_tcp:recv(Socket, 2) of
        {ok, <<DataSize:16/integer>>} ->
            case gen_tcp:recv(Socket, DataSize) of
                {ok, Data} -> {ok, Data};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

send(Socket, Data) ->
    DataSize = byte_size(Data),
    Packet = <<DataSize:16/integer, Data/binary>>,
    gen_tcp:send(Socket, Packet).

handle_connection(#player{socket = Socket, id = Id} = PlayerState) ->
    case recv(Socket) of
        % Not logged in
        {ok, Data} when Id =:= undefined ->
            io:format("handle_connection pid = ~p and socket = ~p ~n", [self(), Socket]),
            {_ServiceByte, Rest} = proto:parse(uint8, Data),
            {_SubserviceByte, Rest1} = proto:parse(uint8, Rest),
            {Username, Rest2} = proto:parse(string, Rest1),
            {Password, _Rest3} = proto:parse(string, Rest2),
            case authenticate(Username, Password) of
                {ok, #player_db_entry{id = UserId}} ->
                    io:format("SERVER: user [#~B] ~s logged in successfully~n", [UserId, Username]),
                    Reply = proto:pack(string, <<"OK">>),
                    send(Socket, Reply), 
                    PlayerState1 = PlayerState#player{id = UserId, username = Username},
                    manager ! {add, [PlayerState1]},
                    handle_connection(PlayerState1);
                {error, Reason} ->
                    io:format("SERVER: user ~s login failed. Reason: ~p~n", [Username, Reason]),
                    Reply = proto:pack(string, atom_to_binary(Reason, utf8)),
                    send(Socket, Reply)
            end;

        % Logged in
        {ok, Data} ->
            {ServiceByte, Rest} = proto:parse(uint8, Data),
            {SubserviceByte, Rest1} = proto:parse(uint8, Rest),
            {Text, _Rest2} = proto:parse(string, Rest1),
            analize(ServiceByte, SubserviceByte, Text),
            %{Text, _} = proto:parse(string, Data),
            io:format("SERVER: message from client: ~s~n", [Text]),
            Reply = proto:pack(string, <<"RECV OK">>),
            send(Socket, Reply),
            handle_connection(PlayerState);

        % Error
        {error, Reason} ->
            {error, Reason}
    end.


analize(130, 2, Data) ->
    io:format("player wants to start battle with arms = ~p~n", [Data]),
    4;
analize(_, _, _) ->
    io:format("unknown info", []),
    5.

players() ->
    #{
        <<"dima">> => #player_db_entry{
            id = 1,
            username = <<"dima">>,
            password = <<"123">>
        },
        <<"max">> => #player_db_entry{
            id = 2,
            username = <<"max">>,
            password = <<"345">>
        }
    }.

authenticate(Username, Password) ->
    case maps:get(Username, players(), undefined) of
        #player_db_entry{username = Username, password = Password} = E -> {ok, E};
        #player_db_entry{} -> {error, invalid_password};
        undefined -> {error, invalid_username}
    end.