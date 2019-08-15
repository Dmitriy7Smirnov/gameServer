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
    username
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
    spawn(?MODULE, server, [Port]),
    ok.

server(Port) ->
    io:format("SERVER: listening on port ~p...~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, raw}]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, 5)],
    timer:sleep(infinity),
    ok.

accept(Id, ListenSocket) ->
    % io:format("SERVER: socket #~p waiting for client~n", [Id]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    % io:format("SERVER: socket #~p, session started~n", [Id]),
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
            {Username, Rest} = proto:parse(string, Data),
            {Password, _Rest1} = proto:parse(string, Rest),
            case authenticate(Username, Password) of
                {ok, #player_db_entry{id = UserId}} ->
                    io:format("SERVER: user [#~B] ~s logged in successfully~n", [UserId, Username]),
                    Reply = proto:pack(string, <<"OK">>),
                    send(Socket, Reply),
                    handle_connection(PlayerState#player{id = UserId, username = Username});
                {error, Reason} ->
                    io:format("SERVER: user ~s login failed. Reason: ~p~n", [Username, Reason]),
                    Reply = proto:pack(string, atom_to_binary(Reason, utf8)),
                    send(Socket, Reply)
            end;

        % Logged in
        {ok, Data} ->
            {Text, _} = proto:parse(string, Data),
            io:format("SERVER: message from client: ~s~n", [Text]),
            Reply = proto:pack(string, <<"RECV OK">>),
            send(Socket, Reply),
            handle_connection(PlayerState);

        % Error
        {error, Reason} ->
            {error, Reason}
    end.

players() ->
    #{
        <<"dima">> => #player_db_entry{
            id = 1,
            username = <<"dima">>,
            password = <<"123">>
        }
    }.

authenticate(Username, Password) ->
    case maps:get(Username, players(), undefined) of
        #player_db_entry{username = Username, password = Password} = E -> {ok, E};
        #player_db_entry{} -> {error, invalid_password};
        undefined -> {error, invalid_username}
    end.