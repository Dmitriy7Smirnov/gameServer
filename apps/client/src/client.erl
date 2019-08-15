%% @author: Dima
%% @date: 08.08.2019

-module(client).

%% Include files

%% Exported Functions

-export([
    start/0,
    start/2,
    login/3,
    send_text/2,
    stop/1,
    client/2
]).

start() ->
    start("localhost", 1234),
    timer:sleep(infinity).

start(Host, Port) ->
    spawn(?MODULE, client, [Host, Port]).

login(Pid, Username, Password) ->
    Pid ! {login, Username, Password},
    ok.

send_text(Pid, Text) ->
    Pid ! {text, Text},
    ok.

stop(Pid) ->
    Pid ! stop,
    ok.

client(Host, Port) ->
    io:format("CLIENT: pid = ~p connects to ~s:~p~n", [self(), Host, Port]),
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, raw}]),
    loop(Socket).

loop(Socket) ->
    receive
        {login, Username, Password} ->
            PackedUsername = proto:pack(string, Username),
            PackedPassword = proto:pack(string, Password),
            send(Socket, <<PackedUsername/binary, PackedPassword/binary>>),
            io:format("CLIENT: trying to login as \"~s\" with password \"~s\"~n", [Username, Password]),
            loop(Socket);
        {text, Text} ->
            send(Socket, proto:pack(string, Text)),
            io:format("CLIENT: sending text ~s~n", [Text]),
            loop(Socket);
        {tcp, Socket, Msg} ->
            Text = recv(Msg),
            io:format("CLIENT: got message: ~s~n", [Text]),
            loop(Socket);
        stop ->
            io:format("CLIENT: close connection and stop~n", []),
            gen_tcp:close(Socket)
    after
        200 -> loop(Socket)
    end.

recv(<<DataSize:16/integer, Data:DataSize/binary>>) ->
    {Text, _} = proto:parse(string, Data),
    Text.

send(Socket, Data) ->
    DataSize = byte_size(Data),
    Packet = <<DataSize:16/integer, Data/binary>>,
    gen_tcp:send(Socket, Packet).
