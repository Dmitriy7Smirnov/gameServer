%% @author: Dima
%% @date: 08.08.2019

-module(client).

%% Include files

%% Exported Functions

-export([
    start/1,
    start/3,
    login/3,
    send_text/2,
    stop/1,
    client/2,
    start_battle/2
]).

start(ClientName) ->
    start("localhost", 1234, ClientName),
    timer:sleep(infinity).

start(Host, Port, ClientName) ->
    Pid = spawn(?MODULE, client, [Host, Port]),
    register(ClientName, Pid).

login(ClientName, Username, Password) ->
    ClientName ! {login, Username, Password},
    ok.

start_battle(ClientName, Arms) ->
    ClientName ! {start_battle, Arms},
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
    loop(Socket).

loop(Socket) ->
    receive
        {login, Username, Password} ->
            ServiceByte = proto:pack(uint8, 129),
            SubserviceByte = proto:pack(uint8, 1),
            PackedUsername = proto:pack(string, Username),
            PackedPassword = proto:pack(string, Password),
            send(Socket, <<ServiceByte/binary, SubserviceByte/binary, PackedUsername/binary, PackedPassword/binary>>),
            io:format("CLIENT: trying to login as \"~s\" with password \"~s\"~n", [Username, Password]),
            loop(Socket);
        {start_battle, Arms} ->
            ServiceByte = proto:pack(uint8, 130),
            SubserviceByte = proto:pack(uint8, 2),
            PackedArms = proto:pack(string, Arms),
            send(Socket, <<ServiceByte/binary, SubserviceByte/binary, PackedArms/binary>>),
            io:format("CLIENT: sending start_battle with arms = ~p~n", [Arms]),    
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
