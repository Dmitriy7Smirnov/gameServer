%% @author: Dima
%% @date: 08.08.2019

-module(client).

%% Include files

%% Exported Functions

-export([
    start/0,
    start/2,
    login/2,
    send_text/1,
    stop/0,
    client/2,
    start_battle/1
]).

start() ->
    start("localhost", 1234),
    timer:sleep(infinity).

start(Host, Port) ->
    Pid = spawn(?MODULE, client, [Host, Port]),
    register(client, Pid).

login(Username, Password) ->
    client ! {login, Username, Password},
    ok.

start_battle(Arms) ->
    client ! {start_battle, Arms},
    ok.

send_text(Text) ->
    client ! {text, Text},
    ok.

stop() ->
    client ! stop,
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
