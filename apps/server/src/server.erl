%% @author: Dima
%% @date: 08.08.2019

-module(server).

%% Include files

%% Exported Functions


-export([
    start/0,
    start/1,
    server/1,
    accept/2,
    auth_map/0
]).



start() ->
    start(1234),
    receive
        _Msg ->
            77
    end.

start(Port) ->
    spawn(?MODULE, server, [Port]),
    ok.

server(Port) ->
    io:format("start server at port ~p~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, raw}]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, 5)],
    timer:sleep(infinity),
    ok.

accept(Id, ListenSocket) ->
    io:format("Socket #~p wait for client~n", [Id]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Socket #~p, session started, input login:~n", [Id]),
    AuthState = auth_map(),
    handle_connection(Id, ListenSocket, Socket, AuthState).


handle_connection(Id, ListenSocket, Socket, AuthState) ->
    case gen_tcp:recv(Socket, 2) of
        {ok, Header} -> <<Size:16/integer>> = Header,
                        {ok, Msg} = gen_tcp:recv(Socket, Size),
                        io:format("Socket #~p got message: ~p~n", [Id, Msg]),
                        Msg1 = binary_to_list(Msg),
                        AuthState1 = case maps:find(islogined, AuthState) of
                            {ok, false} ->
                                case maps:find(logincorrect, AuthState) of 
                                    {ok, false} -> case maps:find(login, AuthState) of
                                                       {ok, Msg1} -> gen_tcp:send(Socket, <<"input password">>),
                                                       maps:update(logincorrect, true, AuthState);
                                                       _NotCorrect -> gen_tcp:send(Socket, <<"try login again:">>),
                                                        AuthState
                                                   end;
                                    {ok, true} -> case maps:find(password, AuthState) of
                                                       {ok, Msg1} -> gen_tcp:send(Socket, <<"you are logined">>),
                                                       AuthStateTemp = maps:update(passwordcorrect, true, AuthState),
                                                       maps:update(islogined, true, AuthStateTemp);
                                                       _NotCorrect -> gen_tcp:send(Socket, <<"try password again:">>),
                                                       AuthState
                                                   end         
                                end;
                             {ok, true} -> gen_tcp:send(Socket, <<"You are logged yet">>),
                                           AuthState;
                                    _Any ->
                                           AuthState
                         end,   
                        handle_connection(Id, ListenSocket, Socket, AuthState1);
        {error, closed} ->
            io:format("Socket #~p, session closed ~n", [Id]),
            accept(Id, ListenSocket)
    end.


auth_map() -> #{
        login => "Maksik",
        logincorrect => false,
        password => "kleviy",
        passwordcorrect => false,
        islogined => false
    
    }.

