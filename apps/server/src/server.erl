%% @author: Dima
%% @date: 08.08.2019
-module(server).

%% Include files
-include_lib("protocol/include/proto.hrl").
    
-record(player_db_entry, {
    id,
    username,
    password
}).

-record(lobby, {
    players :: list()
}).

-record(match, {
    fighter1,
    fighter2
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
    manager(#lobby{players = []}, #match{}),
    timer:sleep(infinity),
    ok.

manager(LobbyState = #lobby{players = Players}, MatchState = #match{fighter1 = Fighter1, fighter2 = Fighter2}) ->
    receive
        {add, Player} -> 
            Players1 = Player ++ Players, 
            lists:map(fun(Elem = #player{username = Username}) -> 
                io:format("player ~p awaiting battle ~n", [Username]), Elem end, Players1),
            manager(LobbyState#lobby{players = Players1}, MatchState);
        {find_opponent, Opponent1 = #player{socket = Socket, pid = Pid, id = Id, username = Name}} ->
            case length(LobbyState#lobby.players) of 
                0 -> 
                    Pid ! {error, no_opponents_list_empty},
                    LobbyError = "no opponetns, list empty",
                    send_reply(Socket, ?service_lobby, ?sc_find_opponent_reply, ?error, LobbyError, #player{}, #player{}),
                    manager(LobbyState, MatchState);
                1 ->
                    Pid ! {error, no_opponents_you_one_in_lobby} ,
                    LobbyError = "no opponetns, you one in the lobby", 
                    send_reply(Socket, ?service_lobby, ?sc_find_opponent_reply, ?error, LobbyError, #player{}, #player{}),
                    manager(LobbyState, MatchState);
                _ ->
                    Opponents = [Opponent || Opponent <- Players, Opponent#player.id  =/= Id],
                    Opponent2 = fighter_fun:get_enemy(Opponents),
                    Opponents1 = [Opponent || Opponent <- Players, Opponent#player.id  =/= Opponent2#player.id],
                    MatchState1 = MatchState#match{fighter1 = Opponent1, fighter2 = Opponent2},
                    MsgForFirstOpponent = io_lib:format("~s, opponent for you was found: ~s, you can start attack", [Name, Opponent2#player.username]),
                    MsgForSecondOpponent = io_lib:format("~s, opponent for you was found: ~s, you can start attack", [Opponent2#player.username, Name]),
                    send_reply(Socket, ?service_lobby, ?sc_find_opponent_reply, ?ok, MsgForFirstOpponent, Opponent1, Opponent2),
                    send_reply(Opponent2#player.socket, ?service_lobby, ?sc_find_opponent_reply, ?ok, MsgForSecondOpponent, Opponent2, Opponent1),
                    Pid ! {ok, you_have_an_opponent}, 
                    manager(LobbyState#lobby{players = Opponents1}, MatchState1)
            end;
        {attack, Who} ->
            case get_opponent(Fighter1, Fighter2, Who) of 
                {ok, Whom} ->
                    ImpactForce = fighter_fun:get_impact_force(),
                    io:format("SERVER: ~s attacked ~s with hp = ~p and impact force = ~p ~n", 
                        [Who#player.username, Whom#player.username, Who#player.hp, ImpactForce]),
                    MsgForFirstOpponent = io_lib:format("SERVER: ~s attacked ~s with hp = ~p and impact force = ~p ~n", 
                        [Who#player.username, Whom#player.username, Who#player.hp, ImpactForce]),
                    MsgForSecondOpponent = io_lib:format("SERVER: ~s attacked ~s with hp = ~p and impact force = ~p ~n", 
                        [Who#player.username, Whom#player.username, Who#player.hp, ImpactForce]),    
                    Whom1 = Whom#player{hp = Whom#player.hp - ImpactForce},
                    send_reply(Who#player.socket, ?service_match, ?sc_attack_reply, ?ok, MsgForFirstOpponent, Who, Whom1),
                    send_reply(Whom#player.socket, ?service_match, ?sc_attack_reply, ?ok, MsgForSecondOpponent, Whom1, Who),
                    manager(LobbyState, MatchState#match{fighter1 = Who, fighter2 = Whom1});
                {error, Reason} ->
                    io:format("SERVER: you haven't opponent yet", [])
            end,
            manager(LobbyState, MatchState)
    after
        200 -> manager(LobbyState, MatchState)
    end.

get_opponent(Fighter1, Fighter2, Opponent) when Fighter2 =:= Opponent -> {ok, Fighter1};
get_opponent(Fighter1, Fighter2, Opponent) when Fighter1 =:= Opponent -> {ok, Fighter2};
get_opponent(_Fighter1, _Fighter2, _Opponent) -> {error, opponent_not_found}.

accept(Id, ListenSocket) ->
    io:format("SERVER: socket #~p waiting for client~n", [Id]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("SERVER: socket #~p, session started~n", [Id]),
    handle_connection(#player{socket = Socket, pid = self()}),
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

send_reply(Socket, ServiceByteIn, SubserviceByteIn, ReplyStatusByteIn, Msg, Player, Opponent) ->
    ServiceByte = proto:pack(uint8, ServiceByteIn),
    SubserviceByte = proto:pack(uint8, SubserviceByteIn),
    ReplyStatusByte = proto:pack(uint8, ReplyStatusByteIn),
    ReplyMsg = proto:pack(string, list_to_binary(Msg)),
    PackedPlayer = generic_proto:pack(player, Player),
    PackedOpponent = generic_proto:pack(player, Opponent),
    PackedData = <<ServiceByte/binary, SubserviceByte/binary, ReplyStatusByte/binary, ReplyMsg/binary,PackedPlayer/binary, 
        PackedOpponent/binary>>,
    send(Socket, PackedData).

handle_connection(#player{socket = Socket, id = _Id} = PlayerState) ->
    case recv(Socket) of
            
        {ok, Data} ->
            io:format("SERVER: Socket = ~p~n", [Socket]),
            {ServiceByte, RestData} = proto:parse(uint8, Data),
            {SubserviceByte, RestData1} = proto:parse(uint8, RestData),
            case analyze(ServiceByte, SubserviceByte, RestData1, PlayerState) of
                {ok, PlayerState1} ->
                    handle_connection(PlayerState1);
                {error, Reason} ->
                    {error, Reason}
            end;
                
        % Error
        {error, Reason1} ->
            {error, Reason1}
    end.



analyze(?service_auth, ?cs_login, Data, #player{socket = Socket, id = 0} = PlayerState) ->
    % Not logged in
    io:format("handle_connection pid = ~p and socket = ~p ~n", [self(), Socket]),
    {Username, Rest} = proto:parse(string, Data),
    {Password, _Rest1} = proto:parse(string, Rest),
    case authenticate(Username, Password) of
        {ok, #player_db_entry{id = UserId, username = Name}} ->
            io:format("SERVER: user ~s login successfully ~n", [Username]),
            Hp = fighter_fun:get_hp(), 
            PlayerState1 = PlayerState#player{id = UserId, username = Name, hp = Hp, hp_max = Hp},
            manager ! {add, [PlayerState1]},
            Msg = "you logged in succesfully",
            send_reply(Socket, ?service_auth, ?sc_login_reply, ?ok, Msg, PlayerState1, #player{}),
            {ok, PlayerState1};
        {error, Reason} ->
            io:format("SERVER: user ~s login failed. Reason: ~p~n", [Username, Reason]),
            Msg = "invalid username",
            send_reply(Socket, ?service_auth, ?sc_login_reply, ?error, Msg, #player{}, #player{}),
            {error, Reason}
    end;
analyze(?service_auth, ?cs_login, _Data, #player{socket = Socket, id = _Id} = PlayerState) ->
    AuthError = "user logged in already.", 
    io:format("SERVER: ~p ~n", [AuthError]),
    send_reply(Socket, ?service_auth, ?sc_login_reply, ?error, AuthError, #player{}, #player{}),
    {ok, PlayerState};
analyze(ServiceByte, SubserviceByte, _Arms, #player{socket = Socket, id = 0} = PlayerState) ->
    AuthError = "you must login first", 
    io:format("SERVER: ~p ~n", [AuthError]),
    send_reply(Socket, ServiceByte, SubserviceByte, ?error, AuthError, #player{}, #player{}),
    {ok, PlayerState};
analyze(?service_lobby, ?cs_find_opponent, Arms, #player{socket = _Socket, id = _Id} = PlayerState) ->
    {Arms1, _Rest1} = proto:parse(string, Arms),
    io:format("SERVER: player wants to start battle with arms = ~s ~n", [Arms1]),
    manager ! {find_opponent, PlayerState},
    receive
        {ok, Opponent} ->
            io:format("SERVER: opponent was found = ~p ~n", [Opponent]),
            {ok, PlayerState};
        {error, Reason} ->
            io:format("SERVER: error: ~p ~n", [Reason]),
            {error, Reason}
    after 
        2000 -> 
            io:format("SERVER: waiting in lobby timeout"),
            {error, timeout}
    end,
    {ok, PlayerState};
analyze(?service_match, ?cs_attack, _Arms, #player{socket = _Socket, id = _Id} = PlayerState) ->
    manager ! {attack, PlayerState}, 
    {ok, PlayerState};
analyze(_, _, _, _) ->
    io:format("unknown info", []),
    {error, "unknown info ~n"}.

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
        },
        <<"user3">> => #player_db_entry{
            id = 3,
            username = <<"user3">>,
            password = <<"345">>
        },
        <<"user4">> => #player_db_entry{
            id = 4,
            username = <<"user4">>,
            password = <<"345">>
        },
        <<"user5">> => #player_db_entry{
            id = 5,
            username = <<"user5">>,
            password = <<"345">>
        }
    }.

authenticate(Username, Password) ->
    case maps:get(Username, players(), 0) of
        #player_db_entry{username = Username, password = Password} = E -> {ok, E};
        #player_db_entry{} -> {error, invalid_password};
        0 -> {error, invalid_username}
    end.

