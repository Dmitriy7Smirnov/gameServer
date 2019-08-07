%% @author: Maxim Pushkar
%% @date: 21.07.2019

-module(arena).

%% Exported Functions
-export([
    start/1,
    fighter/2
]).

%% Macros
-define(delay, 100).

%% Records
-record(manager_state, {
    players :: [pid()],
    monitors :: [reference()]
}).

-record(player_state, {
    manager :: pid(),
    name :: binary(),
    hp :: non_neg_integer(),
    max_hp :: non_neg_integer(),
    opponents :: ordsets:ordset(),
    monitors :: [reference()]
}).

%% API
start(PlayerCount) ->
    manager(PlayerCount).

%% Internal functions
manager(PlayerCount) ->
    io:format("Manager announces arena deathmatch for ~p opponents...~n", [PlayerCount]),
    % PlayerPids = [proc_lib:spawn(?MODULE, fighter, [self(), Index]) || Index <- lists:seq(1, PlayerCount)],
    PlayerPids = [spawn(?MODULE, fighter, [self(), Index]) || Index <- lists:seq(1, PlayerCount)],
    Monitors = [begin
        OpponentPids = [P || P <- PlayerPids, P =/= Pid],
        Pid ! {start, OpponentPids},
        erlang:monitor(process, Pid)
    end || Pid <- PlayerPids],
    manager_loop(#manager_state{players = PlayerPids, monitors = Monitors}).

manager_loop(#manager_state{players = PlayerPids, monitors = Monitors} = State) ->
    receive
        {hit, AttackerName, Name, Damage} ->
            io:format("manager: ~s hits ~s with ~p damage...~n", [AttackerName, Name, Damage]),
            manager_loop(State);

        {'DOWN', MonitorRef, process, Pid, {death, PlayerName, AttackerName}} ->
            io:format("manager: ~s has been killed by ~s!~n", [PlayerName, AttackerName]),
            erlang:demonitor(MonitorRef),
            manager_loop(State#manager_state{
                players = ordsets:del_element(Pid, PlayerPids),
                monitors = ordsets:del_element(MonitorRef, Monitors)
            });
        
        {'DOWN', MonitorRef, process, _Pid, {last_man_standing, PlayerName, HP, MaxHP}} ->
            erlang:demonitor(MonitorRef),
            io:format("manager: ~s is a winner, HP left: ~p of ~p!~n", [PlayerName, HP, MaxHP])
    after
        5000 ->
            io:format("manager: 5 sec passed~n", []),
            manager_loop(State)
    end.

fighter(ManagerPid, Number) ->
    HP = random_hp(),
    State = #player_state{
        manager = ManagerPid,
        name = <<"Player ", (integer_to_binary(Number))/binary>>,
        hp = HP,
        max_hp = HP
    },
    fighter_loop(State).

fighter_loop(#player_state{manager = ManagerPid, name = Name, hp = HP, max_hp = MaxHP, opponents = OpponentsPids, monitors = Monitors} = State) ->
    receive
        {start, Pids} ->
            MonitorRefs = [erlang:monitor(process, Pid) || Pid <- Pids],
            erlang:send_after(?delay, self(), attack),
            fighter_loop(State#player_state{opponents = Pids, monitors = MonitorRefs});

        attack when OpponentsPids =:= [] ->
            exit({last_man_standing, Name, HP, MaxHP});

        attack ->
            OpponentPid = random_element(OpponentsPids),
            OpponentPid ! {hit, random_damage(), Name},
            erlang:send_after(?delay, self(), attack),
            fighter_loop(State);

        {hit, Damage, AttackerName} when HP > Damage ->
            ManagerPid ! {hit, AttackerName, Name, Damage},
            fighter_loop(State#player_state{hp = HP - Damage});

        {hit, _Damage, AttackerName} ->
            exit({death, Name, AttackerName});

        {'DOWN', MonitorRef, process, Pid, _Reason} ->
            erlang:demonitor(MonitorRef),
            fighter_loop(State#player_state{
                opponents = ordsets:del_element(Pid, OpponentsPids),
                monitors = ordsets:del_element(MonitorRef, Monitors)
            })
    end.

random_hp() ->
    74 + rand:uniform(26).

random_damage() ->
    rand:uniform(5).

random_element(List) ->
    lists:nth(rand:uniform(length(List)), List).