%% @author: Dima
%% @date: 25.07.2019

-module(battle).

-define(delay, 100).

-export([
    manager_init/1,
    manager_loop/1,
    fighter_init/2,
    fighter_loop/1,
    get_hp/0,
    get_impact_force/0
]).

-record(fighter_state, {
    hp,
    hp_max,
    manager_id,
    fighter_name,
    enemies_id,
    monitors_id
}).

-record(manager_state, {
    manager_id,
    fighter_monitors,
    enemies_id
}).





manager_init(N) ->
    Manager_id = self(),
    Id_process_list = [spawn(?MODULE, fighter_init, [Manager_id, Name]) || Name <- lists:seq(1, N)],
    Monitor_list = [monitor(process, Id) || Id <- Id_process_list],
    Manager_state = #manager_state{manager_id = Manager_id, fighter_monitors = Monitor_list, enemies_id = Id_process_list},
    [From ! {init_enemies_id, Id_process_list} || From <- Id_process_list],
    [From ! {attack, ?delay} || From <- Id_process_list],
    manager_loop(Manager_state).

manager_loop(_Record_state) ->
    receive
        {created, Pid} ->
            io:format("~n process was created PID = ~p ~n", [Pid]),
            manager_loop(6);
        {attack, Who, Whom, Damage} ->
            io:format("~n ~p attacked ~p with damage ~p ~n", [Who, Whom, Damage]),
            manager_loop(3);
        {hit, Who, Whom, Damage} ->
            io:format("~n ~p hit ~p with damage ~p ~n", [Who, Whom, Damage]),
            manager_loop(3);
        {'DOWN', _MonitorRef, process, Object, killed} ->
            io:format("~n ~p was killed  ~n", [Object]), 
            manager_loop(4);
        {'DOWN', _MonitorRef, process, Object, {win, Fighter_hp, Fighter_hp_max} } ->
            io:format("~n ~p WIN with hp = ~p from ~p~n", [Object, Fighter_hp, Fighter_hp_max])
    after
        25000 ->
            io:format("~n game time is over ~n", [])
    end.


fighter_init(Manager_id, Fighter_name) ->
    timer:sleep(1000),
    Manager_id ! {created, self()},
    Hp = get_hp(),
    State = #fighter_state{fighter_name = Fighter_name, manager_id = Manager_id, hp = Hp, hp_max = Hp},
    fighter_loop(State).


fighter_loop(State) ->
    receive
        {init_enemies_id, Id_process_list} ->
            My_id = self(),
            Enemies_id = [Enemy || Enemy <- Id_process_list, Enemy =/= My_id],
            Monitors_id = [monitor(process, Pid) || Pid <- Id_process_list, Pid =/= My_id],
            New_state = State#fighter_state{enemies_id = Enemies_id, monitors_id = Monitors_id},
            fighter_loop(New_state);
        {attack, _Delay} when State#fighter_state.enemies_id =:= [] ->
            exit({win, State#fighter_state.hp, State#fighter_state.hp_max});
        {attack, Delay} ->
            Whom = get_enemy(State#fighter_state.enemies_id),
            Who = self(),
            Damage = get_impact_force(),
            Whom ! {hit, Who, Whom, Damage},
            erlang:send_after(Delay, Who, {attack, Delay}),
            %erlang:send_after(Delay, Whom, {hit, Who, Whom, Damage}),
            %self() ! {attack, Delay},
            fighter_loop(State);
        {'DOWN', MonitorRef, process, Object, killed} ->
            demonitor(MonitorRef),
            Enemies_id = State#fighter_state.enemies_id,
            fighter_loop(State#fighter_state{enemies_id = ordsets:del_element(Object, Enemies_id)});
        {hit, Who, Whom, Damage} 
            when State#fighter_state.hp > Damage ->
                New_hp = State#fighter_state.hp - Damage,
                New_state = State#fighter_state{hp = New_hp},
                State#fighter_state.manager_id ! {attack, Who, Whom, Damage},
                fighter_loop(New_state);
        {hit, _Who, _Whom, _Damage} ->
            exit(killed)    
            
    end.






get_hp() ->
    74 + rand:uniform(26).

get_impact_force() ->
    rand:uniform(5).

get_enemy(Enemy_list) ->
    N = rand:uniform(length(Enemy_list)),
    lists:nth(N, Enemy_list).
