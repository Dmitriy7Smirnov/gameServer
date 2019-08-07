%% @author: Maxim Pushkar
%% @date: 21.07.2019

-module(fight).

-record(fighter, {
    start_life,
    curr_life,
    enemies = [],
    manager_id
}).

-export([
    start_fight_manager/0
]).

start_fight_manager() ->
    proc_lib:spawn(fun fight_manager/0).

fight_manager() ->
    EnemiesList = [proc_lib:spawn(fight, loop, [#fighter{start_life = 100, curr_life = 100, enemies = [], manager_id = self()}]) || X <- lists:seq(1,100)],
    send_enemies_list(EnemiesList).

send_enemies_list([]) -> done;
send_enemies_list([H|T]) ->
    H ! {enemies, T},
    send_enemies_list(T).

loop(#fighter{curr_life = CurrLife} = State) ->
    receive
        {fight, ImpactForce} ->
            if 
                CurrLife =< ImpactForce  ->
                    proc_lib:stop(self());
                CurrLife > ImpactForce ->
                    loop(State#fighter{curr_life = CurrLife - ImpactForce})
            end;
        _ ->
            loop(State)
%           #fighter{_, CurrLife, _, _} ,
            
    end.
    


