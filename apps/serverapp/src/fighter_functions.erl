%% @author: Dima
%% @date: 04.08.2019

-module(fighter_functions).

%% Include files

%% Exported Functions

-export([
    get_hp/0,
    get_impact_force/0,
    get_enemy/1
]).

get_hp() ->
    74 + rand:uniform(26).

get_impact_force() ->
    rand:uniform(5).

get_enemy(Enemy_list) ->
    N = rand:uniform(length(Enemy_list)),
    lists:nth(N, Enemy_list).





