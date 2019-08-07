%% @author: Dima
%% @date: 15.07.2019

-module(m).

%% Include files

%% Exported Functions

-export([f/1, f1/0, myfil/1, f2/0, f3/0, f4/3
]).

%%%===================================================================
%%% API
%%%===================================================================

f(N) ->
    [X || X <- lists:seq(1, 9)].


myfil(X) ->
    X rem 2 == 0.

f1() ->
    L = f(9),
    Myfil = fun(X) -> X rem 2 == 0 end, 
    lists:filter(Myfil, L).


f2() ->
    lists:dropwhile(fun(D) -> D=:=0 end, [X || <<X:1>> <= <<5:10>>]) .

f3() ->
    lists:foldl(fun(X, [Sum, Sum]) -> Sum + X end, [0, 0], [1, 2, 3, 4, 5]).

f4(List, Max, Curr) ->
    case List of
       [] -> if Curr > Max -> Curr; true -> Max end;
       [0|T] -> if Curr > Max -> f4(T, Curr, 0); true -> f4(T, Max, 0) end;
       [1|T] -> f4(T, Max, Curr +1)
    end.






%%%===================================================================
%%% Internal functions
%%%===================================================================





