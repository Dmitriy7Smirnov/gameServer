%% @author: Dima
%% @date: 22.07.2019

-module(pyth_triples).

-export([
    unique_pyth_triples1/1,
    unique_pyth_triples2/1,
    pyth_tri/1,
    pyth_tri2/1
]).


unique_pyth_triples1(N) ->
    List = [{A, B, C} || A <- lists:seq(1,N), B <- lists:seq(A,N), C <- lists:seq(B,N), A=<B, A*A+B*B =:= C*C],
    % io:format("all triples~n~p~n unique triples~n", [List]),
    [{A, B, C} || {A, B, C} <- List, 
        lists:all(fun({A1, B1, _C1}) -> if (A =:= A1) and (B =:= B1) -> true;
         (A1/A =:= B1/B) and (A>A1) -> false; true -> true end end, List)].


unique_pyth_triples2(N) ->
    List = [{A, B, C} || A <- lists:seq(1,N), B <- lists:seq(A,N), C <- lists:seq(B,N), A=<B, A*A+B*B =:= C*C],
    % io:format("all triples~n~p~n unique triples~n", [List]),
    [{A, B, C} || {A, B, C} <- List, lists:all(fun({A1, B1, _C1}) -> not ((A>A1) and (A1/A =:= B1/B)) end, List)].

pyth_tri(N) ->
    Triplets = [lists:usort([A, B, C]) || A <- lists:seq(1, N), B <- lists:seq(1, N), C <- lists:seq(1, N), C*C =:= A*A + B*B],
%    Triplets = [[A, B, C] || A <- lists:seq(1, N), B <- lists:seq(1, N), C <- lists:seq(1, N), D<-lists:seq(1,A), A=<B, B=<C, C*C =:= A*A + B*B, A rem D =/= 0, B rem D =/=0, C rem D =/=0 ].
    Triplets1 = [list_to_tuple(TL) || TL <- lists:usort(Triplets)],
    Triplets2 = lists:foldl(fun(Elem, UL) ->
        case is_unique(Elem, UL) of
            true -> ordsets:add_element(Elem, UL);
            false -> UL
        end
    end, [], Triplets1).
%    io:format("*** ~p unique triplets for N = ~p~n", [length(Triplets2), N]),
%    Triplets2.

pyth_tri2(N) ->
    [{A, B, C} || A <- lists:seq(1, N), B <- lists:seq(min(A + 1, N), N), C <- lists:seq(min(B + 1, N), N), A =< B, A*A + B*B =:= C*C, gcd(A, B) == 1].
    % [{A, B, C} || {A, B, C} <- Triplets, lists:all(fun({A1, B1, _C1}) -> not ((A > A1) and (A1 / A =:= B1 / B)) end, Triplets)].
    % [{A, B, C} || {A, B, C} <- Triplets, gcd(A, B) == 1].

is_unique(_, []) -> true;
is_unique({A, B, C}, [{AS, BS, CS}|_]) when A / AS =:= B / BS, A / AS =:= C / CS -> false;
is_unique({A, B, C}, [_|T]) -> is_unique({A, B, C}, T).

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).