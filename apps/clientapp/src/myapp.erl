%% @author: Dima
%% @date: 13.07.2019

-module(myapp).

%% Include files

%% Exported Functions

-export([add/2, f/1, f1/1, f2/1
]).

%%%===================================================================
%%% API
%%%===================================================================

 f(N) ->
    lists:usort([{A, B, C} || A<-lists:seq(1,N), B<-lists:seq(1,N), C<-lists:seq(1,N), D<-lists:seq(1,N),
             A<B, B<C, A*A+B*B==C*C, 
            ((A rem D =/= 0) or (B rem D =/= 0) or (C rem D =/= 0))]).


f1(N) ->
    lists:usort([{A, B, C} || A<-lists:seq(1,N), B<-lists:seq(1,N), C<-lists:seq(1,N), D<-lists:seq(1,N),
             A<B, B<C, A*A+B*B==C*C]).


f2(N) ->
    [{A, B, C} || A<-lists:seq(1,N), B<-lists:seq(1,N), C<-lists:seq(1,N), D<-lists:seq(1,N),
             A<B, B<C, A*A+B*B==C*C].

%%%===================================================================
%%% Internal functions
%%%===================================================================


add(X,Y) ->
    X+Y.



