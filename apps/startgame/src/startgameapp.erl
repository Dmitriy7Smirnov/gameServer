%% @author: Dima
%% @date: 07.08.2019

-module(startgameapp).

-behaviour(application).

%% Application callbacks
-export([
    start/2, 
    stop/1
]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    case startgamesup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


