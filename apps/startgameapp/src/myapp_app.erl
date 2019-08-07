%% @author: Dima
%% @date: 13.07.2019

-module(myapp_app).

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
    io:format("I am here~n", []),
    case myapp_sup:start_link() of
        {ok, Pid} ->
            io:format("And here I am ~n", []),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================




