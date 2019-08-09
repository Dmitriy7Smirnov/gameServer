%% @author: Dima
%% @date: 07.08.2019

-module(clientapp).

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
%    case clientsup:start_link() of
%        {ok, Pid} ->
%            {ok, Pid};
%        Error ->
%            Error
%    end.
Pid = spawn(client, start, []),
{ok, Pid}.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================




