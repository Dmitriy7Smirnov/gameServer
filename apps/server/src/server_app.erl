%% @author: Dima
%% @date: 07.08.2019

-module(server_app).

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
%    case serversup:start_link() of
%        {ok, Pid} ->
%            {ok, Pid};
%        Error ->
%            Error
%    end.
Pid = spawn(server, start, []),
{ok, Pid}.


stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================




