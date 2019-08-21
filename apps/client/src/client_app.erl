%% @author: Dima
%% @date: 07.08.2019

-module(client_app).

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
    spawn(client, start, [client1]),
    spawn(client, start, [client2]),
    spawn(client, start, [client3]),
    spawn(client, start, [client4]),    
    Pid = spawn(client, start, [client5]),
    {ok, Pid}.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================




