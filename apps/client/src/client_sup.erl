%% @author: Dima
%% @date: 07.08.2019

-module(client_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10000,
        period => 60},

    ChildSpecifications =
        [
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




