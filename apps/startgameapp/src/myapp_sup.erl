%% @author: Dima
%% @date: 13.07.2019

-module(myapp_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0,
    start_child/1
]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
        {ok, _MySupervisor} = supervisor:start_link({local, ?MODULE}, ?MODULE, []).
        

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(_Args) ->
    SupervisorSpecification = #{
        strategy => simple_one_for_one,
        intensity => 10000,
        period => 60},

    ChildSpecifications =
        [#{id => gen_server_1,
           start => {gen_server_1, start_link, [?MODULE]},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [gen_server_1]}
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_child(Fighters_number) ->
%    {ok, Child3Pid} = supervisor:start_child(?MODULE, #{id => gen_server_3,
%$           supervisor:start_child(?MODULE, ['1']),
           [supervisor:start_child(?MODULE, [list_to_atom([Id]), Fighters_number]) ||
                                                                 Id <-lists:seq(1, Fighters_number) ].


