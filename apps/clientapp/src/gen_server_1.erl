%% @author: Dima
%% @date: 03.08.2019

-module(gen_server_1).

-define(delay, 1000).

-behaviour(gen_server).

-export([
	start_link/3
]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3
         ]).

-record(fighter_state, {
    hp,
    hp_max,
    manager,
    name,
    enemies,
    monitors,
    wins
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Manager_id, Fighter_id, Fighters_number) -> 
    gen_server:start_link({local, Fighter_id}, ?MODULE, [Manager_id, Fighter_id, Fighters_number], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Manager_id, Fighter_id, Fighters_number]) ->
    Hp = fighter_functions:get_hp(),
    io:format("fighter was started with name = ~p and hp_max = ~p~n", [Fighter_id, Hp]),
    New_fighter_state = #fighter_state{hp = Hp, hp_max = Hp, manager = Manager_id,
                                                             name = Fighter_id, wins = 0},
    Fighter_id ! {continue_init_msg, Fighters_number},
    {ok, New_fighter_state}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



handle_cast({hit, Damage, Who}, State) when State#fighter_state.hp > Damage  ->
    New_hp = State#fighter_state.hp - Damage,
    io:format("~p hit ~p with damage ~p, my new hp = ~p~n", [Who, State#fighter_state.name, Damage, New_hp]),
    New_fighter_state = State#fighter_state{hp = New_hp}, 
    {noreply, New_fighter_state};
handle_cast({hit, Damage, Who}, State) ->
    io:format("~p kill ~p with damage ~p, my hp was ~p~n", [Who, State#fighter_state.name, 
                                                               Damage, State#fighter_state.hp]),
    gen_server:cast(Who, {kill, State#fighter_state.name}),
    exit(you_killed_me),
    {noreply, State};
handle_cast({kill, Whom}, State) when State#fighter_state.wins < 1 ->
    Wins = State#fighter_state.wins + 1,
    New_fighter_state = State#fighter_state{wins = Wins},
    io:format("~p wins ~p times killing ~p ~n", [State#fighter_state.name, Wins, Whom]),
    {noreply, New_fighter_state};
handle_cast({kill, Whom}, State) ->
    io:format("~p win the game wins = ~p killing ~p~n", 
        [State#fighter_state.name, State#fighter_state.wins + 1, Whom]),
%    [supervisor:terminate_child(State#fighter_state.manager, [list_to_atom([Id])]) ||
%                                                                 Id <-lists:seq(1, 3) ],
    application:stop(myapp),
    {noreply, State};
handle_cast(_Msg, State) ->
    io:format("unknown in handle_cast ~p~n", [_Msg]),
    {noreply, State}.

handle_info({continue_init_msg, Fighters_number}, State) ->
    io:format("~p init with hp = ~p~n", [State#fighter_state.name, State#fighter_state.hp]),
    Enemies = [list_to_atom([Enemy]) || Enemy <-lists:seq(1, Fighters_number),
               State#fighter_state.name =/= list_to_atom([Enemy])], 
    New_fighter_state = State#fighter_state{enemies = Enemies},
    erlang:send_after(?delay, State#fighter_state.name, attack_msg), 
    {noreply, New_fighter_state};
handle_info(attack_msg, State) ->
    Whom = fighter_functions:get_enemy(State#fighter_state.enemies),
    Damage = fighter_functions:get_impact_force(),
    io:format("~p will attack ~p with damage ~p ~n", [State#fighter_state.name, Whom, Damage]),
    gen_server:cast(Whom, {hit, Damage, State#fighter_state.name}), 
    erlang:send_after(?delay, State#fighter_state.name, attack_msg), 
    {noreply, State};
handle_info(_Msg, State) ->
    io:format("unknown in handle_info ~p~n", [_Msg]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




