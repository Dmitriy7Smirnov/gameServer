%% @author: Maxim Pushkar
%% @date: 20.07.2019

-module(w_max).

-record(state, {
    cols = [],
    value = [],
    quoting = false
}).

%% Exported Functions
-export([
    parse_file/0
]).

%% API
parse_file() ->
    Filename = filename:join([code:priv_dir(myapp), "test.txt"]),
    {ok, Data} = file:read_file(Filename),
    UnicodeData = unicode:characters_to_list(Data),
    ParsedData = [Line || Line <- parse(UnicodeData), Line =/= []],
    [Headings | RestData] = ParsedData,
    HeadingsLength = length(Headings),
    Result = [maps:from_list(lists:zip(Headings, Values)) || Values <- RestData, length(Values) =:= HeadingsLength].
%    [begin
%        [io:format("~ts => ~ts~n", [H, V]) || {H, V} <- maps:to_list(Map)],
%        io:format("===========================================~n", [])
%    end || Map <- Result].

%% Internal functions
parse(Data) ->
    parse(Data, []).

parse([], Lines) ->
    lists:reverse(Lines);
parse(Data, Lines) ->
    {Rest, #state{cols = Cols}} = parse_entry(Data, #state{}),
    parse(Rest, [Cols|Lines]).

% Handle line endings
parse_entry([], #state{quoting = false, cols = C, value = V} = S) ->
    {[], S#state{cols = lists:reverse([lists:reverse(V)|C]), value = []}};
parse_entry([$\r, $\n | Rest], #state{quoting = false, cols = C, value = V} = S) ->
    {Rest, S#state{cols = lists:reverse([lists:reverse(V)|C]), value = []}};
parse_entry([$\r | Rest], #state{quoting = false, cols = C, value = V} = S) ->
    {Rest, S#state{cols = lists:reverse([lists:reverse(V)|C]), value = []}};
parse_entry([$\n | Rest], #state{quoting = false, cols = C, value = V} = S) ->
    {Rest, S#state{cols = lists:reverse([lists:reverse(V)|C]), value = []}};

% Handle quoting
parse_entry([$\\, $" | Rest], #state{quoting = true, value = V} = S) ->
    parse_entry(Rest, S#state{value = [$"|V]});
parse_entry([$" | Rest], #state{quoting = false} = S) ->
    parse_entry(Rest, S#state{quoting = true, value = []});
parse_entry([$" | Rest], #state{quoting = true} = S) ->
    parse_entry(Rest, S#state{quoting = false});

% Handle column separators
parse_entry([$; | Rest], #state{quoting = false, cols = C, value = V} = S) ->
    parse_entry(Rest, S#state{cols = [lists:reverse(V)|C], value = []});

% Handle data
parse_entry([Char | Rest], #state{value = V} = S) ->
    parse_entry(Rest, S#state{value = [Char|V]}).
