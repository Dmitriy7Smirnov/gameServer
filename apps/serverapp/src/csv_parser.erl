%% @author: Dima
%% @date: 18.07.2019

-module(csv_parser).

-export([ 
          parse_file/0
]).


%entry point
parse_file() ->
    UnicodeDataFromFile = get_data_from_file(), 
    ListsOfStrings = strings_lo_lists(UnicodeDataFromFile, [], [], [], false, 0),
    [H|T] = lists_size_validation(ListsOfStrings),
    TupleLists = lists_to_tuples(H, T, []),
    lists_tuples_to_maps(TupleLists).

%get data from files
get_data_from_file() ->
    Filename = filename:join([code:priv_dir(myapp), "test.txt"]),
    {ok, Data} = file:read_file(Filename),
    unicode:characters_to_list(Data).


% function split data to lists of strings
%clause end of the data
strings_lo_lists([],_ItemList, _ResultList, FinalList, _InTheQuote, _LastSymbol) -> FinalList;

%clause new line without quotes (drop it)
strings_lo_lists([$\r|T], [], [], FinalList, false, _LastSymbol) -> strings_lo_lists(T, [], [], FinalList, false, $\r);
strings_lo_lists([$\n|T], [], [], FinalList, false, _LastSymbol) -> strings_lo_lists(T, [], [], FinalList, false, $\n);

%clause \ symbol in quotes was found
strings_lo_lists([$\\|T], ItemList, ResultList, FinalList, InQuotes, $\\) -> strings_lo_lists(T, ItemList ++ $\\, ResultList, FinalList, InQuotes, $\\);
strings_lo_lists([$\\|T], ItemList, ResultList, FinalList, InQuotes, _LastSymbol) -> strings_lo_lists(T, ItemList, ResultList, FinalList, InQuotes, $\\);

%clause quote symbol was found
strings_lo_lists([$"|T], [], ResultList, FinalList, false, _LastSymbol) -> strings_lo_lists(T, [], ResultList, FinalList, true, $");
strings_lo_lists([$"|T], ItemList, ResultList, FinalList, true, LastSymbol) when LastSymbol =:= $\\ -> strings_lo_lists(T, ItemList ++ [$"], ResultList, FinalList, true, $"); 
strings_lo_lists([$"|T], ItemList, ResultList, FinalList, true, _LastSymbol) -> strings_lo_lists(T, ItemList, ResultList, FinalList, false, $");

%clause any symbol in quotes was found
strings_lo_lists([H|T], ItemList, ResultList, FinalList, true, _LastSymbol) -> strings_lo_lists(T, ItemList ++ [H], ResultList, FinalList, true, H); 

%clause symbol new line was found without quoutes
strings_lo_lists([$\r|T], ItemList, ResultList, FinalList, false, _LastSymbol) -> strings_lo_lists(T, [], [], FinalList ++ [ResultList ++ [ItemList]], false, $\r);
strings_lo_lists([$\n|T], ItemList, ResultList, FinalList, false, _LastSymbol) -> strings_lo_lists(T, [], [], FinalList ++ [ResultList ++ [ItemList]], false, $\n);

%clause symbol ; was found without quoutes
strings_lo_lists([$;|T], ItemList, ResultList, FinalList, false, _LastSymbol)  -> strings_lo_lists(T, [], ResultList ++ [ItemList], FinalList, false, $;);

%clause any sybol was found without quotes 
strings_lo_lists([H|T], ItemList, ResultList, FinalList, false, _LastSymbol) -> strings_lo_lists(T, ItemList ++ [H], ResultList, FinalList, false, H).



%check columns number
lists_size_validation([]) -> [];
lists_size_validation(ListsOfLists = [H|_]) ->
    TitleLength = length(H),
    [ List || List <- ListsOfLists, length(List) =:= TitleLength ].

%forms lists of { title, value } tuples  
lists_to_tuples(_, [], ResultList) -> ResultList;
lists_to_tuples(H, [HD|TD], ResultList) -> lists_to_tuples(H, TD, ResultList ++ [lists:zip(H, HD)]).
    
%forms lists of maps    
lists_tuples_to_maps(TupleLists) ->
    [ maps:from_list(X) || X <- TupleLists ]. 