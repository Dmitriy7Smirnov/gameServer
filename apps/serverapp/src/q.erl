%% @author: Dima
%% @date: 18.07.2019

-module(q).

%% Include files

%% Exported Functions

-export([ f1/0
]).

f1() ->
    {Result, Data} = file:read_file("C:/test.txt"),
     UnicodeData = unicode:characters_to_list(Data),
     f2(UnicodeData, [], [], [], false).




f2([],ItemList, ResultList, FinalList, _) -> FinalList;
f2([10|T], [], [], FinalList, false) -> f2(T, [], [], FinalList, false);
f2([13|T], [], [], FinalList, false) -> f2(T, [], [], FinalList, false);
f2([34|T], [], ResultList, FinalList, false) -> f2(T, [], ResultList, FinalList, true);
f2([34|T], ItemList, ResultList, FinalList, true) -> f2(T, ItemList, ResultList, FinalList, false);
f2([H|T], ItemList, ResultList, FinalList, true) -> f2(T, ItemList ++ [H], ResultList, FinalList, true); 
f2([13|T], ItemList, ResultList, FinalList, false) -> f2(T, [], [], FinalList ++ [ResultList ++ [ItemList]], false); %%ResultList ++ [ItemList];
f2([H|T], ItemList, ResultList, FinalList, false) when H =/= 59 -> f2(T, ItemList ++ [H], ResultList, FinalList, false);
f2([59|T], ItemList, ResultList, FinalList, false)  -> f2(T, [], ResultList ++ [ItemList], FinalList, false).

    