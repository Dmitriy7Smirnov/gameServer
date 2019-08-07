%% @author: Dima
%% @date: 18.07.2019

-module(w).

-export([
    f1/0,
    f0/0,
    f3/0,
    f4/3,
    f5/0
]).

f1() ->
    Filename = filename:join([code:priv_dir(myapp), "test.txt"]),
    {ok, Data} = file:read_file(Filename),
    UnicodeData = unicode:characters_to_list(Data),
    f2(UnicodeData, [], [], [], false, 0).


f2([],_, _, FinalList, _, _) -> FinalList;
f2([$\n|T], [], [], FinalList, false, LastSymbol) -> f2(T, [], [], FinalList, false, 10);
f2([$\r|T], [], [], FinalList, false, LastSymbol) -> f2(T, [], [], FinalList, false, 13);
f2([$\"|T], [], ResultList, FinalList, false, LastSymbol) -> f2(T, [], ResultList, FinalList, true, 34);
f2([$\"|T], ItemList, ResultList, FinalList, true, LastSymbol) when LastSymbol =:= 92 -> f2(T, ItemList ++ [34], ResultList, FinalList, true, 34);
f2([$\"|T], ItemList, ResultList, FinalList, true, LastSymbol) -> f2(T, ItemList, ResultList, FinalList, false, 34);
f2([H|T], ItemList, ResultList, FinalList, true, LastSymbol) -> f2(T, ItemList ++ [H], ResultList, FinalList, true, H);
f2([$\r|T], ItemList, ResultList, FinalList, false, LastSymbol) -> f2(T, [], [], FinalList ++ [ResultList ++ [ItemList]], false, 13);
f2([H|T], ItemList, ResultList, FinalList, false, LastSymbol) when H =/= 59 -> f2(T, ItemList ++ [H], ResultList, FinalList, false, H);
f2([$;|T], ItemList, ResultList, FinalList, false, LastSymbol) -> f2(T, [], ResultList ++ [ItemList], FinalList, false, 59).


f0() ->
    MyList = [1,2,3,4,53],
    lists:last(MyList).

f3() ->
    MyList = [],
    MyList1 = ["1","2","3","4","5"],
    MyList2 = [6,7,8,9,0],
    %maps:put(40, 30, MyMap),
    f4(MyList, MyList1, MyList2).


f4(MyList, [], _) -> maps:from_list(MyList);
f4(MyList, _, []) -> maps:from_list(MyList);
f4(MyList, [HK|TK], [HV|TV]) -> f4(MyList ++ [{HK,HV}], TK, TV).
     

f5() ->
    TitleList= [2,3,4,6,7],
    Str1List = [6,7,8,9,0],
    Str2List = [10,20,30,40,50],
    Str3List = ["a", "B","C","d","F"],
    ResultList = [TitleList, Str1List, Str2List, Str3List],
    [H|T] = ResultList,
    f6(H,T, []).

f6(H, [], ResultList) -> ResultList;
f6(H, [HD|TD], ResultList) -> f6(H, TD, ResultList ++ [lists:zip(H, HD)]).
