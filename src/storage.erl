%% @author Nora
%% @doc @todo Add description to storage.


-module(storage).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/0,add/3,lookup/2,split/2,merge/2]).

%%create a new store
create() ->
		[].

%%add 3 adds a key value pair
add(Storage,Key,Value) ->
	NewList = lists:append(Storage, [{Key,Value}]),
	lists:keysort(1, NewList).

%%lookup2 on a key
lookup(Storage,Key) ->
	lists:keyfind(Key, 1, Storage).

%%split the storage in two parts given a key
%%when a node joins
%%split2
split(Key, Storage) ->
	N = index_of(Key, Storage),
	lists:split(N, Storage).


index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _) -> 0;
index_of(Item, [{Item, _}|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

%%merge2 - merge two storages
merge(Storage1,Storage2) ->
	Merged = lists:append(Storage1, Storage2),
	lists:keysort(1, Merged).




%% ====================================================================
%% Internal functions
%% ====================================================================


