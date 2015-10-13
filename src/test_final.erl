%% @author Nora
%% @doc @todo Add description to test_final.


-module(test_final).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/0]).

test() -> 
	Id = key:generate(),
	Id1 = key:generate(),
	Id2 = key:generate(),
	Node = nodeX:start(Id),
	%Node2 = node2:start(Id1, Node),
	%Node3 = node2:start(Id2, Node),
	timer:sleep(10000),
	%%{add, Key,Value, Qref, Client}	
	List = addthousandelements(1, Node,[]),
	io:format("list is: ~w~n", [List]),
	Time =erlang:now(),
	lookelements(Time, Node,List).
	

%waitMessages() ->
%	receive
%		{Qref,{Key,Value}} ->
%			io:format("Key ~w found! It has the value of ~w~n", [Key,Value]),
%			waitMessages()
%		end.

lookelements(Ti,Node,[]) ->
		T1 = erlang:now(),
		timer:now_diff(T1, Ti);
lookelements(Ti,Node,[H|T]) ->
		{Key,Qref} = H,
		X = Node ! {lookup,Key,Qref,self()},
		io:format("Search result is: ~w~n", [X]),
		lookelements(Ti,Node,T).

addthousandelements(10,Node,KeyList) ->KeyList;
addthousandelements(N,Node,KeyList) ->
		Key = key:generate(),
		Qref = make_ref(),
		KeyList2 = lists:append(KeyList, [{Key,Qref}]),
		Value = string:concat("Movie", "xxx"),
		Added = Node ! {add,Key, Value,Qref,self()},
		io:format("Added element:~w~n", [Added]),
		addthousandelements(N+1,Node,KeyList2).
	

%% ====================================================================
%% Internal functions
%% ====================================================================


