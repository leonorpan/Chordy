%% @author Nora
%% @doc @todo Add description to test.


-module(test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

start() ->
	IsBetween1 = key:between(2,9,4),
	IsBetween2 = key:between(9,7,10),
	IsBetween3 = key:between(5,2,5),
	IsBetween4 = key:between(3,8,3),
	IsBetween5 = key:between(5,9,9),
	IsBetween6 = key:between(2,7,9),
	IsBetween7 = key:between(11,2,5),
	TrueList = [IsBetween2,IsBetween3,IsBetween4,IsBetween5],
	FalseList = [IsBetween6,IsBetween7,IsBetween1],
	io:format("This has to be all true ~w~n", [TrueList]),
	io:format("This has to be all false ~w~n", [FalseList]),
	Node = node1:start(1),
	Node1 = node1:start(2,Node),
	Node2 = node1:start(3,Node1),
	%%Node3 = node1:start(4,Node2),
	%%Node4 = node1:start(5,Node3),
	ListOfNodes = [Node,Node1,Node2],
	io:format("This is our list of node processes: ~w~n", [ListOfNodes]),
	io:format("sending probe to node 4"),
	Node1 ! probe.


%% ====================================================================
%% Internal functions
%% ====================================================================


