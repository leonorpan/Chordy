%% @author Nora
%% @doc @todo Add description to test2.


-module(test2).

%% ====================================================================
%% API functions
%% ====================================================================
-export([testing/0]).

testing() ->
	Node = node2:start(key:generate()),
	Node1 = node2:start(key:generate(),Node),
	Node2 = node2:start(key:generate(),Node1),
	Node3 = node2:start(key:generate(),Node1),
	Node4 = node2:start(key:generate(),Node),
	Node5 = node2:start(key:generate(),Node4),
	timer:sleep(10000),
	Node ! status,
	Node1 ! status,
	Node2 ! status,
	Node3 ! status,
	Node4 ! status,
	Node5 ! status,
	timer:sleep(5000).
	%%Node5 ! probe.



%% ====================================================================
%% Internal functions
%% ====================================================================


