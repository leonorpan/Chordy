%% @author Nora
%% @doc @todo Add description to node1.


-module(node2).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1,start/2]).

-define(Stabilize,1000).
-define(Timeout,1000000).

start(Id) ->
	io:format("First node started!~n" ),
	start(Id,nil).

start(Id,Peer) ->
	timer:start(),
	io:format("Second node started!~n" ),
	spawn(fun() ->init(Id,Peer) end).

init(Id,Peer) -> 
	Predecessor = nil,
	{ok,Successor} = connect(Id,Peer),
	io:format("successor of ~w is: ~w~n", [Id,Successor]),
	schedule_stabilize(),
	Storage = storage:create(),
	node(Id,Predecessor,Successor,Storage).


connect(Id, nil) ->
	io:format("myself is a successot!~n"),
	{ok, {Id,self()}};
connect(Id, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			{ok,{Skey,Peer}}
	after 
		?Timeout ->
		io:format("Time out: no response~n")
	end.

node(Id, Predecessor, Successor,Store) ->
	receive
		%% a peer need to know our key
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			%%io:format("New node  ~w asks our key: ~w~n", [Peer,Id]),
			node(Id, Predecessor, Successor,Store);
		%% a new node informs us about its existence
		%% we have a new predecessor
		%%new is the new node key and pid
		{notify, New} ->
			Pred = notify(New, Id, Predecessor,Store),
			%%io:format("Predecessor of ~w is: ~w~n", [Id,Pred]),
			node(Id, Pred, Successor,Store);
		%% a predecessor needs to know our predecessor
		{request, Peer} ->
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor,Store);
		%%our successor informs us about its predecessor
		{status, Pred} ->
			Succ = stabilize(Pred, Id, Successor),
			%%io:format("Successor of ~w is: ~w~n", [Id,Succ]),
			node(Id, Predecessor, Succ,Store);
		{add, Key, Value, Qref, Client} ->
			Added = add(Key, Value, Qref, Client,
						Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Added);
		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Store);
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor,Store);
		status ->
			io:format("Status: Predecessor ~w , Successor : ~w , of node ~w~n", [Predecessor,Successor,Id]),
			node(Id, Predecessor, Successor,Store);
		probe ->
			create_probe(Id, Successor),
			node(Id, Predecessor, Successor,Store);
		{probe, Id1, Nodes, T} when (Id == Id1)->
			io:format("Probe now has to be removed! because id is ~w~n", [Id]),
			remove_probe(Id,T, Nodes),
			node(Id, Predecessor, Successor,Store);
		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			node(Id, Predecessor, Successor,Store);
		%%message used to delegate responsibility
		{handover, Elements} ->
			Merged = storage:merge(Store, Elements),
			node(Id, Predecessor, Successor, Merged);
		%%handling weird messages..
		Error ->
			io:format("Strange message received to node! ~w~n", [Error]),
			node(Id,Predecessor,Successor,Store)
	end.
%% self - nil for first node
request(Peer, Predecessor) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil};
		{Pkey, Ppid} ->
			Peer ! {status, {Pkey, Ppid}}
	end.

%%called when a node is created
%%add a new successor
schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).


%%send a request message to successor
stabilize({_,Spid}) ->
	Spid ! {request,self()}.
%%pred= our successor's predecessor
%%id = our id
%%successor = our successor
%%we have to determine if the pred will be our new successor
stabilize(Pred, Id, Successor) ->
	
	{Skey, Spid} = Successor,
	case Pred of
		%% my successor does not have any predecessor!
		%% i should notify her about my existence!
		nil ->
			Spid ! {notify, {Id,self()}},
			Successor;
		%% i am the successor's predecessor?
		{Id, _} ->
			%%everything ok
			Successor;
		%% new node!
		{Skey, _} ->
			Spid ! {notify, {Id,self()}},
			Successor;
		{Xkey, Xpid} ->
			case key:between(Xkey, Id, Skey) of
				true ->
					%% possible mistake???
					stabilize(Pred,Id,{Xkey,Xpid});
				false ->
					Spid ! {notify, {Id,self()}},
					Successor
			end
	end.

%%new node is {Nkey,Npid} that notified us about its existence
notify({Nkey, Npid}, Id, Predecessor, Store) ->
	case Predecessor of
		nil ->
			Keep = handover(Store, Nkey, Npid),
			Npid ! {status,{Nkey,Npid}},
			{Nkey,Npid};
		{Pkey, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					Keep = handover(Store, Nkey, Npid),
					Npid ! {status,{Nkey,Npid}},
					{Nkey,Npid};
				false ->
					Npid ! {status,Predecessor},
					Predecessor
			end
	end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->
			io:format("Element adding...")
			Client ! {Qref, ok},
			storage:add(Store, Key, Value);
		false ->
			%%qref wtf?
			Spid ! {add,Key,Value,Qref,Client},
			Store
	end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->
			Result = storage:lookup(Key, Store),
			Client ! {Qref, Result};
		false ->
			{_, Spid} = Successor,
			Spid ! {lookup,Key,Qref,Client}
	end.

handover(Store, Nkey, Npid) ->
	{Keep, Leave} = storage:split(Nkey, Store),
	Npid ! {handover, Leave},
	Keep.

%%possible mistake on probe
create_probe(Id, Successor) -> 
	Time = erlang:now(),
	io:format("Creating the probe from node ~w~n",[Id]),
	{_,SPid} = Successor,
	SPid ! {probe, Id, [], Time}.

remove_probe(Id,T, Nodes) -> 
	io:format("Probe received in process ~w of id ~w~n",[self(),Id]),
	T2 = erlang:now(),
	Nodes2 = lists:append(Nodes, [Id]),
	NewTime = timer:now_diff(T2, T).


forward_probe(Ref, T, Nodes, Id, Successor) -> 
	{_,Spid} = Successor,
	io:format("Forward node from  ~w, process ~w ~n",[Ref,self()]),
	Nodes2 = lists:append(Nodes, [Ref]),
	Spid ! {probe,Id,Nodes2,T}.

%% ====================================================================
%% Internal functions
%% ====================================================================


