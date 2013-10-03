-module(distmap).
-behaviour(application).
-include("clusterdefs.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is the main distributed map (distmap) application file.  It is started on each node in the computation cluster
% and has public functions that start and stop client and server processes, and interface with the client coordinator
% to submit, check, delete, and retrieve jobs.
% It is necessary to have an overall application behaviour, instead of starting client and server processes
% by themselves, because it is not possible to start supervisor processes remotely using RPC calls, as they link
% to the calling process, and that link is severed when the RPC call ends.  However bundling things into an
% application works as expected.
% -------------------------------
% This applications works by having one or more client coordinators, and one or more server coordinators running on
% a distributed Erlang computer.  When the clients and servers have the same cluster name, they are able to auto-detect
% each other, and become aware of each other.
% The user interacts with a client coordinator to submit jobs to it.  The data in these jobs are split into small pieces
% and distributed to the server coordinators.
% The clients fairly distribute job data if multiple jobs have been submitted, and the servers fairly process data
% from multiple connected clients, ensuring even load balancing among all jobs.
% The rate of data processing is controlled by the servers, and the clients send data to the servers at the rate
% the servers request.
% If a server dies, the clients whose jobs were on that server will resend the incompletely processed data to other
% servers.
% If a client dies, then all the jobs on that client are lost and need to be submitted.
% Data is processed asynchronously without constraints on how quickly or where it is processed.  Since every server
% that is processing data is monitored by the clients, data should not be able to become lost.
% If data processing is in progress and a server enters the cluster, it will automatically detect the clients and start
% asking them for data.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%% Exports %%%%%%%%%%%%%%%%%%%%%%%%
% Application behaviour
-export([start/2, stop/1]).
% public functions
-export([nodes/1, clusters/1, clients/2, servers/2]).
-export([start_distmap/1, stop_distmap/1]).
-export([set_server_number/3, set_client_number/3, set_worker_number/2]).
-export([submit_job/3, check_job/2, remove_job/2, get_job/2]).
-export([check_all_jobs/2, get_connected_servers/2]).
-export([wait_for_completion/1, wait_for_completion/2]).
-export([watch_client_coordinator/1, stop_watching_client_coordinator/1]).
-export([setupcluster/1]).
-export([slow_example/0, fast_example/0]).

%%%%%%%%%%%%%%%%%%% Application functions %%%%%%%%%%%%%%%%%%%

start(normal, _Args) ->
	distmap_supervisor:start_link().

stop(_State) -> ok.

%%%%%%%%%%%%%%%%%%% Public functions %%%%%%%%%%%%%%%%%%%

% These functions use the "rescan" and "getnum" functions, which are a somewhat clunky way of making interactive
% functions.  Instead of rewriting all of the key press checking logic, these functions use ContinuationTuples
% which call a new function when they've finished.  For example the clusters/0 function has {clusters,[]} as the
% ContinuationTuple, and "rescan" and "getnum" both call apply(M,F,A) where {M,F,A} = ContinuationTuple, so clusters/0
% will be called to continue the interactive function.
% The functions also have tab-delimited output with a heading, so multiple output values are reported in a
% nice table-like fashion.
% The user can select a row number, and that row's value will be returned as an exit value of the function.

% show nodes connected to distributed erlang computer
nodes(GetType) ->
	Header = "Number\tName~n======\t====~n",
	ListFunction = fun() ->
		[erlang:node()] ++ erlang:nodes()
	end,
	NameFunction = fun erlang:atom_to_list/1,
	ItemFunction = fun(Node) -> Node end,
	get_item(Header, ListFunction, NameFunction, ItemFunction, GetType).


% show computation clusters running on distributed erlang computer
clusters(GetType) ->
	Header = "Number\tName~n======\t====~n",
	ListFunction = fun utils:get_clusters/0,
	NameFunction = fun erlang:atom_to_list/1,
	ItemFunction = fun(Cluster) -> Cluster end,
	get_item(Header, ListFunction, NameFunction, ItemFunction, GetType).

% show client coordinators in given cluster
clients(ClusterName, GetType) ->
	Header = "Number\tPid~n======\t===~n",
	ListFunction = fun () -> utils:get_clients(ClusterName) end,
	NameFunction = fun erlang:pid_to_list/1,
	ItemFunction = fun(ClientPid) -> ClientPid end,
	get_item(Header, ListFunction, NameFunction, ItemFunction, GetType).

% show server coordinators in given cluster
servers(ClusterName, GetType) ->
	Header = "Number\tPid\t\tCurrent\tMax\tNode~n======\t===\t\t=======\t===\t====~n",
	ListFunction = fun() ->
		Temp1 = [{Pid, server_coordinator:get_worker_number(Pid)} || Pid <- utils:get_servers(ClusterName)],
		[{Pid,Cur,Max,erlang:node(Pid)} || {Pid, {Cur, Max}} <- Temp1]
	end,
	NameFunction = fun
		({Pid,Cur,Max,Node}) ->
			 erlang:pid_to_list(Pid) ++ "\t" ++ erlang:integer_to_list(Cur) ++ "\t" ++ erlang:integer_to_list(Max) ++ "\t" ++ erlang:atom_to_list(Node) 
	end,
	ItemFunction = fun({Pid, _, _, _}) -> Pid end,
	get_item(Header, ListFunction, NameFunction, ItemFunction, GetType).

% get a list of all the jobs and their status on a client coordinator
check_all_jobs(ClientCoordinatorPid, GetType) ->
	Header = "Number\tRef\t\tSubmitter\tTotal\tReceived~n======\t===\t\t=========\t=====\t========~n",
	ListFunction = fun () -> client_coordinator:check_all_jobs(ClientCoordinatorPid) end,
	NameFunction = fun
		({Ref, SubmittedPid, TotalChunks, ChunksReceived, _PostCalcFunctionMFA}) ->
			 erlang:ref_to_list(Ref) ++ "\t" ++  erlang:pid_to_list(SubmittedPid) ++ "\t" ++ erlang:integer_to_list(TotalChunks) ++ "\t" ++ erlang:integer_to_list(ChunksReceived)
	end,
	ItemFunction = fun({Ref, _, _, _}) -> Ref end,
	get_item(Header, ListFunction, NameFunction, ItemFunction, GetType).


% get a list of all the server coordinators connected to a client coordinator
get_connected_servers(ClientCoordinatorPid, GetType) ->
	Header = "Number\tServerPid\tChunksSubmitted~n======\t=========\t===============~n",
	ListFunction = fun () -> client_coordinator:get_connected_servers(ClientCoordinatorPid) end,
	NameFunction = fun
		({ServerCoordinatorPid,ChunksSubmitted}) ->
			 erlang:pid_to_list(ServerCoordinatorPid) ++ "\t" ++ erlang:integer_to_list(ChunksSubmitted)
	end,
	ItemFunction = fun({ServerCoordinatorPid, _}) -> ServerCoordinatorPid end,
	get_item(Header, ListFunction, NameFunction, ItemFunction, GetType).


%%%%%%
% These functions control the distributed map application and the number of client and server processes, and the
% number of parallel workers for each server processed
%%%%%%

% start the application distmap on a remote node
start_distmap(Node) ->
	rpc:call(Node, application, start, [distmap]).

% stop the distmap application on a remote node
stop_distmap(Node) ->
	rpc:call(Node, application, stop, [distmap]).

% set the number of server coordinators on a remote node
set_server_number(Node, ClusterName, Number) ->
	rpc:call(Node, distmap_supervisor, set_server_number, [ClusterName, Number]).

% set the number of client coordinators on a remote node
set_client_number(Node, ClusterName, Number) ->
	rpc:call(Node, distmap_supervisor, set_client_number, [ClusterName, Number]).

% set the number of parallel workers in a server coordinator on a remote node
set_worker_number(ServerCoordinatorPid, Number) ->
	server_coordinator:change_worker_number(Number, ServerCoordinatorPid).

%%%%%%
% job handling functions.  These are simply interfaces to the client_coordinator functions
%%%%%%

% submit a job to the client coordinator
submit_job(ClientCoordinatorPid, Function, InputData) ->
	client_coordinator:submit_job(ClientCoordinatorPid, Function, InputData).

% check a job on a client coordinator
check_job(ClientCoordinatorPid, Ref) ->
	client_coordinator:check_job(ClientCoordinatorPid, Ref).

% remove a job from a client coordinator
remove_job(ClientCoordinatorPid, Ref) ->
	client_coordinator:remove_job(ClientCoordinatorPid, Ref).

% get the completed data from a job on a client coordinator
get_job(ClientCoordinatorPid, Ref) ->
	client_coordinator:get_job(ClientCoordinatorPid, Ref).

% wait (block) until a given job has finished on a client coordinator
wait_for_completion(Ref) ->
	wait_for_completion(Ref, infinity).

wait_for_completion(Ref,TimeOut) ->
	receive
		{job_complete, Ref} -> ok
	after TimeOut -> timeout
	end.

% start a process that reports events from a client coordinator
watch_client_coordinator(ClientCoordinatorPid) ->
	client_coordinator:start_watcher(ClientCoordinatorPid, []).

% stop the watching process
stop_watching_client_coordinator(WatcherId) ->
	client_coordinator:stop_watcher(WatcherId).

%%%%%%%%%%%%%%%%%%% Private functions %%%%%%%%%%%%%%%%%%%

% These functions are used by the public functions to create interactive functions that display information
% about the cluster.  They are essentially factored-out processing code, and help to make the functions more generic

get_item(Header, ListFunction, NameFunction, ItemFunction, GetType) ->
	io:format(Header),
	List = ListFunction(),
	NumNames = lists:zip(lists:seq(1,length(List)), List),
	[io:format("~s\t~s~n",[integer_to_list(Number),NameFunction(Name)]) || {Number,Name} <- NumNames],
	ReturnValue = case GetType of
		choose ->
			Temp = io:get_line("Enter number to return name, Enter or \"r\" to rescan, \"e\" to exit"), % allow the user to get data, or rescan for more information
			{Prompt,_} = lists:split(length(Temp)-1,Temp),
			case Prompt of
				"r" -> get_item(Header, ListFunction, NameFunction, ItemFunction, GetType);
				"R" -> get_item(Header, ListFunction, NameFunction, ItemFunction, GetType);
				"" -> get_item(Header, ListFunction, NameFunction, ItemFunction, GetType);
				"e" -> {error, no_selection};
				"E" -> {error, no_selection};
				N -> % extract data from the printed table
					try list_to_integer(N) of
						Num when Num =< length(List) ->
							   lists:nth(Num,List);
						_ ->
							io:format("Invalid input, try again~n"),
							get_item(Header, ListFunction, NameFunction, ItemFunction, GetType)
					catch
						error:_ -> {error, number_error}
					end
			end;
		list -> {error, no_selection};
		_ -> {error, unknown_get_type}
	end,
	case ReturnValue of
		{error, Type} -> Type;
		Otherwise -> ItemFunction(Otherwise)
	end.

%%%%%%%%%%%%%%%%%%% Test functions %%%%%%%%%%%%%%%%%%%

% Set up a test cluster on which to process data
setupcluster(NodeStringList) ->
	% monitor when one of the connected nodes dies
	process_flag(trap_exit,true),
	% connect to the list of nodes
	NodeList = lists:map(fun(NodeString) -> erlang:list_to_atom(NodeString) end, NodeStringList),
	lists:map(fun(Node) -> net_kernel:connect_node(Node) end, NodeList),
	
	ClusterName = testcluster,
	
	% start the distmap application on all nodes
	lists:map(fun(Node) -> distmap:start_distmap(Node) end, NodeList),
	
	% start servers on all the nodes
	lists:map(fun(Node) -> distmap:set_server_number(Node, ClusterName, 1) end, NodeList),

	% set up the first node as a client
	distmap:set_client_number(hd(NodeList), ClusterName, 1).

% This is an example that runs a very trivial job on the distributed cluster, and gives the user an idea of how the
% system works and how it's able to recover from failures.
% I won't comment the body of the function because there are lots of output lines that explain what's going on
slow_example() ->

	ClusterName = testcluster,
	
	io:format("INFO: Display nodes in Erlang cluster~n"),
	distmap:nodes(list),
	
	io:format("~nINFO: Displaying defined distributed map clusters~n"),
	distmap:clusters(list),
	
	% for this simple test, adjust the chunksize and numchunks variables on the clients and servers, respectively
	% this makes it so the test runs on a lot of different nodes
	lists:map(fun(Pid) -> client_coordinator:set_chunk_size(Pid, 2) end, utils:get_clients(ClusterName)),
	lists:map(fun(Pid) -> server_coordinator:set_numchunks(Pid, 2) end, utils:get_servers(ClusterName)),
	
	io:format("~nUSER INPUT: ====>>> Select a server coordinator on which to adjust the worker number <<<====~n"),
	ServerPid = distmap:servers(ClusterName,choose),
	
	distmap:set_worker_number(ServerPid, 10),
	
	io:format("~nINFO: Displaying Server Coordinators~n"),
	distmap:servers(ClusterName,list),
	
	io:format("~nUSER INPUT: ====>>> Select a client coordinator to which to submit a job <<<====~n"),
	ClientPid = distmap:clients(ClusterName,choose),
	
	io:format("~nACTION: Starting process to watch events of client coordinator~n"),
	WatcherId = distmap:watch_client_coordinator(ClientPid),
	
	io:format("~nINFO: Displaying server coordinators connected to client coordinator~n"),
	distmap:get_connected_servers(ClientPid,list),

	io:format("~nACTION: Submitting job~n"),
	Ref = distmap:submit_job(ClientPid, {[], fun calculator:calculation1_with_sleep/1, []}, lists:seq(2000,3000)),

	io:format("~nINFO: Waiting...~n"),
	timer:sleep(4000),

	io:format("~nINFO: Getting status of all jobs on client~n"),
	distmap:check_all_jobs(ClientPid,list),
	
	io:format("~nINFO: Displaying server coordinators connected to client coordinator again~n"),
	distmap:get_connected_servers(ClientPid,list),

	io:format("~nINFO: Displaying Server Coordinators again~n"),
	distmap:servers(ClusterName,list),
	
	VictimNode = lists:last(erlang:nodes()),
	
	io:format("~nACTION: Killing server coordinator~n"),
	distmap:stop_distmap(VictimNode),

	timer:sleep(500),
	io:format("~nINFO: Displaying Server Coordinators again~n"),
	distmap:servers(ClusterName,list),

	timer:sleep(2000),
	io:format("~nACTION: Starting server coordinator again~n"),
	distmap:start_distmap(VictimNode),
	distmap:set_server_number(VictimNode, ClusterName, 1),

	timer:sleep(2000),
	io:format("~nINFO: Displaying Server Coordinators again~n"),
	distmap:servers(ClusterName,list),
	
	io:format("~nACTION: Waiting for job completion...~n"),
	distmap:wait_for_completion(Ref),
	
	io:format("~nACTION: Job finished, receiving output data~n"),
	{ok, OutputData} = distmap:get_job(ClientPid, Ref),

	io:format("~nACTION: Stopping process that watches events of client coordinator~n"),
	distmap:stop_watching_client_coordinator(WatcherId),
	
	io:format("==== IMPORTANT ====~n"),
	io:format("Test finished.  Type \"q().\" to exit Erlang interpreter~n"),
	io:format("===================~n"),
	OutputData.

% This is an example that runs a much larger but still very trivial job on the distributed cluster.
% It runs at full speed to give the user an idea of how fast it actually is.
% I won't comment the body of the function because there are lots of output lines that explain what's going on
fast_example() ->

	ClusterName = testcluster,
	
	io:format("INFO: Display nodes in Erlang cluster~n"),
	distmap:nodes(list),
	
	io:format("~nINFO: Displaying defined distributed map clusters~n"),
	distmap:clusters(list),
	
	% for this bigger test, adjust the chunksize and numchunks variables on the clients and servers, respectively to be the defaults
	lists:map(fun(Pid) -> client_coordinator:set_chunk_size(Pid, ?DEFAULT_CHUNK_SIZE) end, utils:get_clients(ClusterName)),
	lists:map(fun(Pid) -> server_coordinator:set_numchunks(Pid, ?DEFAULT_NUM_CHUNKS) end, utils:get_servers(ClusterName)),

	io:format("~nUSER INPUT: ====>>> Select a client coordinator to which to submit a job <<<====~n"),
	ClientPid = distmap:clients(ClusterName,choose),
	
	io:format("~nACTION: Starting process to watch events of client coordinator~n"),
	WatcherId = distmap:watch_client_coordinator(ClientPid),
	
	io:format("~nINFO: Displaying server coordinators connected to client coordinator~n"),
	distmap:get_connected_servers(ClientPid,list),

	io:format("~nACTION: Submitting job~n"),
	Ref = distmap:submit_job(ClientPid, {[], fun calculator:calculation1/1, []}, lists:seq(1,1000000)),

	timer:sleep(100),
	io:format("~nINFO: Getting status of all jobs on client~n"),
	distmap:check_all_jobs(ClientPid,list),
	
	io:format("~nINFO: Displaying server coordinators connected to client coordinator again~n"),
	distmap:get_connected_servers(ClientPid,list),

	io:format("~nACTION: Waiting for job completion...~n"),
	distmap:wait_for_completion(Ref),
	
	io:format("~nACTION: Job finished, receiving output data.~n"),
	{ok, OutputData} = distmap:get_job(ClientPid, Ref),

	io:format("~nACTION: Stopping process that watches events of client coordinator~n"),
	distmap:stop_watching_client_coordinator(WatcherId),
	
	io:format("==== IMPORTANT ====~n"),
	io:format("Test finished.  Type \"q().\" to exit Erlang interpreter~n"),
	io:format("===================~n"),
	OutputData.
