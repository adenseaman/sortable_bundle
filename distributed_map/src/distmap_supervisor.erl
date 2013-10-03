-module(distmap_supervisor).
-behaviour(supervisor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is a supervisor that monitors the client and server coordinators on a node.
% It can dynamically create or destroy client or server coordinators, and will respawn them if they die
% unexpectedly.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% public exports
-export([start_link/0, stop/0, init/1, set_server_number/2, set_client_number/2, test/0]).

% start the supervisor
start_link() ->
	supervisor:start_link({local, distmap_supervisor}, ?MODULE, []).

% initial supervisor specification.  Create an event handler process at least.
init(_) ->
	EventChildSpec = {event_handler, {gen_event, start_link, [{local, event_handler}]}, permanent, 1000, worker, [gen_event]},
	{ok, {{one_for_one, 3, 60},[EventChildSpec]}}.

% create a childspec, starting a child named Module, and passing in ClusterName as an argument to the child
% specifying ClusterName allows one to have completely independent processing clusters working on the same Erlang distributed computer
create_childspec(ClusterName,Module) ->
	{{ClusterName,make_ref()},
	 {Module, start_link, [ClusterName]},
	 permanent, 1000, worker, [Module]}.

% get the children of type Module belonging to ClusterName that the supervisor is taking care of
get_children(ClusterName, Module) ->
	% anonymous helper function that returns true when a worker matches both the Module and ClusterName
	FilterHelper = fun
		({{Name,_}, _, worker, [Mod]}) when Name =:= ClusterName, Mod =:= Module -> true;
	    (_) -> false
	end,
	% make a list of the children the supervisor is taking care of that match the ClusterName and Module
	lists:filter(FilterHelper, supervisor:which_children(distmap_supervisor)).

% set the number of server coordinators belonging to ClusterName the supervisor is taking care of
set_server_number(ClusterName, Number) ->
	% this uses the set_number function with Module = server_coordinator
	set_number(ClusterName, server_coordinator, Number).

% set the number of client coordinators belonging to ClusterName the supervisor is taking care of
set_client_number(ClusterName, Number) ->
	% this uses the set_number function with Module = client_coordinator
	set_number(ClusterName, client_coordinator, Number).

% set the number of Module children belonging to ClusterName the supervisor is taking care of
set_number(ClusterName, Module, Number) ->
	Children = get_children(ClusterName, Module),
	% determine whether to start new children, stop existing children, or do nothing in order to have the desired number of children
	case length(Children) of
		% start new children
		N when N < Number ->
			[supervisor:start_child(distmap_supervisor,create_childspec(ClusterName,Module)) || _X <- lists:seq(1,Number-N)];
		% terminate and delete from care existing children
		N when N > Number ->
			[{supervisor:terminate_child(distmap_supervisor, ChildId), supervisor:delete_child(distmap_supervisor, ChildId)} || {ChildId, _, _, _} <- lists:sublist(Children,N-Number)];
		% do nothing
		_ -> ok
	end.

% stop the server coordinator
stop() ->
	exit(erlang:whereis(distmap_supervisor),shutdown).

%%%%%%%%%%%%%%%%%%%%%%%% Test functions %%%%%%%%%%%%%%%%%%%%%%%%

test() ->
	process_flag(trap_exit,true),
	ClusterName = testcluster,
	{ok, _Pid} = distmap_supervisor:start_link(),
	io:format("starting 10 server coordinators~n"),
	distmap_supervisor:set_server_number(ClusterName,10),
	erlang:display(supervisor:which_children(distmap_supervisor)),
	erlang:display(global:registered_names()),
	io:format("reducing to 3 server coordinators~n"),
	distmap_supervisor:set_server_number(ClusterName,3),
	timer:sleep(500),
	erlang:display(supervisor:which_children(distmap_supervisor)),
	erlang:display(global:registered_names()),
	io:format("reducing to 1 server coordinator~n"),
	distmap_supervisor:set_server_number(ClusterName,1),
	timer:sleep(500),
	erlang:display(supervisor:which_children(distmap_supervisor)),
	erlang:display(global:registered_names()),
	io:format("starting 10 client coordinators~n"),
	distmap_supervisor:set_client_number(ClusterName,10),
	timer:sleep(500),
	erlang:display(supervisor:which_children(distmap_supervisor)),
	erlang:display(global:registered_names()),
	io:format("reducing to 3 client coordinators~n"),
	distmap_supervisor:set_client_number(ClusterName,3),
	timer:sleep(500),
	erlang:display(supervisor:which_children(distmap_supervisor)),
	erlang:display(global:registered_names()),
	io:format("reducing to 1 client coordinator~n"),
	distmap_supervisor:set_client_number(ClusterName,1),
	timer:sleep(500),
	erlang:display(supervisor:which_children(distmap_supervisor)),
	erlang:display(global:registered_names()),
	io:format("stopping supervisor and coordinators~n"),
	distmap_supervisor:stop(),
	timer:sleep(500),
	erlang:display(global:registered_names()).
