-module(client_watcher).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file describes a gen_server that is spawned to pay attention to event feeds from the client_events
% process.  It simply receives events and outputs a text description of what it has received.
% This allows people to monitor the status of their jobs and the health of the cluster
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%% Exports %%%%%%%%%%%%%%%%%%%%%%%%
% GEN_SERVER
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Public functions

%%%%%%% Events used in this module %%%%%%%

% {ClientCoordinatorPid, {server_up, ServerCoordinatorPid}} : a server coordinator has come online
% {ClientCoordinatorPid, {submit_job, SubmitterPid, Ref}} : a job has been submitted
% {ClientCoordinatorPid, {delete_job, Ref}} : a job has been deleted
% {ClientCoordinatorPid, {get_job, Ref}} : the results of a job has been retrieved
% {ClientCoordinatorPid, {server_down, ServerCoordinatorPid}} : a server has gone down
% {ClientCoordinatorPid, {job_complete, Ref}} : a job has finished
% {ClientCoordinatorPid, {progress_update, Ref, ChunksReceived, TotalChunks}} : progress update of running job

%%%%%%%%%%%%%%%%%%%%%%%% GEN_SERVER functions%%%%%%%%%%%%%%%%%%%%%%%%

% Initialize without a state
init([]) ->
	{ok, []}.

% don't handle any synchronous calls
handle_call(_Request, _From, State) ->
	{noreply, State}.

% handle event messages from client_events, and turn them into descriptive text with io:format
% this will appear on the Erlang console of the process that starter the gen_server process
% when a server comes online
handle_cast({_ClientCoordinatorPid, {server_up, ServerCoordinatorPid}}, State) ->
	io:format("EVENT: Server ~s has come online~n",[erlang:pid_to_list(ServerCoordinatorPid)]),
	{noreply, State};
% when a job has been submitted
handle_cast({_ClientCoordinatorPid, {submit_job, SubmitterPid, Ref}}, State) ->
	io:format("EVENT: Pid ~s has submitted job ~s~n",[erlang:pid_to_list(SubmitterPid),erlang:ref_to_list(Ref)]),
	{noreply, State};
% when a job has been deleted
handle_cast({_ClientCoordinatorPid, {delete_job, Ref}}, State) ->
	io:format("EVENT: Job ~s has been deleted~n",[erlang:ref_to_list(Ref)]),
	{noreply, State};
% when a job has been retrieved
handle_cast({_ClientCoordinatorPid, {get_job, Ref}}, State) ->
	io:format("EVENT: Job ~s has been retrieved by submitter~n",[erlang:ref_to_list(Ref)]),
	{noreply, State};
% when a server goes down
handle_cast({_ClientCoordinatorPid, {server_down, ServerCoordinatorPid}}, State) ->
	io:format("EVENT: Server ~s has gone offline~n",[erlang:pid_to_list(ServerCoordinatorPid)]),
	{noreply, State};
% when a job has finished
handle_cast({_ClientCoordinatorPid, {job_complete, Ref}}, State) ->
	io:format("EVENT: Job ~s has finished~n",[erlang:ref_to_list(Ref)]),
	{noreply, State};
% when a progress update is received
handle_cast({_ClientCoordinatorPid, {progress_update, Ref, ChunksReceived, TotalChunks}}, State) ->
	io:format("EVENT: Job ~s is ~s% complete~n",[erlang:ref_to_list(Ref), erlang:integer_to_list(erlang:round(100.0*ChunksReceived/TotalChunks))]),
	{noreply, State};
% for all other casts, do nothing
handle_cast(_Request, State) ->
	{noreply, State}.

% don't handle any non-gen_server messages
handle_info(_Info, State) ->
	{noreply, State}.

% don't do anything for hot code-swapping
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% terminate normally
terminate(_Reason, _State) ->
	ok.
