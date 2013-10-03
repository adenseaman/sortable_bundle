-module(client_events).
-behaviour(gen_event).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file describes the event handler for the client coordinator process.
% It is an event handler that will send messages to a watching PID on the status of the client
% coordinator and of the submitted jobs.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%% Exports %%%%%%%%%%%%%%%%%%%%%%%%
% GEN_EVENT
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%% Records %%%%%%%%%%%%%%%%%%%%%%%%

% FSM State
-record(state,{
  watcherPid,
  clientCoordinatorPid,
  jobRef
}).

%%%%%%% Events used in this module %%%%%%%

% {ClientCoordinatorPid, {server_up, ServerCoordinatorPid}} : a server coordinator has come online
% {ClientCoordinatorPid, {submit_job, SubmitterPid, Ref}} : a job has been submitted
% {ClientCoordinatorPid, {delete_job, Ref}} : a job has been deleted
% {ClientCoordinatorPid, {get_job, Ref}} : the results of a job has been retrieved
% {ClientCoordinatorPid, {server_down, ServerCoordinatorPid}} : a server has gone down
% {ClientCoordinatorPid, {job_complete, Ref}} : a job has finished
% {ClientCoordinatorPid, {progress_update, Ref, ChunksReceived, TotalChunks}} : progress update of running job

%%%%%%%%%%%%%%%%%%%%%%%% GEN_EVENT functions%%%%%%%%%%%%%%%%%%%%%%%%

% Initialize the gen_event
init([WatcherPid, ClientCoordinatorPid, JobRef]) ->
	% record the Pid of the watching process that's asking to receive messages
	% and the jobRef that is has submitted
	State = #state{watcherPid = WatcherPid, clientCoordinatorPid = ClientCoordinatorPid, jobRef = JobRef},
	{ok, State}.

% if any event has come in for the client coordinator that the watching PID is watching, then notify the watching PID about it
handle_event({ClientCoordinatorPid, _} = Event, State) when State#state.clientCoordinatorPid =:= ClientCoordinatorPid ->
	gen_server:cast(State#state.watcherPid, Event),
	{ok, State};
% for all other cases, do nothing
handle_event(_, State) ->
	{ok, State}.

% do nothing if a synchronous call comes in
handle_call(_, State) ->
	{ok, ok, State}.

% do nothing if a non-gen_event message comes in
handle_info(_, State) ->
	{ok, State}.

% hot code swapping does nothing
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% terminate function
terminate(_Reason, _State) ->
	ok.
