-module(calculator).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file describes the functions that do the actual calculation of the job chunk data and return the
% data back to the client coordinators.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%% Messages used in this module %%%%%%%
%% -> calculation completion, sent to client coordinator
%%     {calc_done, ServerCoordinatorPid, OutChunk}

% this takes some input data (InData) and applied CalcFunction to it using a map operation, resulting in OutData.
% it then sends this OutData back to the client coordinator
calculate(ServerCoordinatorPid, {ClientCoordinatorPid, Ref, Seq, {_PreCalcFunctionMFA, CalcFunction, _PostCalcFunctionMFA}, InData}) ->
	OutData = lists:map(CalcFunction, InData),
	gen_fsm:send_all_state_event(ClientCoordinatorPid, {calc_done, ServerCoordinatorPid, {Ref, Seq, OutData}}).

%%%%%%%%%%%%%%%%%%%%% Example calculations used by demonstration functions %%%%%%%%%%%%%

% trivial example calculation
calculation1(Data) ->
	catch Data + 1.

% trivial calculation with intentional failure of a certain data element to test failure handling
calculation1_with_exit(Data) ->
	case Data of
		5 -> exit(kill);
		_ -> nothing
	end,
	catch Data + 1.

% trivial calculation with a delay.  This makes the calculation run slowly so you can see what's going on
calculation1_with_sleep(Data) ->
	timer:sleep(200),
	catch Data + 1.
