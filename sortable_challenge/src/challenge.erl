-module(challenge).
-include("challenge.hrl").
-export([stand_alone/0, distributed_map/0, distmap_camera_find_calculation/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file contains the functions that complete the Sortable challenge.
% The stand_alone function uses single-threaded serial functions to process the data.
% The distributed_map function uses the distmap application I wrote to process the data.  Distmap is a
% parallel and fault-tolerant job processing system for running on a cluster of Erlang nodes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This function writes out the data to a debug file.  This can be processed with "matching.commands.sh" script in the "doc" directory to see what has matched and what hasn't, to spot incorrect and failed matches
write_debug(Filename,ProductsAndListings) ->
	% open up the file
 	{ok, FdDebugOut} = file:open(Filename, [write]),
	% a helper function to write the data
	DebugWriteHelper = fun
		(no_match, Listing) ->
			 file:write(FdDebugOut, "{no_match} " ++ mochijson2:encode({struct,Listing#listing.original_json_tuples}) ++ "\n");
		(Product, Listing) ->
			 file:write(FdDebugOut, "{" ++ filters:manufacturer_tokens_to_text(Product#product.manufacturer) ++ "," ++ filters:family_tokens_to_text(Product#product.family) ++ "," ++ filters:model_tokens_to_text(Product#product.model) ++ "} " ++ mochijson2:encode({struct,Listing#listing.original_json_tuples}) ++ "\n")
	end,
	% go through the ProductsAndListings list and write out the entries to a file
	[ DebugWriteHelper(Product,Listing) || {Product,Listing} <- ProductsAndListings ],
	% close the file
	file:close(FdDebugOut).


% This function write out correct matches to a properly formatted JSON file.  This is the file Sortable wants to see as a result to their coding challenge
write_matches(Filename,ProductsAndListings) ->
	% open a file in which to write the output data
 	{ok, FdJsonOut} = file:open(Filename, [write]),
	% to put this in the format Sortable wants we need to write a JSON file that has one product from the "products.txt" file per line, with an array of the found listings from "listing.txt" in it.
	% to do that, we need to reformat the information in the ProductsAndListings list.  This function goes through and appends matching listings onto product names in an ordered dictionary
	OutputDict = lists:foldl(
		fun
			({no_match,_Listing},DictAcc) -> DictAcc;
			({Product,Listing},DictAcc) -> orddict:append(erlang:list_to_bitstring(Product#product.name), Listing#listing.original_json_tuples, DictAcc)
		end
	, orddict:new(), ProductsAndListings),
	% this function writes out the matching dictionary entries to a file
	orddict:map(
		fun(ProductName, ListingArray) ->
			% Encode the product name and listing array using the json encoder from the MochiWeb project
			% https://github.com/mochi/mochiweb
			file:write(FdJsonOut, mochijson2:encode({struct,[{<<"product_name">>,ProductName},{<<"listings">>,ListingArray}]}) ++ "\n")
		end
	, OutputDict),
	% close the output file
	file:close(FdJsonOut).


% This is the single-threaded stand-alone challenge function
stand_alone() ->
	% open up the file of product listings to search through
	% Note that the etstable:generate_product_ets() function loads the "products.txt" file that contains the cameras we're trying to find in the listing
	{ok, FdListings} = file:open("data/listings.txt", [read]),
	io:format("parsing listings~n"),
	% convert the JSON data to an Erlang list, and apply various filters and sanitizers to the data
	Listings = json:json2listings(FdListings, []),
	io:format("finding cameras~n"),

	% record the current time for benchmarking purposes
	TimeBefore = now(),
	% generate the product ETS table from the "products.txt" file
	etstable:generate_product_ets(),
	% process the listings file and convert it into a list of a product and listing pair
	ProductsAndListings = [ {camerafinders:find_camera(Listing),Listing} || Listing <- Listings ],
	% record the current time after the calculations have completed
	TimeAfter = now(),
	% display the elapsed time
	ElapsedTime = 1000000*(element(1,TimeAfter)-element(1,TimeBefore)) + (element(2,TimeAfter)-element(2,TimeBefore)),
	io:format("elapsed time = ~s~n",[erlang:integer_to_list(ElapsedTime)]),
	% delete the product ETS table
	ets:delete(?PRODUCT_TABLE),

	% write the data out to a JSON file
	io:format("writing out to file~n"),
%	write_debug("debug.stand.alone.txt",ProductsAndListings),
	write_matches("data/results.stand_alone.txt",ProductsAndListings),

	io:format("==== IMPORTANT ====~n"),
	io:format("Job finished.  Type \"q().\" to exit Erlang interpreter~n"),
	io:format("===================~n").


% This is the distributed-map challenge function
distributed_map() ->
	% choose a name for the computation cluster
	ClusterName = testcluster,

	% open up the file of product listings to search through
	% Note that the etstable:generate_product_ets() function loads the "products.txt" file that contains the cameras we're trying to find in the listing
	{ok, FdListings} = file:open("data/listings.txt", [read]),
	io:format("parsing listings~n"),
	% convert the JSON data to an Erlang list, and apply various filters and sanitizers to the data
	Listings = json:json2listings(FdListings, []),
	
	% for debugging purposes, display some information about the Erlang cluster
	io:format("INFO: Display nodes in Erlang cluster~n"),
	distmap:nodes(list),
	io:format("~nINFO: Displaying defined distributed map clusters~n"),
	distmap:clusters(list),
	
	% adjust the chunk sizes for the clients and servers to be suitable for the Sortable challenge data
	lists:map(fun(Pid) -> client_coordinator:set_chunk_size(Pid, 1000) end, utils:get_clients(ClusterName)),
	lists:map(fun(Pid) -> server_coordinator:set_numchunks(Pid, 1) end, utils:get_servers(ClusterName)),

	% prompt the user to select a client coordinator from a list
	io:format("~nUSER INPUT: ====>>> Select a client coordinator to which to submit a job <<<====~n"),
	ClientPid = distmap:clients(ClusterName,choose),
	
	% start up a process that displays events related to the cluster
	io:format("~nACTION: Starting process to watch events of client coordinator~n"),
	WatcherId = distmap:watch_client_coordinator(ClientPid),
	
	% display some more debugging information
	io:format("~nINFO: Displaying server coordinators connected to client coordinator~n"),
	distmap:get_connected_servers(ClientPid,list),

	% submit a job to the distributed map cluster
	io:format("~nACTION: Submitting job~n"),
	Functions = {
		% this is a pre-function that gets executed on each server coordinator before it performs the actual map calculations
		% it fills up an ETS table on each server coordinator from information in the "products.txt" file.
		% this prevents having to send the product information around with each piece of data to be calculated
		{etstable,generate_product_ets,[]},
		% this is the "map" function that gets executed for each piece of input data.
		% NOTE: it's the function defined at the end of this "challenge.erl" file
		fun challenge:distmap_camera_find_calculation/1,
		% this is a post-function that gets executed on each server coordinator after performing the map calculations
		% it deletes the product ets table
		{etstable,delete_product_ets,[]}
	},
	% record the current time for benchmarking purposes
	TimeBefore = now(),
	% submit the job to the distributed map client coordinator
	Ref = distmap:submit_job(ClientPid, Functions, Listings),

	% wait for the job to be completed
	io:format("~nACTION: Waiting for job completion...~n"),
	distmap:wait_for_completion(Ref),
	% record the finishing time and display the elapsed time
	TimeAfter = now(),
	ElapsedTime = 1000000*(element(1,TimeAfter)-element(1,TimeBefore)) + (element(2,TimeAfter)-element(2,TimeBefore)),
	io:format("elapsed time = ~s~n",[erlang:integer_to_list(ElapsedTime)]),

	% get the completed output data from the client coordinator
	io:format("~nACTION: Job finished, receiving output data.~n"),
	{ok, ProductsAndListings} = distmap:get_job(ClientPid, Ref),

	% stop the event-watching process
	io:format("~nACTION: Stopping process that watches events of client coordinator~n"),
	distmap:stop_watching_client_coordinator(WatcherId),

	% write the data out to a JSON file
	io:format("writing out to file~n"),
%	write_debug("debug.distributed.map.txt",ProductsAndListings),
	write_matches("data/results.distributed_map.txt",ProductsAndListings),

	io:format("==== IMPORTANT ====~n"),
	io:format("Job finished.  Type \"q().\" to exit Erlang interpreter~n"),
	io:format("===================~n").


% This is the calculation that gets executed by the calculation processes of the server coordinator.  It's a simple wrapper function that returns
% data in the correct format
distmap_camera_find_calculation(Listing) ->
	% call the camerafinders:find_camera function on each line of input data
	{camerafinders:find_camera(Listing),Listing}.


