-module(json).
-include("challenge.hrl").
-export([json2products/2, json2listings/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file handles reading and writing of the JSON product and listing files, and converts the given JSON
% items into tuples used by the Erlang program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% load a single line of JSON from the input file descriptor Fd, and add the described product tuple to the accumulator
% recursively call self until Fd returns end-of-file
% this generates a list of products that we're trying to find from within a large listing a products
json2products(Fd, Acc) ->
	% try to read a line of JSON from the file
	case file:read_line(Fd) of
		% if successfuly, then parse it
		{ok, Line} ->
			% decode the JSON into a list of binary tuples using the JSON decoder from the MochiWeb project
			% https://github.com/mochi/mochiweb
			{struct, BinaryTuples} = mochijson2:decode(Line),
			% convert the binary tuples into tuples of {atom, string}
			Tuples = [{erlang:binary_to_atom(A,utf8), erlang:binary_to_list(B)} || {A,B} <- BinaryTuples],
			% convert these tuples into a product record
			Record = tuples2product(Tuples),
			% filter and sanitize the manufacturer
			NewManufacturer = filters:sanitize_manufacturer(Record#product.manufacturer),
			% filter and sanitize the family
			NewFamily = filters:sanitize_family(Record#product.family),
			% filter and sanitize the model
			NewModel1 = filters:sanitize_model(Record#product.model),
			% manually reprioritize certain models to tweak the matching behaviour
			NewModel2 = filters:manual_model_priority_adjustment(NewManufacturer, NewFamily, NewModel1),
			% recursively call self, and add a new product record with the sanitized fields to the accumulator
			json2products(Fd, [Record#product{
				manufacturer = NewManufacturer,
				family = NewFamily,
				model = NewModel2
			}] ++ Acc);
		% if end of file has been reached, then return the accumulated list of products
		eof -> lists:reverse(Acc)
	end.

% this is pretty much the same as json2product, but operates on the large listing we're searching through
json2listings(Fd, Acc) ->
	case file:read_line(Fd) of
		{ok, Line} ->
			{struct, BinaryTuples} = mochijson2:decode(Line),
			Tuples = [{erlang:binary_to_atom(A,utf8), erlang:binary_to_list(B)} || {A,B} <- BinaryTuples],
			Record = tuples2listing(Tuples),
			NewManufacturer = filters:sanitize_manufacturer(Record#listing.manufacturer),
			% the Title field contains an unstructured description of a product.  It may or may not be a product we're searching for
			NewTitle = filters:sanitize_title(Record#listing.title),
			json2listings(Fd, [Record#listing{
				original_json_tuples = BinaryTuples,
				manufacturer = NewManufacturer,
				title = NewTitle
		    }] ++ Acc);
		eof -> lists:reverse(Acc)
	end.

% this converts a list of tuples of the form {atom,string} into a product record
% it makes use of the helper functions below to convert atoms to record identifiers
tuples2product(Tuples) ->
	lists:foldl(fun tuples2product_helper/2, #product{}, Tuples).	

% helper functions that convert tuple atoms to the appropriate product record identifier, and sets them to the given string
tuples2product_helper({product_name, ProductName}, RecordAcc) ->
	RecordAcc#product{name=ProductName};
tuples2product_helper({manufacturer, Manufacturer}, RecordAcc) ->
	RecordAcc#product{manufacturer=Manufacturer};
tuples2product_helper({model, Model}, RecordAcc) ->
	RecordAcc#product{model=Model};
tuples2product_helper({family, Family}, RecordAcc) ->
	RecordAcc#product{family=Family};
tuples2product_helper({'announced-date', Date}, RecordAcc) ->
	RecordAcc#product{date=Date}.

% this is the same as the tuples2product function, but operates on the listings
tuples2listing(Tuples) ->
	lists:foldl(fun tuples2listing_helper/2, #listing{}, Tuples).	

% helper functions that operate on the listings
tuples2listing_helper({title, Title}, RecordAcc) ->
	RecordAcc#listing{title=Title};
tuples2listing_helper({manufacturer, Manufacturer}, RecordAcc) ->
	RecordAcc#listing{manufacturer=Manufacturer};
tuples2listing_helper({price, Price}, RecordAcc) ->
	RecordAcc#listing{price=Price};
tuples2listing_helper({currency, Currency}, RecordAcc) ->
	RecordAcc#listing{currency=Currency}.
