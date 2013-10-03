-module(camerafinders).
-include("challenge.hrl").
-export([find_camera/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file contains functions that search for cameras in a listing.  They use the token finding functions
% in the tokenfinders.erl file.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% break the matching cameras into 2 groups, those that have family names and those that dont
find_camera_match_splitter(Matches) ->
	find_camera_match_splitter_helper(Matches, [], []).

% a helper function that actually does the splitting
% this function matches when the Matches list is exhausted, and returns a tuple of the WithFamily and WithoutFamily cameras
find_camera_match_splitter_helper([], WithFamily, WithoutFamily) ->
	{WithFamily, WithoutFamily};
% this function matches all other cases and recursively calls itself
find_camera_match_splitter_helper([HeadMatch|TailMatches], WithFamily, WithoutFamily) ->
	% extract the product from the HeadMatch
	{_,Product} = HeadMatch,
	% get the family name of the product
	case Product#product.family of
		% if the family name is empty, then the camera gets added to the WithoutFamily list
		"" -> find_camera_match_splitter_helper(TailMatches, WithFamily, WithoutFamily ++ [HeadMatch]);
		% otherwise, add the camera to the WithFamily list
		_ -> find_camera_match_splitter_helper(TailMatches, WithFamily ++ [HeadMatch], WithoutFamily)
	end.

% filter the WithFamilyMatches list to include those whose family names appear in the TitleTokens
find_camera_with_family_finder(WithFamilyMatches, TitleTokens) ->
	lists:filter(fun(WithFamilyMatch) -> find_camera_with_family_finder_helper(WithFamilyMatch, TitleTokens) end, WithFamilyMatches).

% a helper function that does the actual searching for the family names
find_camera_with_family_finder_helper({_,Product}, TitleTokens) ->
	% an anonymous fold helper function that returns true if a FamilyName in FamilyNameList matches a given Token in TitleTokens
	FoldHelper = fun
					(FamilyNameList, Token) ->
						 lists:foldl(fun(FamilyName,Acc) -> Acc or (FamilyName =:= Token) end, false, FamilyNameList)
				 end,
	% fold over the TitleTokens and search for the product's family name in the TitleTokens.
	% Return true if the family name is found in TitleTokens
	lists:foldl(fun(Token,Acc) -> Acc or FoldHelper(Product#product.family, Token) end, false, TitleTokens).

% try to figure out what camera model is described by a Listing
find_camera(Listing) ->
	% retrieve the TitleTokens from the Listing
	TitleTokens = Listing#listing.title,
	% first find the manufacturer in the TitleTokens
	case tokenfinders:find_manufacturer(etstable:get_ets_manufacturers(), TitleTokens) of
		% if nothing is found, then there is no match
		[] -> no_match;
		% if a manufacturer is found, then...
		Manufacturer ->
			% find a model made by Manufacturer in the TitleTokens
			case tokenfinders:find_model(Manufacturer, TitleTokens) of
				% if no model is found, then there is no match
				[] -> no_match;
				% if one or more models are found, then...
				Model ->
					% retrieve a list of cameras made by a Manufacturer with a particular Model number.  Most companies only have one
					% camera with a particular model, but some have the same model number for different camera families, and we need to
					% handle that possibility.
					% the ets:lookup returns a list like the following:
					% [{{records, Manufacturer, Model}, Product}] = Matches,
					Matches = ets:lookup(?PRODUCT_TABLE, {records, Manufacturer, Model}),
					% break the matches into 2 groups, those whose products have family names, and those without
					{WithFamilyMatches, WithoutFamilyMatches} = find_camera_match_splitter(Matches),
					% go through the products that do have family names
					% search for each product's family name in the TitleTokens
					% if a name can be found, include that in the resulting list
					WithFamilyMatchesFound = find_camera_with_family_finder(WithFamilyMatches,TitleTokens),
					% determine what kind of match we have
					Match =
						if
							% if the resulting list has more than 1 element, then no_match,
							% because there can only be one family name is a listing
							(length(WithFamilyMatchesFound) > 1) -> no_match;
							% if the resulting list has only 1 element, then return that as the match as it is unique
							(length(WithFamilyMatchesFound) =:= 1) -> hd(WithFamilyMatchesFound);
						% otherwise the resulting list has 0 elements, so examine the group of products without family names
				    		true ->
					   		if
								% if that group has more than 1 element, then no_match, as the match must be unique
								(length(WithoutFamilyMatches) > 1) -> no_match;
								% if that group has 1 element, then that's the unique match
								(length(WithoutFamilyMatches) =:= 1) -> hd(WithoutFamilyMatches);
								% otherwise that group is empty, then no_match
						  		true -> no_match
					   		end
						end,
					% return the product if one is found, or no_match is nothing has been found
					case Match of
						no_match -> no_match;
						{_,Product} -> Product
					end
			end
	end.
