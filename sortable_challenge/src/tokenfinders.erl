-module(tokenfinders).
-include("challenge.hrl").
-export([find_model/2, find_manufacturer/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file contains functions that find tokens in lists of tokens.  They are essentially helper functions
% used by the camera finding functions in camerafinders.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Try to determine a manufacturer from the title tokens.
% Given a list of Manufacturers, see if any of them can be found among the TitleTokens.
% These functions are slightly more complicated than would be ideal because there is one manufacturer,
% konica-minolta that is referred to be either name, so each manufacturer name must be a list
% For example: Manufacturers = [["canon"],["sony"],["konica","minolta"],["epson"]]
find_manufacturer(Manufacturers, TitleTokens) ->
	% filter out the Manufacturers list to be only those that appear in the title tokens
	case lists:filter(fun(Manufacturer) -> find_manufacturer_list_helper(Manufacturer, TitleTokens) end, Manufacturers) of
		[] -> [];
		% if one matches, return only the first one
		FoundManufacturers -> hd(FoundManufacturers)
	end.

% Go through each token in the manufacturer token list and see if any of them are present in the title tokens
% For example: ManufacturerTokens = ["konica","minolta"]
find_manufacturer_list_helper(ManufacturerTokens, TitleTokens) ->
	% fold over the tokens in ManufacturerTokens.  Return true if any of them match
	lists:foldl(fun(ManufacturerToken,Acc) -> Acc or find_manufacturer_list_helper_match(ManufacturerToken,TitleTokens) end, false, ManufacturerTokens).

% this takes meach ManufacturerToken and searches for it in the TitleTokens
% For example: ManufacturerToken = "konica" 
find_manufacturer_list_helper_match(ManufacturerToken, TitleTokens) ->
	% fold over the list of TitleTokens and see if any match the ManufacturerToken.  Return true if any of them do.
	lists:foldl(fun(Token,Acc) -> Acc or (ManufacturerToken =:= Token) end, false, TitleTokens).

% Search for a model in TitleTokens
% This requires a manufacturer to be found first, as the list of possible models will depend on the manufacturer
% Note that models can have required and optional tokens.  If a required or optional token matches, it contributed to the match quality
% if an required token does not match, the search fails.  If an optional token does not match, the search continues
find_model(Manufacturer, TitleTokens) ->
	% retrieve all the models we're looking for from this manufacturer
	Models = etstable:get_ets_models(Manufacturer),
	% search for the Model in the TitleTokens and form a list with rankings for the various Models
	MatchList = [ {token_finder(Model, TitleTokens), Model} || Model <- Models],
	% sort the items in the list to give us the highest ranking match (most characters in common)
	case hd(lists:reverse(lists:sort(MatchList))) of
		% only return the Model if the match is unique, ie. if multiple models match equally well, don't return anything
		{{MatchQuality, unique}, Model} when MatchQuality > 0 -> Model;
		_ -> []
	end.

% Search for the FindTokens among the ListingTokens.  This works by comparing the FindTokens and ListingTokens, and cyclically shifting
% the ListingTokens.  This is like a convolution operation, and the match quality is the result of the operation.  The offset where the
% match quality is highest, is where the FindTokens are mostly likely to be present in the ListingTokens
token_finder(FindTokens, ListingTokens) ->
	% cyclically shift ListingTokens through all of its tokens while comparing FindTokens to the firstmost tokens
	% this returns a list of the match quality for each cyclic shifting offset of ListingTokens
	MatchList = token_finder_shift_helper(FindTokens, ListingTokens, length(ListingTokens), []),
	% find the largest element, this is where the FindTokens are most likely to be found
	MaxElement = lists:max(MatchList),
	% make sure the largest element is unique.  If not, then the FindTokens can be found in multiple locations
	% which means it's not a good match
	case length(lists:filter(fun (X) -> X =:= MaxElement end, MatchList)) of
		% return whether it's unique or not
		1 -> {MaxElement, unique};
		_ -> {MaxElement, not_unique}
	end.

% This helper functions cyclically shifts through the ListingTokens and eventually returns a list of MatchingCounts (match quality)
% this function matches when there are no more shifts left, and returns the accumular
token_finder_shift_helper(_FindTokens, _ListingTokens, 0, Acc) -> Acc;
% this function matches in all other cases, and recursively calls itself
token_finder_shift_helper(FindTokens, ListingTokens, ShiftsLeft, Acc) ->
	[HeadToken|TailTokens] = ListingTokens,
	% call another helper function to compare the FindTokens for this particular permutation of the ListingTokens
	MatchingCount = token_finder_comparison_helper(FindTokens, ListingTokens, 0),
	% recursively call self with a new permutation of the ListingTokens, MatchingCount added onto the accumulator list, and a decreased ShiftsLeft
	token_finder_shift_helper(FindTokens, TailTokens ++ [HeadToken], ShiftsLeft-1, Acc ++ [MatchingCount]).

% This helper function does the actual comparison between the FindTokens and ListingTokens
% It handles when FindTokens are required or optional
% It does a one-by-one comparison of the FindTokens to ListingTokens
% this function matches when the FindTokens have been exhausted, and returns the MatchingCount
token_finder_comparison_helper([], _ListingTokens, MatchingCount) -> MatchingCount;
% this function matches when the ListingTokens have been exhausted, and returns the MatchingCount
token_finder_comparison_helper(_FindTokens, [], MatchingCount) -> MatchingCount;
% this functions handles all other cases and recursively calls itself
token_finder_comparison_helper([FindTokensHead|FindTokensTail], [ListingTokensHead|ListingTokensTail], MatchingCount) ->
	% for the first token in FindTokens
	case FindTokensHead of
		% if the token is required, then search for it.  If it matches, then increase MatchingCount by 1
		% if it's not found, then stop the recursion and return zero (no match)
		{required, Token} ->
			if (Token =:= ListingTokensHead) ->
				   token_finder_comparison_helper(FindTokensTail, ListingTokensTail, MatchingCount+1);
				% otherwise
				true -> 0
			end;
		% if the token is optional and is found, then increase the MatchingCount by 1
		% if it's not found, then proceed with searching for more tokens
		{optional, Token} ->
			if (Token =:= ListingTokensHead) ->
				   token_finder_comparison_helper(FindTokensTail, ListingTokensTail, MatchingCount+1);
				% otherwise
				true ->
				   token_finder_comparison_helper(FindTokensTail, ListingTokensTail, MatchingCount)
			end
	end.
