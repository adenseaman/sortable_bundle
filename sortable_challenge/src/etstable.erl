-module(etstable).
-include_lib("stdlib/include/ms_transform.hrl").
-include("challenge.hrl").
-export([generate_product_ets/0, delete_product_ets/0, get_ets_manufacturers/0, get_ets_models/1, get_ets_product/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file contains functions that create and retrieve data from an ETS table.
% The purpose of these tables is to allow persistent data to be stored on the calculation nodes, without
% having to shuffle lots of information with each function call.  This greatly speeds things up, and ETS
% tables can be accessed very quickly.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% generate the product ETS table
generate_product_ets() ->
	% the first step is to load in the product data
	{ok, FdProducts} = file:open("data/products.txt", [read]),
	% convert it from JSON into tuples that describe the products, sanitizing the data at the same time
	Products = json:json2products(FdProducts,[]),
	% create an named ETS table
	ets:new(?PRODUCT_TABLE, [set,{keypos, 1},named_table,public,{read_concurrency,true}]),
	% put the manufacturers into a table element
	ets:insert(?PRODUCT_TABLE, {manufacturers, get_manufacturers(Products)}),
	% put the models into table elements
	[ ets:insert(?PRODUCT_TABLE, {{models, Manufacturer}, get_models(Products, Manufacturer)}) || Manufacturer <- get_manufacturers(Products)],
	% put the records into table elements
	[ ets:insert(?PRODUCT_TABLE, {{records, Product#product.manufacturer, Product#product.model}, Product}) || Product <- Products],
	% wait a little bit for internal Erlang synchronization, otherwise things might get messed up if it tries to access the data too soon.
	timer:sleep(100).

% delete the table and release the resources
delete_product_ets() ->
	ets:delete(?PRODUCT_TABLE).

% extract the manufacturers from the Product tuples
get_manufacturers(Products) ->
	lists:usort([Product#product.manufacturer || Product <- Products]).

% extract the models from the Product tuples
get_models(Products,Manufacturer) ->
	[Product#product.model || Product <- Products, Product#product.manufacturer =:= Manufacturer].

% retrieve a list of manufacturers from the ETS table
get_ets_manufacturers() ->
	[{manufacturers, Manufacturers}] = ets:lookup(?PRODUCT_TABLE, manufacturers),
	Manufacturers.

% retrieve a list of the models for a particular manufacturer from the ETS table
get_ets_models(Manufacturer) ->
	[{{models, Manufacturer}, Models}] = ets:lookup(?PRODUCT_TABLE, {models, Manufacturer}),
	Models.

% retrieve a product from the ETS table given a Manufacturer and Model number
get_ets_product({Manufacturer, Model}) ->
	ets:select(?PRODUCT_TABLE, ets:fun2ms(fun({{records, TempManufacturer, TempModel},Product}) when {TempManufacturer,TempModel} =:= {Manufacturer,Model} -> Product end)).

