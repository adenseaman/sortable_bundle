%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is the header file for the Sortable Challenge solver.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% a record describing a product that we're trying to find
-record(product, {name="", manufacturer="", model=[], family="", date=""}).

% a record describing an element in a large list of products, in which we're trying to find our given products
-record(listing, {original_json_tuples=[], title="", manufacturer="", currency="", price=""}).

% name of product ETS table.  This will be used on the calculation nodes to find the ETS table resident in memory
-define(PRODUCT_TABLE,sortable_product_ets_table).
