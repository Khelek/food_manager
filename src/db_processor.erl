-module(db_processor).

-export([get_products/1, update_products_price/1]).

-define(GET_PRODUCTS_QUERY, "SELECT name, fats, proteins, carbohydrates, ca, pho, na, ka, hl, mg, 
       fe, zi, se, ft, jo, a, e, d, k, c, b1, b2, b5, b6, bc, b12, pp, 
       h, calories, types, price
  FROM products").


db_request(Fun) ->
    {ok, Conn} = pgsql:connect("localhost", "postgres", "password", [{database, "mydb"}, {port, 55434}]),
    Res = Fun(Conn),
    pgsql:close(Conn),
    Res.

get_products(ExcludeItems = {ExcludeType, ExcludeProducts}) ->
    {ok, Columns, Rows} = db_request(fun(Conn) -> pgsql:squery(Conn, ?GET_PRODUCTS_QUERY) end), %% TODO: add exclude products to query
    Products = from Rows to Product = Row. %% convert row to product tuple

update_products_price(Values) ->
    {ok, Count} = db_request(fun(Conn) -> pgsql:squery(Conn, "update price from products values ($1, $2, $3)", [Values]). %% generic request
