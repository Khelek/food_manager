-module(db).

-export([get_products/1, update_products_price/1]).

-define(select_user_query, "SELECT login FROM users WHERE login = $1").
-define(exists_user_query, "SELECT login, password FROM users WHERE login = $1 AND password = $2").
-define(new_user_query, "INSERT INTO users(login, password) VALUES ($1, $2)").
-define(get_list_query, "SELECT table FROM users WHERE login = $1").
-define(set_list_query, "UPDATE users SET list = $2 WHERE login = $1").
-define(get_shoplist_query, "SELECT shopping_list FROM users WHERE login = $1").
-define(set_shoplist_query, "UPDATE users SET shopping_list = $2 WHERE login = $1").



db_request(Fun) ->
    {ok, Conn} = pgsql:connect("localhost", "postgres", "password", [{database, "mydb"}, {port, 55434}]),
    Res = Fun(Conn),
    pgsql:close(Conn),
    Res.

get_products(ExcludeItems) ->
    {ok, Columns, Rows} = db_request(fun(Conn) -> pgsql:squery(Conn, get_products_query(ExcludeItems)) end), %% TODO: add exclude products to query
    Products = from Rows to Product = Row. %% convert row to product tuple

update_products_price(Values) -> %%%`
    {ok, Count} = db_request(fun(Conn) -> pgsql:equery(Conn, "update price from products values ($1, $2, $3)", [Values]). %% generic request

new_user(Login, Password) ->
    db_request(fun(Conn) -> 
                       case pgsql:equery(Conn, ?select_user_query, [Login]) of
                           {ok, Columns, Rows} when length(Rows) > 0 -> 
                               {error, <<"login is busy">>};
                           _ -> 
                               case pgsql:equery(Conn, ?new_user_query, [Login, Password]) of
                                   {ok, _, _} -> {ok, <<"ok">>};
                                   {error, _} -> {error, <<"db error">>};
                                   _ -> {error, <<"invalid">>}
                               end
                       end
               end).
                           

user_is_auth(Login, Password) ->
    db_request(fun(Conn) -> 
                       case pgsql:equery(Conn, ?exists_user_query, [Login, Password]) of
                           {ok, Columns, Rows} when length(Rows) > 0 -> {ok, <<"ok">>};
                           _ -> {error, <<"db error">>}
                       end
               end).

get_user_list(Login) ->
    get_user_attr(Login, ?get_list_query).
 
set_user_list(Login, Table) ->
    set_user_attr(Login, ?set_list_query, Table). 

get_user_shoplist(Login) ->
    get_user_attr(Login, ?get_shoplist_query).
 
set_user_shoplist(Login, Shoplist) ->
    set_user_attr(Login, ?set_shoplist_query, Shoplist). 

get_user_attr(Login, Query) ->
    db_request(fun(Conn) -> 
                       case pgsql:equery(Conn, Query, [Login]) of
                           {ok, Columns, Rows = [Attr|_Tail]} when length(Rows) > 0 -> {ok, Attr};
                           _ -> {error, <<"db error">>}
                       end
               end).

set_user_attr(Login, Query, Attr) ->    
    db_request(fun(Conn) -> 
                       case pgsql:equery(Conn, Query, [Login, Attr]) of
                           {ok, Count} -> {ok, <<"update complite">>};
                           _ -> {error, <<"db error">>}
                       end
               end).
%% queries

-spec get_products_query({ [string()], [string()] }) -> string().
get_products_query(ExcludeItems = {ExcludeType, ExcludeProducts}) ->
    "SELECT name, fats, proteins, carbohydrates, ca, pho, na, ka, hl, mg, 
       fe, zi, se, ft, jo, a, e, d, k, c, b1, b2, b5, b6, bc, b12, pp, 
       h, calories, types, price FROM products WHERE NOT types && " ++ pg_array(ExcludeType) 
        ++ " AND NOT ARRAY[name] && " ++ pg_array(ExludeProducts) ++ ";".

%% utilite

pg_array(ListString) ->
    "ARRAY['" ++ string:join(ListString, "','") ++ "']".

binlist_to_list(List) ->
    lists:map(fun(A) -> binary:bin_to_list(A) end, List).
