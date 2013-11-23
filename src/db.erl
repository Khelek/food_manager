-module(db).

-export([get_products/1, update_products_price/1]).

-define(select_user_query, "SELECT login FROM users WHERE login = $1").
-define(exists_user_query, "SELECT login, password FROM users WHERE login = $1 AND password = $2").
-define(new_user_query, "INSERT INTO users(login, password) VALUES ($1, $2)").
-define(get_list_query, "SELECT table FROM users WHERE login = $1").
-define(set_list_query, "UPDATE users SET list = $2 WHERE login = $1").
-define(get_shoplist_query, "SELECT shopping_list FROM users WHERE login = $1").
-define(set_shoplist_query, "UPDATE users SET shopping_list = $2 WHERE login = $1").


-compile(export_all).


db_request(Fun) ->
    %{ok, Conn} = pgsql:connect("localhost", "foodmanager", "foodeat", [{database, "food"}]),
    {ok, Conn} = pgsql:connect("localhost", "postgres", "password", [{database, "mydb"}, {port, 55434}]),
    Res = Fun(Conn),
    pgsql:close(Conn),
    Res.


url_db(URL, openshift) ->
    [DBName | Opts] = string:tokens(URL, "/:@"),
    lists:append(Opts, ["food"]).

url_db(URL) ->
    [DBName | Opts] = string:tokens(URL, "/:@"),
    Opts.

get_products(ExcludeItems) ->
    {ok, Columns, Rows} = db_request(fun(Conn) -> pgsql:squery(Conn, get_products_query(ExcludeItems)) end),
    Products = lists:map(fun(A) -> erlang:tuple_to_list(A) end, Rows). 

get_lists(Lang) ->
    [{ok, C1, R1},{ok, C2, R2}] = db_request(fun(Conn) -> pgsql:squery(Conn, get_lists_query(Lang)) end),
    Names = lists:flatten(lists:map(fun(A) -> erlang:tuple_to_list(A) end, R1)),
    Types = lists:flatten(lists:map(fun(A) -> erlang:tuple_to_list(A) end, R2)),
    Catalogue = [
                 [{id, 1}, {title, <<"Салаты"/utf8>>}],
                 [{id, 2}, {title, <<"Закуски, бутерброды"/utf8>>}],
                 [{id, 3}, {title, <<"Первые блюда"/utf8>>}],
                 [{id, 4}, {title, <<"Вторые блюда"/utf8>>}],
                 [{id, 5}, {title, <<"Рыбные блюда"/utf8>>}],
                 [{id, 6}, {title, <<"Сладкая выпечка"/utf8>>}],
                 [{id, 7}, {title, <<"Несладкая выпечка"/utf8>>}],
                 [{id, 8}, {title, <<"Блины, оладьи"/utf8>>}],
                 [{id, 9}, {title, <<"Торты, пирожные"/utf8>>}],
                 [{id, 10}, {title, <<"Десерты"/utf8>>}],
                 [{id, 12}, {title, <<"Интересности"/utf8>>}],
                 [{id, 13}, {title, <<"Мясные блюда"/utf8>>}],
                 [{id, 14}, {title, <<"Суши, роллы"/utf8>>}],
                 [{id, 15}, {title, <<"Напитки, коктейли"/utf8>>}]
                ], 
   Res = [{lang, Lang}, {names, Names}, {types, Types}, {catalogue, Catalogue}]. 
    
get_recipes(Name, Text, Ingrs, CatalogId, Amount) ->
    {Recipes, Steps, Ingredients} = db_request(fun(Conn) -> {ok, Cols1, RecipesRows} = pgsql:squery(Conn, get_recipes_query(Name, Text, Ingrs, CatalogId, Amount)),
                                                  Recipes = lists:map(fun(A) -> erlang:tuple_to_list(A) end, RecipesRows),
                                                  RecipesIds = [ erlang:binary_to_integer(Id) || [Id | Tail] <- Recipes],
                                                  {ok, Cols2, StepsRows} = pgsql:squery(Conn, get_recipe_steps_query(RecipesIds)),
                                                  {ok, Cols3, IngrsRows} = pgsql:squery(Conn, get_ingredients_query(RecipesIds)),
                                                  Steps = lists:map(fun(A) -> erlang:tuple_to_list(A) end, StepsRows),
                                                  Ingredients = lists:map(fun(A) -> erlang:tuple_to_list(A) end, IngrsRows),
                                                  {Recipes, Steps, Ingredients}
                                                  
                                     end).

update_products_price(Values) -> %%%`
    {ok, Count} = db_request(fun(Conn) -> pgsql:equery(Conn, "update price from products values ($1, $2, $3)", [Values]) end). %% generic request

new_user(Login, Password) ->
    db_request(fun(Conn) -> 
                       case pgsql:equery(Conn, ?select_user_query, [Login]) of
                           {ok, Columns, Rows} when length(Rows) > 0 -> 
                               {error, <<"login is busy">>};
                           _ -> 
                               case pgsql:equery(Conn, ?new_user_query, [Login, Password]) of
                                   {ok, _} -> {ok, <<"ok">>};
                                   {error, _} -> {error, <<"db error">>};
                                   _ -> {error, <<"error invalid">>}
                               end
                       end
               end).
                           
user_is_registred(Login) ->
    db_request(fun(Conn) -> 
                       case pgsql:equery(Conn, ?select_user_query, [Login]) of
                           {ok, Columns, Rows} when length(Rows) > 0 -> {ok, <<"user is registred">>};
                           _ -> {error, <<"user is not registred">>}
                       end
               end).

user_is_auth(Login, Password) ->
    db_request(fun(Conn) -> 
                       case pgsql:equery(Conn, ?exists_user_query, [Login, Password]) of
                           {ok, Columns, Rows} when length(Rows) > 0 -> {ok, <<"ok">>};
                           _ -> {error, <<"error">>}
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
                           {ok, Columns, Rows = [{Attr}|_Tail]} when length(Rows) > 0, Attr == null -> {ok, <<"null">>};
                           {ok, Columns, Rows = [{Attr}|_Tail]} when length(Rows) > 0 -> {ok, Attr};
                           _ -> {error, <<"db error">>}
                       end
               end).

set_user_attr(Login, Query, Attr) ->    
    db_request(fun(Conn) -> 
                       case pgsql:equery(Conn, Query, [Login, Attr]) of
                           {ok, Count} -> {ok, <<"ok">>};
                           _ -> {error, <<"db error">>}
                       end
               end).
%% queries

get_lists_query(<<"ru">>) ->
    "SELECT DISTINCT name FROM products; SELECT DISTINCT types[1] FROM products";
get_lists_query(<<"en">>) ->
    "SELECT DISTINCT en_name FROM products; SELECT DISTINCT types[1] FROM products".

get_recipe_steps_query(RecipesIds) ->
    "SELECT " 
        ++ "cooksteps.cookbook_id, "
        ++ "cooksteps.src_big, "
        ++ "cooksteps.text "
        ++ "FROM "
        ++ "public.cookbook,"
        ++ "public.cooksteps "
        ++ "INNER JOIN ( "
        ++ "SELECT unnest (" ++ pg_int_array(RecipesIds) ++ ") AS tbl_id "
        ++ ") x ON cooksteps.cookbook_id = tbl_id "
        ++ "WHERE "
        ++ "cookbook.cookbook_id = cooksteps.cookbook_id;".

get_ingredients_query(RecipesIds) ->
    "SELECT " 
	++ "cookingredients.cookbook_id, "
	++ "ingr.title_ru, "
	++ "cookingredients.count, "
	++ "types.title_ru, "
	++ "cookingredients.comment "
        ++ "FROM cookingredients "
        ++ "INNER JOIN ( "
        ++ "SELECT unnest (" ++ pg_int_array(RecipesIds) ++ ") AS tbl_id "
        ++ ") x ON cookingredients.cookbook_id = tbl_id "
        ++ "INNER JOIN cookbookingredients as ingr ON  cookingredients.cookbookingredients_id = ingr.cookbookingredients_id "
        ++ "INNER JOIN cookbookingredientstype as types ON cookingredients.cookbookingredientstype_id = types.cookbookingredientstype_id "
        ++ "GROUP BY cookingredients.cookbook_id, "
	++ "ingr.title_ru, "
	++ "cookingredients.count, "
	++ "types.title_ru, "
	++ "cookingredients.comment "
        ++ "ORDER BY cookingredients.cookbook_id; ".

get_recipes_query(Name, Text, Ingrs, CatalogId, Amount) ->
    "SELECT DISTINCT cookbook.cookbook_id, cookbook.title, cookbook.timecooking, cookbook.servingsnumber, "
	++ "cookbook.isstepphoto, cookbook.description "
        ++ "FROM cookbook "
        ++ "INNER JOIN cookingredients as ingrs ON ingrs.cookbook_id = cookbook.cookbook_id "
        ++ "INNER JOIN cookbookingredients as ingrname ON ingrname.cookbookingredients_id = ingrs.cookbookingredients_id "
        ++ "INNER JOIN cooksteps as steps ON steps.cookbook_id = cookbook.cookbook_id "
        ++ "WHERE "
	++ like_ingrs(Ingrs)
	++ "(steps.text ILIKE '%" ++ Text ++"%' OR cookbook.description ILIKE '%" ++ Text ++ "%') "
        ++ and_catalog(CatalogId)
	++ " AND "
	++ "cookbook.title ILIKE '%" ++ Name ++ "%' "
	++ " AND "
	++ "(cookbook.timecooking >= 0 AND cookbook.timecooking < 30) " %%time
        ++ "ORDER BY cookbook.cookbook_id "
        ++ "LIMIT " ++ integer_to_list(Amount) ++";".

and_catalog(0) ->
    "";
and_catalog(Catalog)->
    " AND cookbook.cookbookcategory_id = " ++ integer_to_list(Catalog).

like_ingrs([]) ->
    "";
like_ingrs(List) ->
    "(" ++ like_ingrs0(List) ++ ") AND ".

like_ingrs0([]) ->
    [];
like_ingrs0([Ingr | []]) ->
    like_ingr(Ingr);
like_ingrs0([Ingr | Tail]) ->
    like_ingr(Ingr) ++ " OR " ++ like_ingrs0(Tail).

like_ingr(Ingr) ->
    " ingrname.title_ru ILIKE '%" ++ Ingr ++ "%' ".

-spec get_products_query({ [string()], [string()] }) -> string().
get_products_query(_ExcludeItems = {ExcludeTypes, ExcludeProducts}) ->
    "SELECT name, en_name, types, calories, price, fats, proteins, carbohydrates, ca, pho, na, ka, hl, mg," ++
       "fe, zi, se, ft, jo, a, e, d, k, c, b1, b2, b5, b6, bc, b12, pp," ++ 
        "h FROM products" ++ where(and_(not_types(ExcludeTypes), not_products(ExcludeProducts))).

where([]) ->
    ";";
where(Condition) ->
    " WHERE " ++ Condition ++ ";".

and_(Val1, Val2) when Val1 == []; Val2 == [] ->
    Val1 ++ Val2;
and_(Val1, Val2) ->
    Val1 ++ " AND " ++ Val2.    

not_types([]) ->
    [];
not_types(ExcludeTypes) ->
    "NOT types && " ++ pg_array(ExcludeTypes).

not_products([]) ->
    [];
not_products(ExcludeProducts) ->
    "NOT ARRAY[name] && " ++ pg_array(ExcludeProducts).
%% utilite

pg_int_array(ListInt) ->
    "ARRAY[" ++ string:join(lists:map(fun(X) -> erlang:integer_to_list(X) end, ListInt), ",") ++ "]".

pg_array(ListString) ->
    "ARRAY['" ++ string:join(binlist_to_list(ListString), "','") ++ "']".

binlist_to_list(List) ->
    lists:map(fun(A) -> binary:bin_to_list(A) end, List).
