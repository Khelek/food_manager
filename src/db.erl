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

get_products(ExcludeItems = {ExcludeTypes, ExcludeProducts}) ->
    %% erlang:display([get_products_query(ExcludeItems)]), %%% lager!11
    CategoryList = [
                    {"Алкоголь", 10},
                    {"Напитки", 10},
                    {"Грибы", 6},
                    {"Жиры растительные и животные", 10},
                    {"Кондитерские изделия", 10},
                    {"Крупы, злаковые, бобы", 10},
                    {"Молочные продукты", 10},
                    {"Мука, крахмал и макароны", 5},
                    {"Мясные продукты", 10},
                    {"Овощи и зелень", 12},
                    {"Орехи и семена", 5},
                    {"Рыба и морепродукты", 10},
                    {"Специи, приправы и соусы", 10},
                    {"Фрукты и ягоды", 15},
                    {"Хлебобулочные изделия", 5},
                    {"Яйца", 3}
                   ],
    Query = string:join(lists:map(fun({Category, Limit}) ->
                      get_products_by_category_query(Category, Limit, ExcludeProducts) 
              end, CategoryList), " "),
    erlang:display(Query),
    ResponseList = db_request(fun(Conn) -> pgsql:squery(Conn, Query) end),
    Products = lists:map(fun(Row) ->
                                 erlang:tuple_to_list(Row) 
                         end,
                         lists:flatten(lists:map(fun({ok, Cols, Rows}) -> Rows end, ResponseList))),
    lists:map(fun(X) -> make_nutrients_list(X) end, fold_map_by_name(Products)).

get_lists(Lang) ->
    [{ok, _C1, R1},{ok, _C2, R2},{ok, _C3, R3}] = db_request(fun(Conn) -> pgsql:squery(Conn, get_lists_query(Lang)) end),
    Names = lists:flatten(lists:map(fun(A) -> erlang:tuple_to_list(A) end, R1)),
    Types = lists:flatten(lists:map(fun(A) -> erlang:tuple_to_list(A) end, R2)),
    Catalogue =  lists:map(fun({Id, Name}) -> [{id, erlang:binary_to_integer(Id)}, {title, Name}] end, R3) ,
    [{lang, Lang}, {names, Names}, {types, Types}, {catalogue, Catalogue}]. 
    
get_recipes(Name, Text, Ingrs, CatalogId, Amount) ->
    {_Recipes, _Steps, _Ingredients} = db_request(fun(Conn) -> {ok, _Cols1, RecipesRows} = pgsql:squery(Conn, get_recipes_query(Name, Text, Ingrs, CatalogId, Amount)),
                                                  Recipes = lists:map(fun(A) -> erlang:tuple_to_list(A) end, RecipesRows),
                                                  RecipesIds = [ erlang:binary_to_integer(Id) || [Id | _Tail] <- Recipes],
                                                  {ok, _Cols2, StepsRows} = pgsql:squery(Conn, get_recipe_steps_query(RecipesIds)),
                                                  {ok, _Cols3, IngrsRows} = pgsql:squery(Conn, get_ingredients_query(RecipesIds)),
                                                  Steps = lists:map(fun(A) -> erlang:tuple_to_list(A) end, StepsRows),
                                                  Ingredients = lists:map(fun(A) -> erlang:tuple_to_list(A) end, IngrsRows),
                                                  {Recipes, Steps, Ingredients}
                                     end).

update_products_price(Values) -> %%%`
    {ok, Count} = db_request(fun(Conn) -> pgsql:equery(Conn, "update price from products values ($1, $2, $3)", [Values]) end). %% generic request

new_user(Login, Password) ->
    db_request(fun(Conn) -> 
                       case pgsql:equery(Conn, ?select_user_query, [Login]) of
                           {ok, _Columns, Rows} when length(Rows) > 0 -> 
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
                           {ok, _Columns, Rows} when length(Rows) > 0 -> {ok, <<"user is registred">>};
                           _ -> {error, <<"user is not registred">>}
                       end
               end).

user_is_auth(Login, Password) ->
    db_request(fun(Conn) -> 
                       case pgsql:equery(Conn, ?exists_user_query, [Login, Password]) of
                           {ok, _Columns, Rows} when length(Rows) > 0 -> {ok, <<"ok">>};
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
                           {ok, _Columns, Rows = [{Attr}|_Tail]} when length(Rows) > 0, Attr == null -> {ok, <<"null">>};
                           {ok, _Columns, Rows = [{Attr}|_Tail]} when length(Rows) > 0 -> {ok, Attr};
                           _ -> {error, <<"db error">>}
                       end
               end).

set_user_attr(Login, Query, Attr) ->    
    db_request(fun(Conn) -> 
                       case pgsql:equery(Conn, Query, [Login, Attr]) of
                           {ok, _Count} -> {ok, <<"ok">>};
                           _ -> {error, <<"db error">>}
                       end
               end).

%% queries

get_lists_query(<<"ru">>) ->
    "SELECT DISTINCT name FROM products; SELECT name FROM products_category; 
     SELECT cookbookcategory_id, title FROM cookbookcategory WHERE cookbookcategory_id < 16";
get_lists_query(<<"en">>) ->
    "SELECT DISTINCT en_name FROM products; SELECT en_name FROM products_category;
     SELECT cookbookcategory_id, title FROM cookbookcategory WHERE cookbookcategory_id < 16".

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

get_products_by_category_query(Category, Limit, ExcludeProducts) ->
    "SELECT prod.name, nutrients.name, info.value "
        ++ "FROM (SELECT products._id, products.name FROM products "
        ++ "INNER JOIN products_category ON products_category._id = products.category "
        ++ "WHERE products_category.name = '" ++ Category ++ "' "
        ++ and_(not_products(ExcludeProducts))
        ++ "ORDER BY random() LIMIT " ++ integer_to_list(Limit) ++ ") as prod "
        ++ "INNER JOIN info ON info.product = prod._id "
        ++ "INNER JOIN nutrients ON info.nutrient = nutrients._id "
        ++ "AND ARRAY[nutrients.name] && ARRAY['Калорийность', 'Стоимость', 'Белки', 'Углеводы', 'Жиры'] "
        ++ "ORDER BY prod.name;".
%% sql

where([]) ->
    " ";
where(Condition) ->
    " WHERE " ++ Condition ++ " ".

and_(Val) when Val == [] ->
    [];
and_(Val) ->
    "AND " ++ Val.

and_(Val1, Val2) when Val1 == []; Val2 == [] ->
    Val1 ++ Val2;
and_(Val1, Val2) ->
    Val1 ++ " AND " ++ Val2.    

not_types([]) ->
    [];
not_types(ExcludeTypes) ->
    "NOT ARRAY[products_category.name] && " ++ pg_array(ExcludeTypes).

not_products([]) ->
    [];
not_products(ExcludeProducts) ->
    "NOT ARRAY[products.name] && " ++ pg_array(ExcludeProducts).

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

%% utilite

pg_int_array(ListInt) ->
    " ARRAY[" ++ string:join(lists:map(fun(X) -> erlang:integer_to_list(X) end, ListInt), ",") ++ "] ".

pg_array(ListString) ->
    " ARRAY['" ++ string:join(binlist_to_list(ListString), "','") ++ "'] ".

binlist_to_list(List) ->
    lists:map(fun(A) -> binary:bin_to_list(A) end, List).

fold_map_by_name(_List = [[Name | Body] | T]) ->
    fold_map_by_name(lists:append([Name], [list_to_tuple(Body)]), T). % [name, {a,b,c}, {d,e,f}]

fold_map_by_name(ElemAcc, []) -> [ElemAcc];
fold_map_by_name(ElemAcc = [Name | _T], _List = [[Name | CurrentBody] | Tail]) ->
    fold_map_by_name(lists:append(ElemAcc, [list_to_tuple(CurrentBody)]), Tail);
fold_map_by_name(ElemAcc, [[ElemName | ElemBody] | Tail]) ->
    [ElemAcc | fold_map_by_name(lists:append([ElemName], [list_to_tuple(ElemBody)]), Tail)].

make_nutrients_list(NameMap = [Name | _Body]) ->
    Vitamins = [<<"Стоимость">>, <<"Калорийность">>, <<"Жиры">>, <<"Белки">>, <<"Углеводы">>, <<"Кальций (Ca)">>, <<"Фосфор (P)">>, 
                 <<"Натрий (Na)">>, <<"Калий (K)">>, <<"Хлор (Cl)">>, <<"Магний (Mg)">>,
                 <<"Железо (Fe)">>, <<"Цинк (Zn)">>, <<"Селен (Se)">>, <<"Фтор (F)">>, <<"Йод (I)">>, 
                 <<"Витамин А">>, <<"Витамин Е">>, <<"Витамин D">>, <<"Витамин K">>, <<"Витамин С">>,
                 <<"Витамин В1">>, <<"Витамин В2">>, <<"Витамин В5">>, <<"Витамин В6">>, <<"Витамин В9">>,
                 <<"Витамин В12">>, <<"Витамин РР">>, <<"Витамин Н">>],
    VitaminsValues = lists:map(fun(X) ->
                                       case lists:keyfind(X, 1, NameMap) of
                                           {VitName, Value} when VitName == <<"Витамин А">>;   VitName == <<"Витамин В9">>;
                                                                 VitName == <<"Витамин В12">>; VitName == <<"Витамин Н">>;
                                                                 VitName == <<"Йод (I)">>;     VitName == <<"Селен (Se)">>;
                                                                 VitName == <<"Фтор (F)">>;    VitName == <<"Цинк (Zn)">>;
                                                                 VitName == <<"Витамин В5">> 
                                                                 -> 0.001 * bin_to_float(Value); %% мкг
                                           {_VitaminName, Value} -> bin_to_float(Value);
                                           false -> 0.0
                                       end
                               end, Vitamins),
    [Name | VitaminsValues].

bin_to_float(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> float(list_to_integer(N));
        {F,_Rest} -> F
    end.
%% tests

test_fold() ->
    A = [[1, "a", "b"],
         [1, "c", "d"],
         [1, "3", "4"],
         [2, "23"],
         [2, "43"],
         ["adr", 1, 4, 5],
         ["adr", 34, 23, 21]
        ],
    fold_map_by_name(A).

tc() ->    
    A = [{<<"Вино сухое белое"/utf8>>,<<"Белки">>,<<"0.2">>},
         {<<"Вино сухое белое"/utf8>>,<<"Углеводы">>,<<"0.3">>},
         {<<"Вино сухое белое"/utf8>>,<<"Калорийность"/utf8>>,<<"64">>},
         {<<"Вино сухое белое"/utf8>>,<<"Пищевые волокна"/utf8>>,<<"1.6">>},
         {<<"Вино сухое белое"/utf8>>,<<"Витамин В2"/utf8>>,<<"0.01">>},
         {<<"Вино сухое белое"/utf8>>,<<"Витамин РР"/utf8>>,<<"0.1">>},
         {<<"Вино сухое красное"/utf8>>,<<"Белки">>,<<"0.2">>},
         {<<"Вино сухое красное"/utf8>>,<<"Углеводы"/utf8>>,<<"0.3">>},
         {<<"Вино сухое красное"/utf8>>,<<"Калорийность">>,<<"64">>},
         {<<"Вино сухое красное"/utf8>>,<<"Сахара"/utf8>>,<<"0.3">>},
         {<<"Вино сухое красное"/utf8>>,<<"Пищевые волокна"/utf8>>,<<"1.6">>},
         {<<"Вино сухое красное"/utf8>>,<<"Витамин В2"/utf8>>,<<"0.01">>},
         {<<"Вино ликерное"/utf8>>,<<"Вода"/utf8>>,<<"54.1">>},
         {<<"Вино ликерное"/utf8>>,<<"Белки"/utf8>>,<<"0.5">>},
         {<<"Вино ликерное"/utf8>>,<<"Углеводы"/utf8>>,<<"30">>},
         {<<"Вино ликерное"/utf8>>,<<"Калорийность"/utf8>>,<<"212">>},
         {<<"Вино ликерное"/utf8>>,<<"Сахара"/utf8>>,<<"30">>},
         {<<"Вино ликерное"/utf8>>,<<"Пищевые волокна"/utf8>>,<<"1.6">>},
         {<<"Вино столовое красное"/utf8>>,<<"Белки"/utf8>>,<<"0.07">>},
         {<<"Вино сухое белое"/utf8>>,<<"Стоимость">>,<<"1000">>},
         {<<"Вино столовое красное"/utf8>>,<<"Углеводы"/utf8>>,<<"2.61">>},
         {<<"Вино столовое красное"/utf8>>,<<"Калорийность"/utf8>>,<<"85">>},
         {<<"Вино столовое красное"/utf8>>,<<"Сахара"/utf8>>,<<"0.62">>}], 
    AList = lists:map(fun(X) -> erlang:tuple_to_list(X) end, A),
    lists:map(fun(X) -> make_nutrients_list(X)  end, fold_map_by_name(AList)).
    
