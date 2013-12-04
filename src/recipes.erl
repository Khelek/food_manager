-module(recipes).
%% cowboy handler
-export([get/1]).

-include("include/records.hrl").

-compile(export_all).

get() ->
    recipes:get(#find_recipes{in_name = <<"">>, in_ingredients = <<"молоко">>, catalog_id = 6, amount = 10}).

get(Data) ->
    try
        Ingrs = string:tokens(binary:bin_to_list(Data#find_recipes.in_ingredients), ","),
        {Recipes0, Steps0, Ingredients0} = db:get_recipes(binary:bin_to_list(Data#find_recipes.in_name),
                                                          binary:bin_to_list(Data#find_recipes.in_text), 
                                                          Ingrs, Data#find_recipes.catalog_id,
                                                          Data#find_recipes.amount),
        Steps = lists:map(fun(X) -> lists:map(fun([Id, Src, Com]) -> 
                                                      {Id, #step{
                                                              photo_url = erlang:list_to_binary(lists:flatten(string:tokens( binary:bin_to_list(Src), "\\"))), comment = Com 
                                                             }} end, X) end, filter0(Steps0, Recipes0)),
        Ings = lists:map(fun(X) -> lists:map(fun([Id, Name, Count, Type, Comment]) ->
                                                     Space = <<" ">>,
                                                     {Id, <<Name/binary, Space/binary, Count/binary, Space/binary, 
                                                            Type/binary, Space/binary, Comment/binary>>}
                                             end, X) end, filter0(Ingredients0, Recipes0)),
        {ok, #recipes{recipes = 
                           [#recipe{name = Title, time = Time, number_portion = 0, steps_exists = (Issteps == <<"1">>),
                                     how_to_cook = Descr,
                                     ingredients = [ Text || {Id, Text} <- lists:flatten(lists:filter(fun([{Id, Str} | Tail]) -> Id == IdRec end, Ings))],
                                     steps = [ Obj || {Id, Obj} <- lists:flatten(lists:filter(fun([{Id, Obj} | Tail]) -> Id == IdRec end, Steps))] }
                              || [IdRec, Title, Time, Portion, Issteps, Descr] <- Recipes0]
                     }}
    catch
        _:_ -> {error, <<"Невозможно получить рецепты; попробуйте другой запрос">>}
    end.

filter0(S, Recipes) ->
    RecipesIds = [Id || [Id | Tail] <- Recipes],
    lists:map(fun(X) ->
                      filter(S, X) end, RecipesIds).
    

filter(S, RecipeId) ->
    lists:filter(fun([X | _Tail]) ->  RecipeId == X end, S).
                        

image_hash(Link) ->
    BinMd5 = crypto:hash(md5, Link),
    List = binary_to_list(BinMd5),
    End = lists:map(fun(X) -> string:substr(integer_to_list(16#100 bor 16#FF band X, 16), 2, 2) end, List),
    string:to_lower(lists:flatten(End)).

recipe_dummy() ->
     #recipe{name = <<"Булки с чаем"/utf8>>, time = <<"12 мин."/utf8>>, number_portion = 4, complexity = 1, ingredients = [<<"2 булки"/utf8>>, <<"1 чай"/utf8>>],
             steps_exists = true, how_to_cook = <<"см. шаги приготовления">>, 
             steps = [
                      #step{photo_url = <<"http://www.koolinar.ru/all_image/recipe_pictures/310/310683/rp310683_large.jpg"/utf8>>,
                       comment = <<"1. Берем булочки"/utf8>>},
                      #step{photo_url = <<"http://teatraditions.ru/upload/medialibrary/34e/70897532_1298108768_63_2.jpg"/utf8>>,
                       comment = <<"2. Завариваем чай"/utf8>>},
                      #step{photo_url = <<"http://www.fotografiy.net/_ph/131/2/52626611.jpg"/utf8>>,
                       comment = <<"3. Ставим булочки и чай на стол"/utf8>>},
                      #step{photo_url = <<"http://ic.pics.livejournal.com/pravozaschitneg/45578787/473/473_original.jpg"/utf8>>,
                       comment = <<"4. Съешь еще этих мягких французских булок да выпей чаю"/utf8>>}
                     ]}.
