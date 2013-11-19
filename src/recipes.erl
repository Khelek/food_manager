-module(recipes).
%% cowboy handler
-export([get/1]).

-include("include/records.hrl").

-compile(export_all).

get(FindData) ->
    {ok, #recipes{recipes = lists:duplicate(FindData#find_recipes.amount, recipe_dummy())}}.

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
