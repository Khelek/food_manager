-module(recipes).
%% cowboy handler
-export([get/1]).

-include("include/records.hrl").

get(FindData)->
    {ok, #recipes{recipes = lists:duplicate(FindData#find_recipes.amount, recipe_dummy())}}.

recipe_dummy() ->
     #recipe{name = <<"Булки с чаем">>, time = <<"12 мин.">>, number_portion = 4, complexity = 1, ingredients = [<<"2 булки">>, <<"1 чай">>],
             steps_exists = true, how_to_cook = <<"см. шаги приготовления">>, 
             steps = [
                      #step{photo_url = <<"http://www.koolinar.ru/all_image/recipe_pictures/310/310683/rp310683_large.jpg">>,
                       comment = <<"1. Берем булочки">>},
                      #step{photo_url = <<"http://teatraditions.ru/upload/medialibrary/34e/70897532_1298108768_63_2.jpg">>,
                       comment = <<"2. Завариваем чай">>},
                      #step{photo_url = <<"http://www.fotografiy.net/_ph/131/2/52626611.jpg">>,
                       comment = <<"3. Ставим булочки и чай на стол">>},
                      #step{photo_url = <<"http://ic.pics.livejournal.com/pravozaschitneg/45578787/473/473_original.jpg">>,
                       comment = <<"4. Съешь еще этих мягких французских булок да выпей чаю">>}
                     ]}.
