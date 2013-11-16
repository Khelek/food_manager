-module(recipes).
%% cowboy handler
-export([get/1]).

-include("include/records.hrl").

-compile(export_all).

get(FindData) ->
    {ok, Client} = cowboy_client:init([]),
    Host = <<"http://localhost:8090/ahh">>, 
    Data = cowboy_client:request(<<"POST">>, Host, 
                                 [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
                                  {<<"Accept">>, <<"text/javascript, application/javascript, application/ecmascript, application/x-ecmascript
">>}],
                                 request(FindData), Client),    
    {ok, #recipes{recipes = lists:duplicate(FindData#find_recipes.amount, recipe_dummy())}}.
test() ->
    {ok, Client} = cowboy_client:init([]),
    Host = <<"http://www.koolinar.ru/recipe/fsearch">>, 
    {ok, Client2} = cowboy_client:request(<<"POST">>, Host, 
                                 [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
                                  {<<"Accept">>, <<"text/javascript, application/javascript, application/ecmascript, application/x-ecmascript">>}],
                                 request(#find_recipes{in_all = <<"молоко">>}), Client),
    {ok, _Status, Header, Client3} = cowboy_client:response(Client2),
    {ok, Transport, Socket} = cowboy_client:transport(Client3),
    Res = lists:foldl(fun(A, Acc) ->
                              case A of
                                  {ok, R} ->
                                      <<Acc/binary, R/binary>>;
                                  _ -> Acc
                              end
                      end, element(7, Client3), 
                      [Transport:recv(Socket, 0, 1000) || L <- lists:seq(1, 30)]),
    Res.


get_chunked(Acc, Socket, Transport) ->
    case Transport:recv(Socket, 0, 3000) of
        {ok, 200, Header, Client2} ->
            {ok, Transport, Socket} = cowboy_client:transport(Client2),
            {ok, R} = Transport:recv(Socket, 0, 3000),
            Acc2 = <<Acc/binary, R/binary>>,
            get_chunked(Acc2, Socket, Transport);
        _ -> Acc
    end.

request() ->
    request(#find_recipes{in_all = <<"молоко">>}).

request(FindData) -> request(1, FindData).
 
request(Page, #find_recipes{in_all = InAll, in_name = InName, in_text = InText, 
                      in_ingredients = InIngr, time_from = TimeFrom, time_to = TimeTo,
                      catalog_id = Catalog, complexity = Complexity, amount = Amount}) ->
    erlang:list_to_binary("utf8=%E2%9C%93" ++
        "&in_all=" ++ erlang:binary_to_list(InAll) ++ 
        "&where%5Bname%5D=" ++ erlang:binary_to_list(InName) ++
        "&where%5Brecipe_text%5D=" ++ erlang:binary_to_list(InText) ++
        "&where%5Bmaterial%5D=" ++ erlang:binary_to_list(InIngr) ++
        "&kitchen%5Bname%5D=" ++
        "&page=" ++ erlang:integer_to_list(Page) ++
        "&sort=rating_favorit+DESC" ++
        "&time%5Bfrom%5D=" ++ erlang:integer_to_list(TimeFrom) ++
        "&time%5Bto%5D=" ++ erlang:integer_to_list(TimeTo) ++
        "&dff=&dtf=&with%5Bcatalog_top_id%5D=" ++ erlang:integer_to_list(Catalog) ++
        "&with%5Bcatalog_sub_id%5D=0" ++
        "&complexity=" ++ erlang:integer_to_list(Complexity) ++
        "&commit=%D0%98%D1%81%D0%BA%D0%B0%D1%82%D1%8C").

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
