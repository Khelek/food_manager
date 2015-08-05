-module(http_receiver).
%% cowboy handler
-export([init/3, handle/2, terminate/3]).

-include("include/records.hrl").
-compile(export_all).

%% API.

init(_Transport, Req, []) ->
    {ok, Req, undefined};
init(_Transport, Req, [Opts]) ->
    {ok, Req, Opts}.

u() ->
    #find_recipes{}.

handle(Req, State = undefined) ->
    {ok, Req, State}; %ошибка
handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    ReqResultate =
           case Method of
               <<"GET">> ->
                   {Echo, Req3} = cowboy_req:qs_val(<<"lang">>, Req2), %% case lang of ru, en
                   case Echo of
                       <<"ru">> ->
                           Res = db:get_lists(<<"ru">>),
                           ResJson = jsonx:encode(Res),
                           cowboy_req:reply(200, [{<<"content-type">>,<<"application/json; charset=utf-8">>}], ResJson, Req3);
                       <<"en">> ->
                           Res = db:get_lists(<<"en">>),
                           ResJson = jsonx:encode(Res),
                           cowboy_req:reply(200, [{<<"content-type">>,<<"application/json; charset=utf-8">>}], ResJson, Req3);
                       _ ->
                           cowboy_req:reply(400, [], <<"Languague is not.">>, Req)
                       end,
                   {ok, Req3, State};
               <<"PUT">> ->
                   cowboy_req:reply(Req2, cowboy_req:body(infinity, Req2)); %% no
               <<"POST">> ->
                   {ok, Json, Req3} = cowboy_req:body(infinity, Req2),
                   Data = decoder(Json),
                   Res = case handle_worker(State, Method, Data) of
                             {ok, ResData} -> #answer{success = true, response = ResData};
                             {error, Reason} -> #answer{success = false, response = Reason}
                         end,
                   ResJson = encoder(Res),
                   cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>},{<<"charset">>, <<"UTF-8">>}], ResJson, Req3);
               _ ->
                   some
           end,
    {ok, ReqResultate, State}.

handle_worker(user, <<"POST">>, #auth{login = BinLogin, password = BinPassword, registration = Registration}) ->
    Login = binary:bin_to_list(BinLogin),
    Password = binary:bin_to_list(BinPassword),
    _Response = case Registration of
        true ->
            db:new_user(Login, Password);
        false ->
            db:user_is_auth(Login, Password)
    end;
handle_worker(table, <<"POST">>,  #request{login = Login, action = <<"get">>, body = Data}) ->
    if_registred_do(fun() ->
                            try
                                {ok, processor:calculate_products_list(Data)}
                            catch
                                throw:Term -> {error, erlang:list_to_bitstring("Не получается найти решение с этими данными")};
                                exit:Reason -> {error, erlang:list_to_bitstring("Невозможно рассчитать ваше меню с текущими данными")};
                                error:{badmatch,
                                       {error,econnrefused}} -> restart_R(),
                                                                lager:error("R restarted"),
                                                                {error, erlang:list_to_bitstring("Не получилось рассчитать решение")};
                                error:Reason -> {error, erlang:list_to_bitstring("Невозможно найти решение с текущими данными")}
                            end
                    end, Login);
handle_worker(shoplist, <<"POST">>, #request{login = Login, action = <<"get">>}) ->
    if_registred_do(fun() ->
                            db:get_user_shoplist(Login)
                    end, Login);
handle_worker(shoplist, <<"POST">>, #request{login = Login, action = <<"set">>, body = Shoplist}) -> %% Shoplist - тупо строка
    if_registred_do(fun() ->
                            db:set_user_shoplist(Login, Shoplist)
                    end, Login);
handle_worker(recipes, <<"POST">>, #request{login = Login, action = <<"get">>, body = FindData}) ->
    if_registred_do(fun() ->
                            recipes:get(FindData)
                    end, Login).

terminate(_Reason, _Req, _State) ->
    ok.

if_registred_do(Fun, Login) ->
    case db:user_is_registred(Login) of
        {ok, _} -> Fun();
        {error, Reason} -> {error, Reason}
    end.
%% utilite function

restart_R() ->
    spawn(fun() -> os:cmd("nohup R -f r_src/rserve.r --gui-none --no-save") end).

encoder(Json) ->
    Encoder = jsonx:encoder([{person, record_info(fields, person)},
                             {auth, record_info(fields, auth)},
                             {answer, record_info(fields, answer)},
                             {table, record_info(fields, table)},
                             {request, record_info(fields, request)},
                             {product, record_info(fields, product)},
                             {find_recipes, record_info(fields, find_recipes)},
                             {recipes, record_info(fields, recipes)},
                             {step, record_info(fields, step)},
                             {recipe, record_info(fields, recipe)}],
                            [{format, proplist}]),
    Encoder(Json).

decoder(Json) ->
    Decoder = jsonx:decoder([{person, record_info(fields, person)},
                             {auth, record_info(fields, auth)},
                             {answer, record_info(fields, answer)},
                             {table, record_info(fields, table)},
                             {request, record_info(fields, request)},
                             {product, record_info(fields, product)},
                             {find_recipes, record_info(fields, find_recipes)},
                             {recipes, record_info(fields, recipes)},
                             {step, record_info(fields, step)},
                             {recipe, record_info(fields, recipe)}],
                            [{format, proplist}]),
    Decoder(Json).
