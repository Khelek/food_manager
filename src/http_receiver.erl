-module(http_receiver).
%% cowboy handler
-export([init/3, handle/2, terminate/3]).

-include("include/records.hrl").
-compile(export_all).
%%


person() ->
     #person{}.

%% API.

init(_Transport, Req, []) ->
    {ok, Req, undefined};
init(_Transport, Req, Opts) ->
    {ok, Req, Opts}.


handle(Req, State = undefined) ->
    {ok, Req, State}; %ошибка
handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    ReqResultate = case Method of 
               <<"PUT">> -> % еще GET?
                   cowboy_req:reply(Req2, cowboy_req:body(infinity, Req2)); % заменить таблицу 
               <<"POST">> ->
                   {ok, Json, Req3} = cowboy_req:body(infinity, Req2),
                   Data = decoder(Json), 
                   Res = case handle_worker(State, Method, Data) of
                             {ok, ResData} -> #answer{success = true, response = ResData};
                             {error, Reason} -> #answer{success = false, response = Reason}
                         end,
                   ResJson = encoder(Res), 
                   cowboy_req:reply(Req3, ResJson);
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
handle_worker(table, <<"POST">>, Data) ->
    processor:calculate_products_list(Data); 
handle_worker(shoplist, <<"POST">>, #request{login = Login, action = <<"get">>}) ->
    db:get_user_shoplist(Login);
handle_worker(shoplist, <<"POST">>, #request{login = Login, action = <<"set">>, body = Shoplist}) -> %% Shoplist - тупо строка(или объект?)
    db:set_user_shoplist(Login, Shoplist).

terminate(_Reason, _Req, _State) ->
    ok.

%% utilite function

encoder(Json) ->
    Encoder = jsonx:encoder([{person,   record_info(fields, person)},
                             {auth,  record_info(fields, auth)},
                             {answer, record_info(fields, answer)},
                             {table, record_info(fields, table)},
                             {request, record_info(fields, request)},
                             {product, record_info(fields, product)}]),
    Encoder(Json).

decoder(Json) ->
    Decoder = jsonx:decoder([{person,   record_info(fields, person)},
                             {auth,  record_info(fields, auth)},
                             {answer, record_info(fields, answer)},
                             {table, record_info(fields, table)},
                             {request, record_info(fields, request)},
                             {product, record_info(fields, product)}],
                            [{format, proplist}]),
    Decoder(Json).

