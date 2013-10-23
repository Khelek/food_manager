-module(http_receiver).
%% cowboy handler
-export([init/3, handle/2, terminate/3]).

-include("../include/records.hrl").

%% API.

init(_Transport, Req, []) ->
    {ok, Req, undefined};
init(_Transport, Req, Opts) ->
    {ok, Req, Opts}.


handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    ReqResultate = case Method of
                       <<"GET">> ->
                           ok;
                       <<"PUT">> -> 
                           cowboy_req:reply(Req2, cowboy_req:body(infinity, Req2)); % заменить таблицу 
                       <<"POST">> ->
                           {ok, Json, Req3} = cowboy_req:body(infinity, Req2),
                           Data = decoder(Json), 
                           Res = case handle_worker(State, Method, Data) of
                                     {error, Reason} -> #answer{response = <<"error">>};
                             {ok, ResData} -> ResData
                                 end,
                           ResJson = encoder(Res), 
                           cowboy_req:reply(Req3, ResJson);
                       _ ->
                           some          
                   end,
    {ok, ReqResultate, State};
handle(Req, State = undefined) ->
    {ok, Req, State}. %ошибка

handle_worker([user], Method, Data) ->
    ;
handle_worker([table], Method, Data) ->
    processor:process_products(Data);
handle_worker([shoplist], Method, Data) ->
    .

terminate(_Reason, _Req, _State) ->
    ok.

%% utilite function

encoder() ->
     jsonx:encoder([{person,   record_info(fields, person)},
                    {auth,  record_info(fields, auth)},
                    {answer, record_info(answer)},
                    {table, record_info(table)},
                    {product, record_info(product)}]).
decoder() ->
     jsonx:decoder([{person,   record_info(fields, person)},
                    {auth,  record_info(fields, auth)},
                    {answer, record_info(answer)},
                    {table, record_info(table)},
                    {product, record_info(product)}]).
