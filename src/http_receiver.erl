-module(http_receiver).

%% cowboy handler
-export([init/3, handle/2, terminate/3]).
%% API.

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    ReqResultate = case Method of 
               <<"PUT">> ->
                   cowboy_req:reply(Req2, cowboy_req:body(infinity, Req2); %% для хранения таблицы, если потребуется 
               <<"POST">> ->
                   {ok, DataJson, Req3} = cowboy_req:body(16384, Req2), %% размер, infinity?
                   Data = jsonx:decode(DataJson), %% specs
                   Products = processor:process_products(Data),
                   ProductsJson = jsonx:encode(Products), %%
                   cowboy_req:reply(Req3, ProductsJson);
               _ ->
                   some          
           end,
    {ok, ReqResultate, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% utilite function

