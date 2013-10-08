-module(http_handler).

%% cowboy handler
-export([init/3, handle/2, terminate/3]).


init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Key, Req3} = cowboy_req:binding(key, Req2),
    % if_longer_key(Key), %% exit
    Req4 = case Method of 
               <<"PUT">> ->
                   ; 
               <<"POST">> ->
                   ;
               _ ->
                   
           end,
    {ok, Req4, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% utilite function

