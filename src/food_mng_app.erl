-module(food_mng_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(ranch),
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                              {"/", http_receiver, []},
                                              {"/user", http_receiver, [user]},
                                              {"/table", http_receiver, [table]},
                                              {"/shoplist", http_receiver, [shoplist]},
                                              {"/recipes", http_receiver, [recipes]},
                                              {"/lists", http_receiver, [lists]}
                                            ]}
                                     ]),
    Port = 8083,
    IP = ip(),
    {ok, _} = cowboy:start_http(http, 100, [{reuseaddr, true}, {port, Port}, {ip, IP}
                                           ], [
                                                            {env, [{dispatch, Dispatch}]}
                                                           ]),
    food_mng_sup:start_link().

ip() ->
    case os:getenv("USER") of
        "haukot" ->
            {0,0,0,0};
        _ ->
            {62,76,185,46}
    end.

stop(_State) ->
    ok.
