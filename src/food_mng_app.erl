-module(food_mng_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                              {"/", http_receiver, []},
                                              {"/user", http_receiver, [user]},
                                              {"/table", http_receiver, [table]},
                                              {"/shoplist", http_receiver, [shoplist]}
                                            ]}
                                     ]),
    Port = 8081, %% сделать переменной application
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
                                                            {env, [{dispatch, Dispatch}]}
                                                           ]),
    food_mng_sup:start_link().

stop(_State) ->
    ok.
