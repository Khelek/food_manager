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
                                              {"/", http_receiver []}
                                            ]}
                                     ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
                                                            {env, [{dispatch, Dispatch}]}
                                                           ]),
    food_mng_sup:start_link().

stop(_State) ->
    ok.
