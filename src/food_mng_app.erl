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
                                              {"/shoplist", http_receiver, [shoplist]}
                                            ]}
                                     ]),
    Port = port(),
    erlang:display(os:getenv("DATABASE_URL")),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
                                                            {env, [{dispatch, Dispatch}]}
                                                           ]),
    food_mng_sup:start_link().

stop(_State) ->
    ok.

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.
