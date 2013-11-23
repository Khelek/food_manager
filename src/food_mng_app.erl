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
    IP = {62,76,185,46},
    {ok, _} = cowboy:start_http(http, 100, [{reuseaddr, true}, {port, Port}], [
                                                            {env, [{dispatch, Dispatch}]}
                                                           ]),
    food_mng_sup:start_link().

stop(_State) ->
    ok.

ip() ->
    case os:getenv("OPENSHIFT_DIY_IP") of
        false ->
            {127,0,0,1};
        Other ->
            {ok, IP} = inet:parse_address(Other),
            IP
    end.

port() ->
    case os:getenv("OPENSHIFT_DIY_PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.
