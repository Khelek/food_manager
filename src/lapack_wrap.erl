-module(lapack_wrap).
-export([solve/2]).
-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    case code:which(load_average) of
        Filename when is_list(Filename) ->
            erlang:load_nif(filename:join([filename:dirname(Filename),
                                           "..","priv",
                                           "lapack_wrap"]), []);
        Reason when is_atom(Reason) ->
            {error, Reason}
    end.

-spec solve([[float()]], [float()]) -> {ok, [float()]} | { error, atom() }.
% @doc Returns system load average
solve(Products, Coefficients) ->
    erlang:nif_error(nif_library_not_loaded).

