-module(algo_wrap).
-export([solve_equations/2]).
-on_load(init/0).

init() ->
    case code:which(algo_wrap) of
        Filename when is_list(Filename) ->
            erlang:load_nif(filename:join([filename:dirname(Filename),
                                           "..","priv",
                                           "algo_wrap"]), []);
        Reason when is_atom(Reason) ->
            {error, Reason}
    end.

solve_equations(Arg1, Arg2) -> c_solve_eq(Arg1, Arg2).


% @doc Returns system load average
c_solve_eq(Products, Coefficients) ->
    erlang:nif_error(nif_library_not_loaded).

