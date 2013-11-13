-module(algo).
-export([solve_equations/2]).


solve_equations(ListOfLists, Coeff) ->
    Conn = erserve:open("localhost", 8082), 
    {ok, Rdata} = erserve:eval(Co1, "calculatediet(" ++ matrix_to_string(ListOfLists) ++ "," ++ vector_to_string(Coeff) ++ ")"),
    {xt_has_attr, {Tag, {xt_array_double, Vector}}} = Rdata,
    {xt_list_tag,[{{xt_str,<<"dim">>},{xt_array_int,[Rows, Cols]}}]} = Tag,
    Solves = divide(Rows, Vector),
    ok = erserve:close(Conn),
    Solves.

matrix_to_string(Matrix) ->
    "cbind(" ++ string:join(lists:map(fun vector_to_string/1, Matrix), ",") ++ ")". 

vector_to_string(Vec) ->
    "c(" ++ string:join(lists:map(fun(A) -> case A of
                                        A when is_integer(A) -> erlang:integer_to_list(A);
                                        A when is_float(A) -> erlang:float_to_list(A);
                                        A when is_list(A) -> A;
                                        A when is_binary(A) -> binary:bin_to_list(A)
                                    end 
                          end, Vec), ",") ++ ")".
divide(Len, []) ->
    [];
divide(Len, List) ->
    [H, T] = lists:split(Len, List),
    [H | divide(Len, T)].
