-module(matlab).
-export([solve_equations/2]).

solve_equations(ListOfLists, Coeff) ->
    ResultKey = "ResultOfNNLS:",
    MatlabAnswer = os:cmd("matlab  -nosplash -nodesktop -nodisplay -r \"warning off;" 
     ++ "x = libnnls([" ++ matrix_to_string(ListOfLists) ++ "], [" ++ vector_to_string(Coeff) ++ "]); disp('" ++ ResultKey ++ "');"
     ++ "disp(x); exit;\""),
    ((string:str(MatlabAnswer, ResultKey) == 0) andalso throw("No result.")),
    Strings = string:tokens(MatlabAnswer, "\n "), 
    [_Head | AnswerList] = lists:dropwhile(fun(A) -> not string:equal(A, ResultKey) end, Strings),
    _SolutionVector = lists:map(fun(A) -> {Float, _Rest} = string:to_float(A), Float end, AnswerList).

matrix_to_string(Matrix) ->
    string:join(lists:map(fun vector_to_string/1, Matrix), ";"). 

vector_to_string(Vec) ->
    string:join(lists:map(fun(A) -> case A of
                                        A when is_integer(A) -> erlang:integer_to_list(A);
                                        A when is_float(A) -> erlang:float_to_list(A);
                                        A when is_list(A) -> A;
                                        A when is_binary(A) -> binary:bin_to_list(A)
                                    end 
                          end, Vec), " ").
