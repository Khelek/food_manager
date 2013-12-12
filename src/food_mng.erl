-module(food_mng).



-compile(export_all).

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch), 
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(goldrush),
    ok = application:start(lager),
    ok = application:start(food_mng).
