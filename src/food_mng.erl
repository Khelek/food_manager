-module(food_mng).



-compile(export_all).

start() ->
    application:start(crypto),
    application:start(ranch), 
    application:start(cowlib),
    application:start(cowboy),
    application:start(food_mng).
