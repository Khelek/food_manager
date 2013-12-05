REBAR = ./rebar

all: dependencies compile_all

dependencies:
	$(REBAR) get-deps
compile_all:
	$(REBAR) compile  
compile:
	$(REBAR) skip_deps=true compile  
tests: 
	$(REBAR) -C rebar.config get-deps compile
	ERL_FLAGS="-config app.config -pa ebin deps/*/ebin" $(REBAR) -C rebar.config skip_deps=true eunit 
clean:
	$(REBAR) clean 
localstart:
	kjell -pa ebin deps/*/ebin -s food_mng +pc unicode
rserve:
	nohup R -f r_src/rserve.r --gui-none --no-save --silent
serverstart:
	nohup erl -pa ebin deps/*/ebin -noshell -noinput -detached -s food_mng
