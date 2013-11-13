REBAR = ./rebar

all: dependencies compile

dependencies:
	$(REBAR) get-deps
compile:
	$(REBAR) compile 
tests: 
	$(REBAR) -C rebar.config get-deps compile
	ERL_FLAGS="-config app.config -pa ebin deps/*/ebin" $(REBAR) -C rebar.config skip_deps=true eunit 
clean:
	$(REBAR) clean 
start:
	erl -pa ebin deps/*/ebin +pc unicode -s food_mng
rserve:
	R -f r_src/rserve.r --gui-none --no-save
deploy:
	git push heroku master
logs:
	heroku logs -t
