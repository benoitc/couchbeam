ERL          ?= erl
ERLC		     ?= erlc
APP          := couchbeam
REBAR?= $(shell which rebar3)

.PHONY: doc

all: compile

compile:
	@$(REBAR) compile

doc:
	@$(REBAR) edoc

test:
	@$(REBAR) eunit
	
clean:
	@$(REBAR) clean

distclean: clean
	@rm -rf _build

dialyzer: 
	@$(REBAR) dialyzer
