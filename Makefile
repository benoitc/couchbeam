ERL          ?= erl

EBIN_DIRS    := $(wildcard deps/*/ebin)
ERLC_FLAGS := -W $(INCLUDE_DIRS:%=-I %) $(EBIN_DIRS:%=-pa %)
APP          := couchbeam

erl:
	./rebar compile

docs:
	@mkdir -p doc
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: all
	prove t/*.t

cover: all
	COVER=1 prove t/*.t
	erl -detached -noshell -eval 'etap_report:create()' -s init stop

clean: 
	./rebar clean


