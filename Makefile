ERL          ?= erl
ERLC		     ?= erlc
APP          := couchbeam

all:
	./rebar compile

docs:
	@mkdir -p doc
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: all
	@$(ERLC) -o t/ t/etap.erl
	prove t/*.t

cover: all
	COVER=1 prove t/*.t
	@$(ERL) -detached -noshell -eval 'etap_report:create()' -s init stop

clean: 
	./rebar clean
	@rm t/*.beam


