ERL          ?= erl
ERLC		     ?= erlc
APP          := couchbeam

all: doc
	./rebar compile

docs:
	@mkdir -p doc/api
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}, {dir, "./doc/api"}]'

test: all
	@$(ERLC) -o t/ t/etap.erl
	prove t/*.t

cover: all
	COVER=1 prove t/*.t
	@$(ERL) -detached -noshell -eval 'etap_report:create()' -s init stop

clean: 
	./rebar clean
	@rm -f t/*.beam
	@rm -rf docs/api
