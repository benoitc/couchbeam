ERL          ?= erl
ERLC		     ?= erlc
APP          := couchbeam

.PHONY: rel deps

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

rel: deps
	@./rebar compile generate

relforce: deps
	@./rebar compile generate force=1

doc:
	@mkdir -p doc/api
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}, {dir, "./doc/api"}]'
	

test: all
	@$(ERLC) -o t/ t/etap.erl
	@$(ERLC) -o t/ t/test_util.erl
	prove t/*.t

cover: all
	COVER=1 prove t/*.t
	@$(ERL) -detached -noshell -eval 'etap_report:create()' -s init stop

clean: 
	@./rebar clean
	@rm -f t/*.beam
	@rm -rf doc

relclean:
	rm -rf rel/couchbeam


distclean: clean relclean
	@./rebar delete-deps

