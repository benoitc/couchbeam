ERL          ?= erl
ERLC		     ?= erlc
APP          := couchbeam
REBAR?=./rebar

.PHONY: deps doc

all: deps compile

dev: devbuild

compile:
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

doc: dev
	$(REBAR) -C rebar_dev.config doc skip_deps=true

test: dev
	@$(ERLC) -o t/ t/etap.erl
	prove t/*.t

verbose-test: devbuild
	@$(ERLC) -o t/ t/etap.erl
	prove -v t/*.t

cover: dev
	COVER=1 prove t/*.t
	@$(ERL) -detached -noshell -eval 'etap_report:create()' -s init stop

clean:
	@$(REBAR) clean
	@rm -f t/*.beam
	@rm -f doc/*.html doc/*.css doc/edoc-info doc/*.png

distclean: clean
	@$(REBAR) delete-deps
	@rm -rf deps

dialyzer: compile
	@dialyzer -Wno_return -c ebin


# development
#
devclean:
	$(REBAR) -C rebar_dev.config clean

devbuild: devdeps
	$(REBAR) -C rebar_dev.config compile

devdeps:
	$(REBAR) -C rebar_dev.config get-deps
