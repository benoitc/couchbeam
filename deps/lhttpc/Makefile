APPLICATION := lhttpc
APP_FILE := ebin/$(APPLICATION).app
SOURCES := $(wildcard src/*.erl)
HEADERS := $(wildcard src/*.hrl)
MODULES := $(patsubst src/%.erl,%,$(SOURCES))
BEAMS := $(patsubst %,ebin/%.beam,$(MODULES))

comma := ,
e :=
space := $(e) $(e)
MODULELIST := $(subst $(space),$(comma),$(MODULES))

TEST_SOURCES := $(wildcard test/*.erl)
TEST_BEAMS := $(patsubst %.erl,%.beam, $(TEST_SOURCES))

include vsn.mk

.PHONY: all clean dialyzer

all: $(APPLICATION) doc

$(APPLICATION): $(BEAMS) $(APP_FILE)

test: $(APPLICATION) $(TEST_BEAMS) util/run_test.beam
	@echo Running tests
	@erl -pa util/ -pa ebin/ -pa test/ -noinput -s run_test run

test_shell: $(APPLICATION) $(TEST_BEAMS)
	@echo Starting a shell with test paths included
	@erl -pa ebin/ -pa test/

test/%.beam: test/%.erl
	@echo Compiling $<
	@erlc +debug_info -o test/ $<

$(APP_FILE): src/$(APPLICATION).app.src
	@echo Generating $@
	@sed -e 's/@MODULES@/$(MODULELIST)/' -e 's/@VSN@/$(VSN)/' $< > $@

ebin/%.beam: src/%.erl $(HEADERS) $(filter-out $(wildcard ebin), ebin)
	@echo Compiling $<
	@erlc +debug_info +warn_missing_spec -o ebin/ $<

ebin:
	@echo Creating ebin/
	@mkdir ebin/

doc: doc/edoc-info

dialyzer:
	@echo Running dialyzer on sources
	@dialyzer --src -r src/

doc/edoc-info: doc/overview.edoc $(SOURCES) 
	@erlc -o util/ util/make_doc.erl
	@echo Generating documentation from edoc
	@erl -pa util/ -noinput -s make_doc edoc

util/%.beam: util/%.erl
	@erlc -o util/ util/run_test.erl

clean:
	@echo Cleaning
	@rm -f ebin/*.{beam,app} test/*.beam doc/*.{html,css,png} doc/edoc-info
	@rm -r cover_report
	@rm -f util/*.beam

release: clean all test dialyzer
	@util/releaser $(APPLICATION) $(VSN)
