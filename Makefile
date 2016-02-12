# -*- mode: Makefile -*-


MKDIR    := mkdir -p
RM       := rm -rf
CP       := cp -rf
REBAR    := rebar
ERL      := erl
ERLC     := erlc
ESCRIPT  := escript


QUIET    := @

deps_dir               := $(CURDIR)/deps
src_dir                := $(CURDIR)/src
test_dir               := $(CURDIR)/test
beam_output_dir        := $(CURDIR)/ebin
test_output_dir        := $(beam_output_dir)



# get erlang modules in specified directory
# $(call get-modules-in-dir,dir-name)
define get-modules-in-dir
  $(patsubst %.erl,%,$(notdir $(wildcard $1/*.erl)))
endef

# calculate beam file from module
# $(call modules-to-beams,destination,modules...)
define modules-to-beams
  $(addprefix $1/,$(addsuffix .beam,$2))
endef

modules           := $(call get-modules-in-dir,$(src_dir))
beam_files        := $(call modules-to-beams,$(beam_output_dir),$(modules))
test_modules      := $(call get-modules-in-dir,$(test_dir))
test_beam_files   := $(call modules-to-beams,$(test_output_dir),$(test_modules))
parser_src_file   := $(src_dir)/ceiba_parser.erl

# ensure directory's existence
# $(call ensure-dir,dir-name)
define ensure-dir
  $(QUIET) [ -d "$1" ] || $(MKDIR) $1
endef

# check the existence of command
# $(call check-cmd,cmd-name)
define check-cmd
  $(QUIET) if ! which "$1" > /dev/null; \
then echo "command" "$1" "not found"; exit 1; fi
endef

# generate erl code path options
# Notice: no leading space before macro body in order Embedded into command line
# $(call get-code-path-options,pattern)
define get-code-path-options
$(addprefix -pz ,$(wildcard $1))
endef


ERL_PATH_OPTIONS := \
  -pa $(beam_output_dir) $(call get-code-path-options,$(test_output_dir))
ERL_OPTIONS      := -noshell $(ERL_PATH_OPTIONS)

# call erl command line
# $(call erl,file,function,arguments...)
define erl
  $(QUIET) $(ERL) $(ERL_OPTIONS) -s $1 $2 $3 -s init stop
endef

ERLC_OPTIONS      := $(ERL_PATH_OPTIONS)

# call erlc command line
# $(call erlc,file,outdir)
define erlc
  $(QUIET) $(ERLC) $(ERLC_OPTIONS) -o "$2" "$1"
endef

# call eunit test on specified module, using the auto-exported `test` function
# $(call eunit-test,module)
define eunit-test
  $(QUIET) $(ERL) $(ERL_OPTIONS) -s $1 test -s init stop
endef

# generate the `run-test-*` targets for specified modules
# $(call gen-run-test-target,module...)
define gen-run-test-target
  $(addprefix run-test-,$1)
endef

# define the `run-test-*` rules for specified modules
# $(call gen-run-test-rule,module)
define gen-run-test-rule
run-test-$1:
	$(QUIET) printf "Run test %s\n" $1
	$(call eunit-test,$1)
endef

.PHONY : all build test run-test

all: build

build: $(parser_src_file) $(beam_files)

$(parser_src_file): $(src_dir)/%.erl:  $(src_dir)/%.yrl

$(parser_src_file):
	$(QUIET) echo "--- generate parser and build source files ---"
	$(REBAR) compile

$(beam_files): $(beam_output_dir)/%.beam: $(src_dir)/%.erl

$(beam_files):
	$(QUIET) echo "--- build source files ---"
	$(REBAR) compile

test: $(beam_files) $(test_beam_files) run-test

$(test_beam_files): $(beam_files)
$(test_output_dir)/%.beam: $(test_dir)/%.erl
	$(call ensure-dir,$(dir $@))
	$(QUIET) printf "Compile 'test/%s' to beam\n" $(notdir $<)
	$(call erlc,$<,$(dir $@))

run-test: $(test_beam_files)
run-test: $(call gen-run-test-target,$(test_modules))
$(foreach m,$(test_modules), \
  $(eval $(call gen-run-test-rule,$m)))

clean:
	$(RM) $(parser_src_file) $(beam_files) $(test_beam_files)

