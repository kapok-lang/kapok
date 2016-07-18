# Makefile for Kapok
# -*- mode: Makefile -*-


MKDIR    := mkdir -p
RM       := rm -rf
CP       := cp -rf
REBAR    := rebar
ERL      := erl
ERLC     := erlc
ESCRIPT  := escript


QUIET    := @

lib_dir                := $(CURDIR)/lib
lib_names              := $(patsubst $(lib_dir)/%,%,$(wildcard $(lib_dir)/*))

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

ERL_PATH_OPTIONS := -pa $(CURDIR)/*/*/ebin
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

# generate the targets with specified prefix for specified modules
# $(call gen-target,prefix,module...)
define gen-target
  $(addprefix $1,$2)
endef

# define the rules with specified prefix for the specified module
# $(call gen-run-test-rule,prefix,module)
define gen-run-test-rule
$1$2:
	$(QUIET) printf '--- Run test %s ---\n' $2
	$(call eunit-test,$2)
endef

# echo the message that about to build the specified type package
# $(call echo-build,type,module)
define echo-build
$(QUIET) printf "=== build %s %s ===\n" $1 $2
endef

# define the build vars for specified lib
# $(call gen-build-vars,lib)
define gen-build-vars

ifeq "$(strip $1)" "kapok"

$1_lib_dir             := $(lib_dir)/$1
$1_deps_dir            := $$($1_lib_dir)/deps
$1_src_dir             := $$($1_lib_dir)/src
$1_test_dir            := $$($1_lib_dir)/test
$1_beam_output_dir     := $$($1_lib_dir)/ebin
$1_test_output_dir     := $$($1_beam_output_dir)

$1_modules             := $$(call get-modules-in-dir,$$($1_src_dir))
$1_beam_files          := $$(call modules-to-beams,$$($1_beam_output_dir),$$($1_modules))
$1_test_modules        := $$(call get-modules-in-dir,$$($1_test_dir))
$1_test_beam_files     := $$(call modules-to-beams,$$($1_test_output_dir),$$($1_test_modules))
$1_parser_src_file     := $$($1_src_dir)/kapok_parser.erl
$1_other_files         := $$($1_lib_dir)/erl_crash.dump

else

$$(error "currently we don't define any vars for lib: $1")

endif

endef


# define the build rules for specified lib
# $(call gen-build-rules,lib,build-prefix,test-prefix,clean-prefix)
define gen-build-rules

ifeq "$(strip $1)" "kapok"

$2$1: $($1_parser_src_file) $($1_beam_files)

$($1_parser_src_file): $($1_src_dir)/%.erl:  $($1_src_dir)/%.yrl

$($1_parser_src_file):
	$(call echo-build,lib,$1)
	$(QUIET) echo "--- generate parser and build source files ---"
	$(QUIET) cd $($1_lib_dir) && $(REBAR) compile

$($1_beam_files): $($1_beam_output_dir)/%.beam: $($1_src_dir)/%.erl

$($1_beam_files):
	$(call echo-build,lib,$1)
	$(QUIET) echo "--- build source file ---"
	$(QUIET) cd $($1_lib_dir) && $(REBAR) compile

$3$1: build-test-$1 run-test-$1

build-test-$1: $($1_test_beam_files)

$($1_test_beam_files): $($1_beam_files)
$($1_test_output_dir)/%.beam: $($1_test_dir)/%.erl
	$$(call ensure-dir,$$(dir $$@))
	$$(QUIET) printf "Compile '%s' to beam\n" $$<
	$$(call erlc,$$<,$$(dir $$@))

run-test-$1: $($1_test_beam_files)
run-test-$1: $(call gen-target,run-test-,$($1_test_modules))
$(foreach m,$($1_test_modules), \
   $(eval $(call gen-run-test-rule,run-test-,$m)))

$4$1:
	$(RM) $($1_parser_src_file) $($1_beam_files) $($1_test_beam_files) $($1_other_files)

else

$$(error "currently we don't define any rule for lib: $1")

endif

endef

define gen-build-for
$(eval $(call gen-build-vars,$1))
$(eval $(call gen-build-rules,$1,$2,$3,$4))
endef


.PHONY : all build test clean

all: build

build: $(foreach l,$(lib_names),$(call gen-target,build-,$l))
test:  $(foreach l,$(lib_names),$(call gen-target,test-,$l))
clean: $(foreach l,$(lib_names),$(call gen-target,clean-,$l))

$(foreach l,$(lib_names),$(call gen-build-for,$l,build-,test-,clean-))

