# Makefile for Kapok
# -*- mode: Makefile -*-


MKDIR    := mkdir -p
RM       := rm -rf
CP       := cp -rf
REBAR    := rebar
ERL      := erl
ERLC     := erlc
ESCRIPT  := escript
KAPOKC   := kapokc
KDT      := kdt


QUIET    := @
# add local bin into PATH
export PATH := $(CURDIR)/bin:$(PATH)

lib_dir                := $(CURDIR)/lib
# sort the libs according to their names to preserve compilation orders
lib_names              := $(sort $(patsubst $(lib_dir)/%,%,$(wildcard $(lib_dir)/*)))
other_files            := $(CURDIR)/erl_crash.dump

# strip the CURDIR prefix and get the related path for output absolute path
# $(call related-path,absolute-path)
define related-path
  $(patsubst $(CURDIR)/%,%,$1)
endef

# get files with specified suffix in specified directory
# $(call get-files-in-dir,dir-name,suffix)
define get-files-in-dir
  $(patsubst %.$2,%,$(notdir $(wildcard $1/*.$2)))
endef

# filter out the specified word list from another word list
# $(call filter-out-list,to-remove-list,word-list)
define filter-out-list
  $(strip                       \
    $(foreach w,$2,             \
      $(if $(findstring $w,$1)  \
           ,,                   \
         $w)))
endef

# calculate beam file from module
# $(call modules-to-beams,destination,modules...)
define modules-to-beams
  $(addprefix $1/,$(addsuffix .beam,$2))
endef

# ensure directory's existence
# $(call ensure-dir,path)
define ensure-dir
  $(QUIET) [ -d "$1" ] || $(MKDIR) $1
endef

# check the existence of command
# $(call check-cmd,cmd-name)
define check-cmd
  $(QUIET) if ! which "$1" > /dev/null; \
then echo "command" "$1" "not found"; exit 1; fi
endef

# generate path options for erlang compiler
# $(call get-path-options,project-root-dir)
define get-path-options
  $(shell find $1/lib -type d -name ebin | sed -e 's#\(.*\)# -pa \1 #' | tr -d '\n')
endef

ERL_PATH_OPTIONS := $(call get-path-options,$(CURDIR))
ERL_OPTIONS      := -noshell $(ERL_PATH_OPTIONS)

# call erl command line
# $(call erl,file,function,arguments...)
define erl
  $(QUIET) $(ERL) $(ERL_OPTIONS) -s $1 $2 $3 -s init stop
endef

KAPOKC_OPTIONS    := $(ERL_PATH_OPTIONS)
KAPOK_OPTIONS     :=

# call kapokc command line
# $(call kapokc,file,outdir)
define kapokc
  $(QUIET) $(KAPOKC) $(KAPOKC_OPTIONS) -o "$2" "$1"
endef

# call kdt command line to test a project
define kdt-test
  $(QUIET) $(KDT) test $1
endef

# generate the targets with specified prefix for specified modules
# $(call gen-target,prefix,module...)
define gen-target
  $(addprefix $1,$2)
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

lib_$1_dir             := $(lib_dir)/$1
$1_src_dir             := $$(lib_$1_dir)/src
$1_lib_dir             := $$(lib_$1_dir)/lib
$1_test_dir            := $$(lib_$1_dir)/test
$1_beam_output_dir     := $$(lib_$1_dir)/ebin

$1_modules             := $$(call get-files-in-dir,$$($1_src_dir),erl)
$1_parser_src_file     := $$($1_src_dir)/kapok_parser.erl
$1_core_lib_files      :=       \
  kapok.core.kpk                \
  kapok.module.kpk              \
  kapok.code-server.kpk         \
  kapok.protocol.kpk
$1_core_lib_modules    := $$(patsubst %.kpk,%,$$($1_core_lib_files))
$1_lib_files           :=       \
  kapok.exception.kpk           \
  kapok.char.kpk                \
  kapok.atom.kpk                \
  kapok.integer.kpk             \
  kapok.tuple.kpk               \
  kapok.list.kpk                \
  kapok.alist.kpk               \
  kapok.string.kpk              \
  kapok.access.kpk              \
  kapok.inspect.algebra.kpk     \
  kapok.inspect.kpk             \
  kapok.time.kpk

$1_lib_modules         := $$(patsubst %.kpk,%,$$($1_lib_files))
$1_beam_files          := $$(call modules-to-beams,$$($1_beam_output_dir),$$($1_modules))
$1_core_lib_beam_files := $$(call modules-to-beams,$$($1_beam_output_dir),$$($1_core_lib_modules))
$1_lib_beam_files      := $$(call modules-to-beams,$$($1_beam_output_dir),$$($1_lib_modules))

else

lib_$1_dir             := $(lib_dir)/$1
$1_lib_dir             := $$(lib_$1_dir)/lib
$1_beam_output_dir     := $$(lib_$1_dir)/ebin

$1_lib_files           := $$(call get-files-in-dir,$$($1_lib_dir),kpk)
$1_lib_beam_files      := $$(call modules-to-beams,$$($1_beam_output_dir),$$($1_lib_files))

endif

endef


# define the build rules for specified lib
# $(call gen-build-rules,lib,build-prefix,test-prefix,clean-prefix)
define gen-build-rules

ifeq "$(strip $1)" "kapok"

.PHONY : $2$1 $2$1-compiler $2$1-core-libs $2$1-libs $3$1 $4$1

$2$1: $2$1-compiler $2$1-core-libs $2$1-libs

$2$1-compiler: $($1_parser_src_file) $($1_beam_files)

$($1_parser_src_file): $($1_src_dir)/%.erl:  $($1_src_dir)/%.yrl

$($1_parser_src_file):
	$(call echo-build,lib,$1)
	$(QUIET) echo "--- generate parser and build source files ---"
	$(QUIET) cd $(lib_$1_dir) && $(REBAR) compile

# TODO add $($1_parser_src_file) as an dependency
$($1_beam_files): $($1_beam_output_dir)/%.beam: $($1_src_dir)/%.erl

$($1_beam_files):
	$(call echo-build,lib,$1)
	$(QUIET) echo "--- build source file ---"
	$(QUIET) cd $(lib_$1_dir) && $(REBAR) compile

$2$1-core-libs: $($1_core_lib_beam_files)

$($1_core_lib_beam_files): $($1_beam_files)
$($1_core_lib_beam_files): $($1_beam_output_dir)/%.beam: $($1_lib_dir)/%.kpk

$($1_core_lib_beam_files):
	$(QUIET) echo "--- build core libs ---"
	$(call erl,kapok_compiler,core)

$2$1-libs: $($1_lib_beam_files)
$($1_lib_beam_files): $($1_beam_files) $($1_core_lib_beam_files)

$($1_lib_beam_files): $($1_beam_output_dir)/%.beam: $($1_lib_dir)/%.kpk
	$(QUIET) printf "Compile '%s'\n" $$(call related-path,$$<)
	$$(call kapokc,$$<,$$(dir $$@))

$3$1: $2$1
	$$(call kdt-test,$$(lib_$1_dir))

$4$1:
	$(QUIET) $(RM) $($1_parser_src_file) $($1_beam_files) \
  $($1_core_lib_beam_files) $($1_lib_beam_files) $($1_beam_output_dir)/*.beam

else

.PHONY : $2$1 $2$1-libs $3$1 $4$1

$2$1: $2$1-libs

$2$1-libs: $($1_lib_beam_files)

$($1_lib_beam_files): $($1_beam_output_dir)/%.beam: $($1_lib_dir)/%.kpk
	$(QUIET) printf "Compile '%s'\n" $$(call related-path,$$<)
	$$(call kapokc,$$<,$$(dir $$@))

$3$1:
	$$(call kdt-test,$$(lib_$1_dir))

$4$1:
	$(QUIET) $(RM) $($1_lib_beam_files)

endif

endef

define gen-build-for
$(eval $(call gen-build-vars,$1))
$(eval $(call gen-build-rules,$1,$2,$3,$4))
endef


.PHONY : all build test clean

all: build

build: $(foreach l,$(lib_names),$(call gen-target,build-,$l))
test:  build $(foreach l,$(lib_names),$(call gen-target,test-,$l))
clean: $(foreach l,$(lib_names),$(call gen-target,clean-,$l))
	$(QUIET) $(RM) $(other_files)

# add eval call to expand multiple line definitions of variables and rules
$(eval $(foreach l,$(lib_names),$(call gen-build-for,$l,build-,test-,clean-)))
