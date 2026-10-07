.DEFAULT_GOAL := all
.DELETE_ON_ERROR:

PROLOG_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
REPO_ROOT := $(abspath $(PROLOG_DIR)/..)
STACK ?= stack
SWIPL ?= swipl
STACK_CMD = $(STACK) --stack-root "$(REPO_ROOT)/.stack-root"
SOEXT ?= so
EXTENSION := build/dnssec_native.$(SOEXT)

# These are supplied by the outer build after Stack has updated the library.
# Inspecting them afterwards avoids using a stale package registration/header.
HASKELL_LIBRARY := $(wildcard $(HASKELL_LIB_DIR)/lib$(HASKELL_LIB_NAME)-ghc*.so $(HASKELL_LIB_DIR)/lib$(HASKELL_LIB_NAME)-ghc*.dylib)

.PHONY: all build native test clean
all: build

# Stack owns Haskell dependency tracking. Recursive Make then sees the current
# generated header and shared library and rebuilds only affected native targets.
build:
	@cd "$(REPO_ROOT)" && $(STACK_CMD) build dns-regressor:lib
	@set -eu; cd "$(REPO_ROOT)"; \
	  dist_dir=$$($(STACK_CMD) path --dist-dir); \
	  case "$$dist_dir" in /*) ;; *) dist_dir="$(REPO_ROOT)/$$dist_dir" ;; esac; \
	  lib_dir=$$($(STACK_CMD) exec -- ghc-pkg field dns-regressor dynamic-library-dirs --simple-output); \
	  lib_name=$$($(STACK_CMD) exec -- ghc-pkg field dns-regressor hs-libraries --simple-output); \
	  extension=$$($(SWIPL) -q -g 'current_prolog_flag(shared_object_extension, E), write(E), halt'); \
	  $(MAKE) --no-print-directory -C "$(PROLOG_DIR)" native \
	    BRIDGE_HEADER="$$dist_dir/build/Prolog/Bridge_stub.h" \
	    HASKELL_LIB_DIR="$$lib_dir" HASKELL_LIB_NAME="$$lib_name" SOEXT="$$extension"

# Internal target: build supplies the generated dependencies before invoking it.
native: $(EXTENSION)

build/dnssec_bridge.o: dnssec_bridge.c $(BRIDGE_HEADER) Makefile | build/
	@set -eu; eval "$$($(SWIPL) --dump-runtime-variables)"; \
	  cd "$(REPO_ROOT)"; \
	  $(STACK_CMD) exec -- ghc -c -dynamic -fPIC \
	    -I"$$PLBASE/include" -I"$(dir $(BRIDGE_HEADER))" \
	    "$(PROLOG_DIR)/dnssec_bridge.c" -o "$(PROLOG_DIR)/$@"

$(EXTENSION): build/dnssec_bridge.o $(HASKELL_LIBRARY) Makefile
	@set -eu; test -n "$(HASKELL_LIBRARY)" || { echo 'Missing Haskell shared library; use make build.' >&2; exit 1; }; \
	  eval "$$($(SWIPL) --dump-runtime-variables)"; \
	  cd "$(REPO_ROOT)"; \
	  $(STACK_CMD) exec -- ghc \
	    -dynamic -shared -fPIC -flink-rts -no-hs-main -threaded \
	    -package dns-regressor -L"$$PLLIBDIR" -lswipl \
	    -optl-Wl,-rpath,"$$PLLIBDIR" \
	    "$(PROLOG_DIR)/build/dnssec_bridge.o" -o "$(PROLOG_DIR)/$@"

build/:
	mkdir -p build

# Run only after building; Prolog must load the newly linked extension.
test: build
	$(SWIPL) -q -g run_tests -t halt \
	  "$(PROLOG_DIR)/test/dnssec_tests.pl" "$(PROLOG_DIR)/test/headers_tests.pl"

clean:
	rm -rf "$(PROLOG_DIR)/build"
