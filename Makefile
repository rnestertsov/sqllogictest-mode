# ============================================================================ #
# SHELL CONFIGURATION
# ============================================================================ #

SHELL := /bin/bash

# ============================================================================ #
# HELP
# ============================================================================ #

## help: Show this help message
.PHONY: help
help:
	@echo 'sqllogictest-mode - Available Commands:'
	@echo ''
	@sed -n 's/^##//p' ${MAKEFILE_LIST} | column -t -s ':' | sed -e 's/^/  /'
	@echo ''
	@echo 'Examples:'
	@echo '  make test    # Run ERT tests'
	@echo ''

# ============================================================================ #
# DEVELOPMENT
# ============================================================================ #

## test: Run ERT tests in batch mode
.PHONY: test
test:
	@emacs --batch -l ert \
		-l sqllogictest-mode.el \
		-l sqllogictest-mode-test.el \
		-f ert-run-tests-batch-and-exit
