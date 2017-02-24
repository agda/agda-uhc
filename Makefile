
.PHONY: install test

install :
	cabal install --enable-tests

PARALLEL_TESTS = $(shell getconf _NPROCESSORS_ONLN)
TEST_OPTIONS ?=-i -j$(PARALLEL_TESTS)

test :
	AGDA_BIN=dist/build/agda-uhc/agda-uhc dist/build/agda-uhc-tests/agda-uhc-tests $(TEST_OPTIONS)
