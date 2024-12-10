.PHONY: all types lint format

all: format lint types

types:
	@echo running test
	@nvim -l test/types.lua --skip-tests=true --level=Information

lint:
	@echo running lint
	@luacheck .

format:
	@echo running format
	@stylua --glob '**/*.lua' .
