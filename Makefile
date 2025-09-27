.PHONY: all link build

all: link build

link:
	@nu -l -c 'dotty link --no-cache | ignore'

build:
	@cd oven && bun install && bun run verify
