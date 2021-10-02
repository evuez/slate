SOURCES := $(wildcard */*.hs) $(wildcard */*/*.hs)

.PHONY: build
build:
	stack build

.PHONY: build.watch
build.watch:
	ghcid --command "stack ghci --ghci-options=-fobject-code" --allow-eval --lint

.PHONY: install
install:
	stack install

.PHONY: format
format: $(SOURCES)

.PHONY: $(SOURCES)
$(SOURCES):
	hindent $@

.PHONY: test
test:
	test/commands

.PHONY: start
start:
	stack run

.PHONY: lint
lint:
	hlint .
