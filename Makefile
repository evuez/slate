SOURCES := $(wildcard */*.hs)

.PHONY: format
format: $(SOURCES)

.PHONY: $(SOURCES)
$(SOURCES):
	hindent $@
