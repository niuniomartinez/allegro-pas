## Makefile to create Allegro.pas documentation.

# List of units.
SOURCES = $(wildcard ../lib/al*.pas)
ifeq ($(strip $(SOURCES)),)
  $(error ERROR: No source code found at ../lib/)
endif

.PHONY: all latex

all: HTML

HTML:
	pasdoc -T Allegro.pas -O html --cache-dir cache -E html --introduction intro.txt $(SOURCES)

latex:
	pasdoc -T Allegro.pas -O latex --cache-dir cache -E latex --introduction intro.txt $(SOURCES)
