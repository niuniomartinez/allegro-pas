## Makefile to create Allegro.pas documentation.

# List of units.
SOURCES = $(wildcard ../lib/al*.pas)
ifeq ($(strip $(SOURCES)),)
  $(error ERROR: No source code found at ../lib/)
endif

.PHONY: all

all: HTML

HTML:
	pasdoc -T Allegro.pas -O html --cache-dir cache -E html --auto-abstract --introduction intro.txt --conclusion progs.txt $(SOURCES)
