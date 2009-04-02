## Makefile to create Allegro.pas documentation.

# List of units.
SOURCES = $(wildcard ../lib/al*.pas)
ifeq ($(strip $(SOURCES)),)
  $(error ERROR: No source code found at ../lib/)
endif

all:
	pasdoc $(SOURCES)
