# ------------------------------------------------------------------------
# Makefile
# ------------------------------------------------------------------------
# from "Makeproject" (http://www.burdjia.com/proyectos/makeproject/)

# This file defines the tarjet platform, and is modified by fix.bat,
# fix.cmd or fix.sh script. See it's sources.
include target.os

# Suggested by "GNU Coding Stardards"
SHELL = /bin/sh

# ===============================================
# Project name
PROJECT = Allegro.pas 5.2.b.2

# ===============================================

# --------------------------------------
# -- Platform dependent configuration --
# --------------------------------------

# ------------------
# Windows
# ------------------
ifeq ($(TARGET),WIN)
	# Platform name.
	PLATFORM=Windows
	# Binary sufix.
	BINSUF = .exe
	# Extra flags.
	# EFLAGS = -WG
	EFLAGS =

	# File management.
	# TODO: Detect MSys, Cywing and such...
	DELETE = del
	COPY   = copy
endif

# ------------------
# Linux
# ------------------
ifeq ($(TARGET),LINUX)
	# Platform name.
	PLATFORM=GNU/Linux
	# Binary sufix.
	BINSUF = 
	# Extra flags.
	EFLAGS = 

	# File management.
	DELETE = rm -rf
	COPY   = cp
endif



# ----------------------------
# -- Optimization specifics --
# ----------------------------

# Optimization options, including "smart linking".
OPTOPT = -O3 -CX -Xs -XX

# Enable inline.
OPTOPT += -Si

# Next can be used to optimize for almost any current 32bit PC with Linux or
# Windows, doing it pretty well.  Of course, it will prevent your executable to
# run in anything older than PentiumIII.
# OPTOPT += -CpPENTIUM3

# Next one can be used to optimize for 64bit PC with Linux or Windows.
# OPTOPT += -CpATHLON64

# Do not show Hint:
# Parameter "..." not used
# OPTOPT += -vm5024

# Do not show Note:
# Call to subroutine "...;" marked as inline is not inlined
# (In FPC 3.3.1, not in FPC 3.1.1 rev 38027)
OPTOPT += -vm6058



# ---------------------
# -- Debug specifics --
# ---------------------

# Debugging options.
DBGOPT = -O1 -g -gl
# Save debug information in an external file (.dbg).
DBGOPT += -Xg
# Show warnings + notes, but do not show hints.
# DBGOPT += -vwn
# Adds some code to check ranges and overflows.
DBGOPT += -Ci -Co -Cr -CR
# Adds code for valgrind
# DBGOPT += -gv
# Use heaptrace unit (for memory leak/corruption debugging).
DBGOPT += -gh
# Define symbol AL_DEBUGMODE to link with the "-debug" version of Allegro.
# Note you need to build the "-debug" version of Allegro.
# DBGOPT += -dAL_DEBUGMODE



# --------------------------
# -- No platform specific --
# --------------------------

# Pascal dialect.
# This is to make it more easy to write code that works on both Delphi and FPC
# compilers.  Actually you can use Allegro.pas in any of the dialects
# supported by FPC.
CPFLAGS = -Mdelphi
# Force use of AnsiStrings.
# CPFLAGS += -Sh

# Pascal flags.
PFLAGS = $(CPFLAGS)

# Optimized compilation.
FLAGS = $(OPTOPT) $(PFLAGS) $(EFLAGS)
# Use next line instead to activate debug.
#FLAGS = $(DBGOPT) $(PFLAGS) $(EFLAGS)

# If you're using Windows, the examples will link with the "monolith" version
# of Allegro by default.  If you want to link with the no-monolith version then
# use next line (read "docs/build/windows.txt" for more information).
# FLAGS += -dNO_MONOLITH



# -------------------
# -- Documentation --
# -------------------

DOCFMT = -O html -L en
DOCOPTS = --staronly --auto-abstract --include-creation-time --use-tipue-search



# -- Source files list --
include makefile.list

# -- Build rules  --
include makefile.all

