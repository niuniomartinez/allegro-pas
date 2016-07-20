# ------------------------------------------------------------------------
# Makefile
# ------------------------------------------------------------------------
# Based in the Almake project as used in KOF91 V1.49.
# Visit http://almake.sf.net/ for almake information.
# Visit http://kof91.sf.net/  for KOF91 information.

# This file defines the tarjet platform, and is modified by fix.bat or
# fix.sh script. See it's sources.
include target.os

# Suggested by "GNU Coding Stardards"
SHELL = /bin/sh

# ===============================================
# Project name
PROJECT = Allegro.pas 4.4

# ===============================================

# --------------------------------------
# -- Platform dependent configuration --
# --------------------------------------

# ------------------
# Win32
# ------------------
ifeq ($(TARGET),WIN)
	# Platform name
	PLATFORM=Windows
	# Binary sufix
	BINSUF = .exe
	LIBSUF = .dll
	# Extra flags.
	EFLAGS = -WG

	# File management
	# TODO: Detect MSys, Cywing and such...
	DELETE = del
	COPY   = copy
endif

# ------------------
# Linux
# ------------------
ifeq ($(TARGET),LINUX)
	# Platform name
	PLATFORM=GNU/Linux
	# Binary sufix
	BINSUF = 
	# Extra flags.
	EFLAGS = 

	# File management
	DELETE = rm -rf
	COPY   = cp
endif



# ----------------------------
# -- Optimization specifics --
# ----------------------------

# Optimization options, including "smart linking".
OPTOPT = -O3 -Xs -XX

# Next can be used to optimize for almost any current 32bit PC with Linux or
# Windows, doing it pretty well.  Of course, it will prevent your executable to
# run in anything older than Pentium.
# OPTOPT += -CpPENTIUM -CfSSE

# Next one can be used to optimize for 64bit PC with Linux or Windows.
# OPTOPT += -CpATHLON64



# ---------------------
# -- Debug specifics --
# ---------------------

# Debugging opetions.
# Not only adds GDB information to the executable, but also tells the compiler
# to show ALL warnings and hints.
DBGOPT = -O- -gl -vh -vw



# --------------------------
# -- No platform specific --
# --------------------------

OBJDIR = obj/
EXMDIR = examples/
LIBDIR = lib/
BINDIR = bin/
DOCDIR = docs/

EXMBIN = $(BINDIR)$(EXMDIR)

ALEXDIR = demo/
ALEXBIN = $(BINDIR)demo/

PFLAGS = -Sh -Si

# Optimized compilation
FLAGS = $(OPTOPT) $(PFLAGS) $(EFLAGS)
# Use next line instead to activate debug.
#FLAGS = $(DBGOPT) $(PFLAGS) $(EFLAGS)

# -- Source files list --
include makefile.list

# -- Build rules  --
include makefile.all

