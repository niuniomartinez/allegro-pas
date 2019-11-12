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
PROJECT = Allegro.pas 5.2.b

# ===============================================

# --------------------------------------
# -- Platform dependent configuration --
# --------------------------------------

# ------------------
# Win32
# ------------------
ifeq ($(TARGET),WIN)
	# Platform name.
	PLATFORM=Windows
	# Binary sufix.
	BINSUF = .exe
	# Extra flags.
	EFLAGS = -WG

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
OPTOPT = -O3 -Xs -XX

# Next can be used to optimize for almost any current 32bit PC with Linux or
# Windows, doing it pretty well.  Of course, it will prevent your executable to
# run in anything older than PentiumIII.
# OPTOPT += -CpPENTIUM3

# Next one can be used to optimize for 64bit PC with Linux or Windows.
# OPTOPT += -CpATHLON64



# ---------------------
# -- Debug specifics --
# ---------------------

# Debugging opetions.
# Not only adds GDB information to the executable, but also tells the compiler
# to show ALL warnings and hints.
DBGOPT = -O- -gl -vh -vw

# Liks with "debug" Allegro.  Usefull when creating an add-on or Allegro.pas
# itself.
# DBGOPT += -dDEBUGMODE



# --------------------------
# -- No platform specific --
# --------------------------

# Sufix for main unit.  See "makefile.list" and "makefile.all".
MAINSUF = .pas
LIBSUF  = .pas

# Directories
SRCDIR = src/
LIBDIR = lib/
EXMDIR = examples/
DEMDIR = demos/
OBJDIR = obj/
BINDIR = bin/

FPDIR = furiouspaladin/
PADIR = pascaleroids/

DOCSRC = $(SRCDIR)docs/
LIBSRC = $(SRCDIR)$(LIBDIR)
EXMSRC = $(SRCDIR)$(EXMDIR)
DEMSRC = $(SRCDIR)$(DEMDIR)
FPSRC = $(DEMSRC)$(FPDIR)
PASRC = $(DEMSRC)$(PADIR)

DOCDIR = docs/lib/
EXMBIN = $(BINDIR)$(EXMDIR)
DEMBIN = $(BINDIR)$(DEMDIR)
FPBIN = $(DEMBIN)$(FPDIR)
PABIN = $(DEMBIN)$(PADIR)



#Pascal flags
PFLAGS = -Sh -Si

# Optimized compilation
#FLAGS = $(OPTOPT) $(PFLAGS) $(EFLAGS)
# Use next line instead to activate debug.
FLAGS = $(DBGOPT) $(PFLAGS) $(EFLAGS)

# If you're using Windows, the examples will link with the "monolith" version
# of Allegro by default.  If you want to link with the "non-monolith" version
# then use next line (read "docs/build/windows.txt" for more information).
# FLAGS += -dNO_MONOLITH


# ---------------------------
# -- Documentation options --
# ---------------------------

DOCFMT = -O html -L en
DOCOPTS = --staronly --auto-abstract --include-creation-time --use-tipue-search



# -- Source files list --
include makefile.list

# -- Build rules  --
include makefile.all

