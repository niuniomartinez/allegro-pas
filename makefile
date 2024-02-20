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
PROJECT = Allegro.pas 5.2.b.2

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

# do not show
# Note:  Call to subroutine "...;" marked as inline is not inlined
# (In FPC 3.3.1, not in FPC 3.1.1 rev 38027)
OPTOPT += -vm6058

# do not show Hint: (5024) Parameter "..." not used
OPTOPT += -vm5024


# ---------------------
# -- Debug specifics --
# ---------------------

# Debugging options.
DBGOPT = -O1 -g -gl
# Save debug information in an external file (.dbg).
DBGOPT += -Xg
# Show warnings + notes. Do not show hints (there are too many useless hints produced by FPC).
# (Suggestion from Castle Game Engine)
DBGOPT += -vwn
# Adds some code to check ranges and overflows.
DBGOPT += -Ct -Cr -CR -Co
# Adds code for valgrind
# DBGOPT += -gv
# Use heaptrace unit (for memory leak/corruption debugging).
DBGOPT += -gh

# Liks with "debug" Allegro.  Useful when creating an add-on or testing Allegro
# itself.
# DBGOPT += -dDEBUGMODE



# --------------------------
# -- No platform specific --
# --------------------------

# Sufix for main unit.  See "makefile.list" and "makefile.all".
MAINSUF = .pas
LIBSUF  = .pas

# Directories.
SRCDIR = src/
LIBDIR = lib/
EXMDIR = examples/
DEMDIR = demos/
OBJDIR = obj/
BINDIR = bin/

FPDIR = furiouspaladin/
PADIR = pascalroids/

LIBSRC = $(SRCDIR)$(LIBDIR)
EXMSRC = $(SRCDIR)$(EXMDIR)
DEMSRC = $(SRCDIR)$(DEMDIR)
FPSRC = $(DEMSRC)$(FPDIR)
PASRC = $(DEMSRC)$(PADIR)

EXMBIN = $(BINDIR)$(EXMDIR)
DEMBIN = $(BINDIR)$(DEMDIR)
FPBIN = $(DEMBIN)$(FPDIR)
PABIN = $(DEMBIN)$(PADIR)


# Pascal dialect: Delphi.
# This is to make it more easy to write code that works on both Delphi and FPC
# compilers.  Actually you can use Allegro.pas in any of the dialects
# supported by FPC.
CPFLAGS = -Mdelphi

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

DOCSRC = $(SRCDIR)docs/
DOCDIR = docs/lib/

DOCFMT = -O html -L en
DOCOPTS = --staronly --auto-abstract --include-creation-time --use-tipue-search



# -- Source files list --
include makefile.list

# -- Build rules  --
include makefile.all

