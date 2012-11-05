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
PROJECT = Allegro.pas

# ===============================================


# --------------------------------------
# -- Platform dependent configuration --
# --------------------------------------

# ------------------
# Win32
# ------------------
ifeq ($(TARGET),WIN32)
	# Platform name
	PLATFORM=Windows
	# Binary sufix
	BINSUF = .exe
	LIBSUF = .dll
	# Object sufix
	OBJSUF = .o
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
	# Object sufix
	OBJSUF = .o
	# Extra flags.
	EFLAGS = 

	# File management
	DELETE = rm -rf
	COPY   = cp
endif



# --------------------------
# -- No platform specific --
# --------------------------

OBJDIR = obj/
DEMODIR = demo/
EXMDIR = examples/
LIBDIR = lib/

#FLAGS = -O3 -Sh -Si -Xs -XX $(EFLAGS)
# Use next line instead to activate debug.
FLAGS = -Co -CO -gl -O- -Sh -Si $(EFLAGS)

# -- Source files list --
include makefile.list

# -- Build rules  --
include makefile.all

