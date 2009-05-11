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

	# File management
	# TODO: Detect MSys, Cywing and such...
	DELETE = del
	COPY   = copy
endif

# ------------------
# Linux
# ------------------
# Actually unsupported, but it will :)
ifeq ($(TARGET),LINUX)
	# Platform name
	PLATFORM=GNU/Linux
	# Binary sufix
	BINSUF = 
	# Object sufix
	OBJSUF = .o

	# File management
	DELETE = rm -rf
	COPY   = cp
endif



# --------------------------
# -- No platform specific --
# --------------------------

OBJDIR = obj/
TOOLDIR = tools/
DEMODIR = demo/
EXMDIR = examples/
LIBDIR = lib/

#FLAGS = -g -O- -pg -Sh
FLAGS = -02 -Sh

# -- Source files list --
include makefile.list

# -- Build rules  --
include makefile.all

