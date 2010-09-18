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

#
# Dynamic library build.


# Binary file name without extension
LIBRARY = alpas42

# Sources directory
SRCDIR = src

# Headers directory
INCDIR = $(SRCDIR)

# Source extension.
SRCSUF = .c

# Library directory.
LIBDIR = lib

# Source file for a test program (test if Allegro is installed).
# (Should be in the root directory) :(
# It'll use the SRCSUF sufix.
TESTFILE = test
# ===============================================


# --------------------------------------
# -- Platform dependent configuration --
# --------------------------------------

# ------------------
# DJGPP/DOS
# ------------------
#  Actually unsuported, but who knows ;-)
ifeq ($(TARGET),DJGPP)
	# Platform name
	PLATFORM=DOS/DJGPP
	# Compiller for C code
	GCC = gcc
	# Binary sufix
	BINSUF = .exe
	LIBSUF = .a
	# Object sufix
	OBJSUF = .o
	# Link options (libraries, etc)
	LFLAGS = -g
	LIBS = -lalleg
	#LFLAGS = -s
	# Compiler options
	CFLAGS = -I$(INCDIR) -Wall -g
	#CFLAGS = -I$(INCDIR) -Wall -O3
	# Free Pascal options
	FPCFLAGS = -O2

	# File management
	DELETE = del
	COPY   = xcopy
endif

# ------------------
# MinGW32/Win32
# ------------------
ifeq ($(TARGET),MINGW32)
	# Platform name
	PLATFORM=Win32/MinGW32
	# Compiler for C code
	GCC = gcc
       	LIBCC = gcc -shared
	# Binary sufix
	BINSUF = .exe
	LIBSUF = .dll
	# Object sufix
	OBJSUF = .o
	# Link options (libraries, etc)
	LDFLAGS = -Wl,--out-implib,$(LIBDIR)/libalpas.a
	LIBS = -lalleg
	# Compiler options
	CFLAGS = -I$(INCDIR) -Wall -O3
	# Free Pascal options
	FPCFLAGS = -dMSWINDOWS -O2 -WG

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
	# Compiler for C code
	GCC = gcc
	# Binary sufix
	BINSUF = .bin
	LIBSUF = .so
	# Object sufix
	OBJSUF = .o
	# Link options (libraries, etc)
	LFLAGS = -s -g
	#LFLAGS = -s 
	# Compiler options
	CFLAGS = -I$(INCDIR) -Wall -g3
	# Free Pascal options
	FPCFLAGS = -O2

	# File management
	DELETE = rm -rf
	COPY   = cp
endif



# --------------------------
# -- No platform specific --
# --------------------------

OBJDIR = obj
TOOLDIR = tools
DEMODIR = demo
EXMDIR = examples

# -- Source files list --
include makefile.list

# -- Build rules  --
include makefile.all

