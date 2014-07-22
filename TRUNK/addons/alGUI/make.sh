#!/bin/sh
## Compiles the program.
## Still in development!!

# Main pascal file name.
FileName='test.pas'

# Paths to working directories.
PATHS='-Fulib/ -Fu../../lib -I../../lib/ -FEbin/ -FUobj/'

# Generate debug and profile information.
DFLAGS='-g -gp' # Debug on
PFLAGS='-pg'    # Profile on

# Generate optimized code.
OFLAGS='-O2'

# Languaje and syntax.
LFLAGS='-Mobjfpc -Sh -Si'

# Flags.
FLAGS="$DFLAGS $LFLAGS"
#FLAGS="$PFLAGS $LFLAGS"
#FLAGS="$OFLAGS $LFLAGS"

# Compile.
echo
echo 'Compiling...'
echo
fpc $FLAGS $PATHS $FileName
echo
echo "Binary compiled!"
echo

# Exits status is the last command (fpc or echo? )
exit $?
