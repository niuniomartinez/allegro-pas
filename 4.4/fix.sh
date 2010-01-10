#!/bin/sh
#
# Sets makefile source code for the different platforms
# Based on fix.sh of Allegro.
# Modified By Kronoman - In loving memory of my father.
# Modified By Ñuño Martínez.


proc_help()
{
   echo "Usage: fix platform"
   echo
   echo "Where platform is one of: win32 or linux. "
   echo
   echo
}

proc_fix()
{
   echo "Configuring for $1..."

   if [ "$2" != "none" ]; then
      echo "# Warning! This file will be overwritten by configuration routines!" > target.os
      echo "TARGET=$2" >> target.os
   fi
}


# prepare for the given platform.

case "$1" in
   "win32"   ) proc_fix "Windows (FPC)" "WIN32";;
   "linux"   ) proc_fix "Linux (FPC)"   "LINUX";;
   "help"    ) proc_help;;
   *         ) proc_help;;
esac

echo "Done!"
