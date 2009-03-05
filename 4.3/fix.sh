#!/bin/sh
#
# Sets makefile source code for the different platforms
# Based on fix.sh of Allegro.
# Modified By Kronoman - In loving memory of my father.


# REMEMBER TO ALTER THIS TEST TO SUIT YOUR NEEDS!!!
proc_test()
{
    # You first need to configure the platform
    if [ ! -e target.os ]; then
	echo "Before test, you first must configure your platform."
	proc_help;
    else
	echo Testing, please wait...
	make test
	
	if [ $? -eq 0 -a -e test.run ]; then
	    echo
	    echo "* SUCESS *"
	    echo "Congratulations, the test compiled!"
	    echo
	else
	    echo
	    echo "* ERROR *"
	    echo
	    echo "The compilation returned a error or can't be runned!"
	    echo "Check that:"
	    echo "(*) You have all compiler tools installed (gcc,make,etc)"
	    echo
	fi

	echo "Cleaning the test..."
	make cleantest
    fi	
}

proc_help()
{
   echo "Usage: fix platform"
   echo
   echo "Where platform is one of: mingw32 or linux. "
   echo
   echo "NOTICE:"
   echo "You can also call: fix test"
   echo "to check if your system can compile this project."
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
   "mingw32" ) proc_fix "Windows (Mingw32)" "MINGW32";;
   "linux"   ) proc_fix "Linux (GCC)"       "LINUX";;
   "test"    ) proc_test;;
   "help"    ) proc_help;;
   *         ) proc_help;;
esac

echo "Done!"
