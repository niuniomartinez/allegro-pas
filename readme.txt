 ______   ___    ___
/\  _  \ /\_ \  /\_ \
\ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
 \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
  \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
   \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
    \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                   /\____/               \ \_\
     Version 4.2.3 SVN             \_/__/                 \/_/

  A wrapper to use the Allegro library with Pascal compilers
  by Ñuño Martínez, February 21, 2009.



========
Contents
========

 - Introduction
 - License
 - Installation
 - How to use
 - Release programs
 - Help needed
 - Contact info



============
Introduction
============

  Allegro.pas is a wrapper to allow Pascal compilers (such as Free Pascal or
  Delphi) to use the Allegro library.

  I wrote it to use it by myself, so I didn't included all functionality of
  Allegro.  Aniway, I'll fix it and complete it as I'm using it (or as other
  users do).  Any collaboration will be wellcomed.



=======
License
=======

  Allegro & Allegro.pas are gift-ware.  They were created by a number of people
  working in cooperation, and is given to you freely as a gift.  You may use,
  modify, redistribute, and generally hack it about in any way you like, and
  you do not have to give us anything in return.  However, if you like this
  product you are encouraged to thank us by making a return gift to the Allegro
  community.  This could be by writing an add-on package, providing a useful
  bug report, making an improvement to the library, or perhaps just releasing
  the sources of your program so that other people can learn from them.  If you
  redistribute parts of this code or make a game using it, it would be nice if
  you mentioned Allegro somewhere in the credits, but you are not required to
  do this.  We trust you not to abuse our generosity.

   Disclaimer:

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT.  IN NO EVENT
   SHALL THE COPYRIGHT HOLDERS OR ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE
   FOR ANY DAMAGES OR OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.



============
Installation
============

NOTE:  This is an SVN version, so the library is under development.
       DON'T USE IT except for evaluation or testing.



  Current version works only in Windows, since I have no experience building
  .so files for *NIX based systems.  I didn't write an install script (again a
  lack of experience) but it isn't hard to do by hand.

  The best way is to install the binary version for Windows because it has all
  you'll need and you won't need to compile the dynamic library.


Binary version:

  Unpak the zip file and copy the files named "alleg42.dll" and "alpas42.dll"
  from the "lib" directory to your system directory
  (i.e.: C:\WINDOWS\SYSTEM32\).

  Then copy the whole "lib" directory in your compiler's unit directory and
  configure it:

  * Delphi: Go to "Project>Options>Directories/Conditionals>Search Path" and
    add the path.

  * Lazarus: Go to "Project>Compiler Options" and add the path in the "Other
    Unit Files" field.

  * Free Pascal: Set the directory using the "-Fu" option.

  That's all.

Source code version:

  First be sure you have a C/C++ compiler (i.e.: MinGW32), the Free Pascal
  compiler and the Allegro library (http://alleg.sourceforge.net/) installed
  and running in you system.

  Then unpak the Allegro.pas sources in your hard disk, open a console
  (i.e.: cmd.com) and go to the Allegro.pas root directory
  (i.e.: C:\allegro.pas\).  Then run the fix script this way:

  ~# fix
  >Usage: fix platform
  >
  >Where platform is one of: djgpp, mingw32 or linux. 
  >
  >NOTICE:
  >You can also call: fix test
  >to check if your system can compile this project.
  >
  >
  >Done!
  >
  ~# fix mingw32
  >Configuring for Windows (Mingw32)...
  >Done!

  Then execute "make".  Nothe that it's configured to use the GNU make so it
  wouldn't run using other "make" utilities such as "fpmake".  There are some
  command options available.  For example:

  ~# make help
  >Allegro.pas - http://allegro-pas.sourceforge.net/
  >--------
  >- Help -
  >--------
  >make all            } Builds the whole project.
  >make lib            } Creates the library only.
  >make examples       } Builds the examples only.
  >make tools          } Builds the tools only.
  >make demo           } Builds the demo game.
  >make clean          } Deletes temporary files.
  >make veryclean      } Deletes all builded files.
  >make status         } Shows some configuration variables.
  >make test           } Check program.
  >make help           } Shows this help.
  ~# make all
  >Allegro.pas - http://allegro-pas.sourceforge.net/
  > ...
  >Finished Allegro.pas
  >To create the HTML documentation, run 'make' from the docs directory

  Once the library is compiled (you must read the message
  "Finished Allegro.pas") follow the instructions from the "Binary version"
  avobe.



==========
How to use
==========

  To know how to use a procedure or struct, use your favourite web browser to
  read the documentation at http://allegro-pas.sourceforge.net/.  You can
  download it from there too.


================
Release programs
================

  Do not forget to include the "alpas42.dll" and "alleg42.dll" in the
  installation package or your programs wouldn't run at all!



===========
Help needed
===========

  The curren version is unfinished.  You can help to finish it and add your
  name to the author's list.  We need people to test the library, to complete
  the documentation, to translate it to other languajes or to write more
  wrapper functons, develop demos and examples.

  If you want to help, read the next section and contact me!



============
Contact info
============

  I've reserved space in sourceforge.net: http://allegro-pas.sourceforge.net.
  The project summary is http://sourceforge.net/projects/allegro-pas.

  There are forums and a mailing list there, so use it, please.

  Another nice forum is "Pascal Game Development"
  (http://www.pascalgamedevelopment.com/).

