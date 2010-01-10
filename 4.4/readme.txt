 ______   ___    ___
/\  _  \ /\_ \  /\_ \
\ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
 \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
  \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
   \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
    \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                   /\____/               \ \_\
     Version 4.4.0                 \_/__/                 \/_/

  A wrapper to use the Allegro library with Pascal compilers
  by Ñuño Martínez, January 11, 2010.



========
Contents
========

 - Introduction
 - Features
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
  Allegro.  Anyway, I'll fix it and complete it as I'm using it (or as other
  users do).  Any collaboration will be welcomed.



========
Features
========

  Cross-platform support for Windows and GNU/Linux.  Should be easy to use on
  POSIX systems as MacOS and BSD but this wasn't tested yet.  If you do it
  please tell us your experience.

  Drawing functions including putpixel, line, rectangles, sprites, etc.
  Supports clipping, translucency/lighting and can draw directly to the
  screen or to memory bitmaps.

  Full-screen or windowed modes and 8, 15, 16, 24 and 32 bits per pixel.

  FLI/FLC animation player.

  Plays background MIDI music and up to 64 simultaneous sound effects.  Samples
  can be looped (forwards, backwards, or bidirectionally), and the volume, pan,
  pitch, etc, can be adjusted while they are playing.

  Easy access to the mouse, keyboard, joystick, and high resolution timer
  interrupts.

  Routines for reading and writing LZSS compressed files.

  GUI dialog manager and file selector.

  3D textured polygons with and without transparences.



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

  Be sure your have Free Pascal compiler installed and configured on your
  system.  If you have Lazarus IDE then it should be enough.  Visit Free
  Pascal's web site for more information (http://www.freepascal.org/).

  If you're using GNU/Linux then install Allegro 4.4.0.  You can download it
  from http://sourceforge.net/project/showfiles.php?group_id=5665 .  May be you
  can install the "enduser" version.

  If you're using Windows then you have two ways:  Install Allegro as said in
  the previous paragraph to obtain the file "alleg44.dll" or download the
  "bin" version of Allegro.pas wich include such file and precompiled tools.

  NOTE:  Allegro.pas 4.4.0 will work ONLY with Allegro 4.4.0 (and its RC
  releases).  It will not work with newer versions at the moment.

  Open a console and go to the Allegro.pas directory.  In most cases just
  use "cd allegro.pas".

  Execute "fix.bat" on Windows or "./fix.sh" on GNU/Linux and follow the
  on screen instructions.

  Execute the "make" command.  This should create the examples and the
  demonstration game.  If there's an error then be sure you have Allegro,
  Free Pascal and any additional library correctly installed and configured.
  If you have problems read the "Contact info" section.

  There's HTML documentation at "docs" subdirectory.  You can rebuild that
  using "pasdoc" (http://pasdoc.sipsolutions.net/).



==========
How to use
==========

  To use Allegro.pas in your programs just copy files with extension ".pas"
  and ".inc" from directory "lib" to your program sources directory.  Also you
  can copy those files to your compiler's unit directory.

  ATM no special configuration needed.



================
Release programs
================

  Refer to Allegro's documentation to know which files your need to distribute
  with your program.



===========
Help needed
===========

  You can help to finish Allegro.pas and add your name to the author's list.
  We need people to test the library, to complete the documentation, to
  translate it to other languages or to write more wrapper functions, develop
  demos and examples.

  If you want to help, read the next section and contact us!



============
Contact info
============

  You'll find more information at http://allegro-pas.sourceforge.net.  There
  are forums, a mailing-list and "wiki" pages in that site.

  Another nice forum I visit is "Pascal Game Development"
  (http://www.pascalgamedevelopment.com/).
