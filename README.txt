~~~
 ______   ___    ___
/\  _  \ /\_ \  /\_ \
\ \ \L\ \\//\ \ \//\ \      __     __   _  __  ___        __    ___      ____
 \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
  \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
   \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
    \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                   /\____/               \ \_\
     Version 5.2.beta.1-1          \/___/                 \/_/

  A wrapper to use the Allegro library with Pascal compilers
  by Guillermo "Ñuño" Martínez, April 16, 2020.
~~~


# Introduction #

**Allegro.pas** is a wrapper that allows Pascal compilers (such as Free Pascal
and Delphi) to use the _Allegro library_.

Note that Allegro 5 is wholly *incompatible* with Allegro 4 and earlier
versions, and so Allegro.pas is, but both may be installed at the same time
without conflicts.

This is a beta version of the Allegro.pas 5 version.  It is released to show
what's done and to allow developers to test it and give feed-back.  Please go
to the Allegro.pas' project site at SourceForge or GitHub to know about progress
and know when new versions are released.

Current version was tested with Free Pascal on WindowsXP, Windows7 and
GNU/Linux, both 32bit and 64 bit, and with Delphi 10.3 Community Edition on
Windows 7 32bit.  MacOS was tested but it is no possible to make it work.
Other operating systems and compilers weren't tested.

As this is a WIP version you should remember that at the moment not all
functionality are implemented.

## Implemented state ##

+ Event manager, including but not limited to keyboard, mouse, timer and
  display.
+ Joysticks.
+ OpenGL (except extensions).
+ Shaders.
+ Software bitmaps.
+ Text drawing.
+ ttf fonts.
+ Audio samples and streams.
+ kcm audio.
+ 2D and 3D transformations.
+ Clipboard.
+ Custom memory management.
+ OpenGL extensions.
+ Demonstration game.

## Partially implemented or need more testing ##

* Primitive drawing.

## Unimplemented but planned ##

- Touch-screen support.



# License #

Allegro.pas is released under zlib/png license.  Read it in file `LICENSE.txt`.



# Installation and use #

Installation and use are different depending on the operating system and
compiler you're using.  To make things more easy I've wrote different documents
for each one you can find in the `~/docs/build` directory.  For example, read
`windows.txt` to know how to get and install Allegro in your Windows system,
then read `lazarus.txt` to know how to use it with Lazarus IDE.

For Lazarus and Free Pascal users it is recommendable to read the `makefile.txt`
file too.



# Documentation #

You can build it (read `~/docs/build/make.txt`) or download it from the webpage
(see **Contact info**).  You can also read it on-line from SourceForge:
[http://allegro-pas.sourceforge.net/docs/5.2/]()

Also, there are a collection of examples and a demonstration game.  Open them
and read the code.  Some have a lot of comments that explain them.



# Contact info #

The [project page](https://allegro-pas.sourceforge.net/) have forums and a
mailing-list you can use.

I've created a [mirror at GitHub](https://github.com/niuniomartinez/allegro-pas/)
I try to keep it up-to-date:

You may find interesting the Game Development portal at the [Free Pascal wiki
pages](https://wiki.freepascal.org/Portal:Game_Development).

Also there are some useful forums:

* [Allegro community](http://www.allegro.cc/) (Lately have problems though.)
* [Pascal Game Development (AKA "PGD")](https://www.pascalgamedevelopment.com/)
* [PGD Telegram Chat](https://t.me/joinchat/AAAAAA53Fi94SyjjD5xYnA)
* [Lazarus games](https://forum.lazarus.freepascal.org/index.php/board,74.0.html)

Useful discord servers:
- [Allegro community](https://discord.gg/yJ49UusV).  Most Allegro.cc people are
  here now!
- [Unnoficial Free Pascal](https://discord.gg/nAGPGJjK)
