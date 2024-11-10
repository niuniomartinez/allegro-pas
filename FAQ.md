# Frecuent Asked Questions #

## I've compiled the examples but it returns errors saying it doesn't find `liballeg`.  Why? ##

You haven't installed Allegro or you have installed it incorrectly.  Read section
_Insallation and use_ in file `~/README.md` and follow the instructions.



## I've compiled the examples without errors but they don't work and they don't show any error message.  What's wrong? ##

See question **Why `al_init` returns `False`?** below.



## How can I learn how to use Allegro.pas? ##

Go to the `~/src/examples/` directory and open them.  The ones with numbers
introduce different concepts in order.  The other show some advanced concepts.

Also, there's a tutorial in the web site named [Vivace
](https://allegro-pas.sourceforge.net/tutorials/vivace/).  It shows how to use
Allegro.pas in an actual game.

Finally, there is a complete game in the `src/demos/` directory, with comments
explaining how it works.



## Why `al_init` returns `False`? ##

The main reason `al_init` fails is because the library version is old or it is
not compatible.

Look at the beginning of unit `allegro5`.  There are some constants that define
the version number.  Compare with the version of your _liballeg_ installation
and update if needed.  If your _liballeg_ is newer, maybe you need to downgrade
it.  Allegro.pas should work with Allegro 5.2.6 and up.

Remember that **only** the two first numbers of the version are the same in both
Allegro and Allegro.pas.
