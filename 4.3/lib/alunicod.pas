UNIT alunicod;
(*<Unicode routines.

  Allegro can manipulate and display text using any character values from 0
  right up to 2^32-1 (although the current implementation of the grabber can
  only create fonts using characters up to 2^16-1).  You can choose between a
  number of different text encoding formats, which controls how strings are
  stored and how Allegro interprets strings that you pass to it.  This setting
  affects all aspects of the system:  whenever you see a function that returns
  a @code(STRING) or @code(PCHAR) type, or that takes the same as an argument,
  that text will be in whatever format you have told Allegro to use.

  By default, Allegro uses UTF-8 encoded text (@code(AL_U_UTF8)).  This is a
  variable-width format, where characters can occupy anywhere from one to four
  bytes.  The nice thing about it is that characters ranging from 0-127 are
  encoded directly as themselves, so UTF-8 is upwardly compatible with 7-bit
  ASCII (@code('Hello, World!') means the same thing regardless of whether you
  interpret it as ASCII or UTF-8 data).  Any character values above 128, such
  as accented vowels, the UK currency symbol, and Arabic or Chinese characters,
  will be encoded as a sequence of two or more bytes, each in the range
  128-255.  This means you will never get what looks like a 7-bit ASCII
  character as part of the encoding of a different character value, which makes
  it very easy to manipulate UTF-8 strings.

  There are a few editing programs that understand UTF-8 format text files.
  Alternatively, you can write your strings in plain ASCII or 16-bit Unicode
  formats, and then use the Allegro textconv program to convert them into
  UTF-8.

  If you prefer to use some other text format, you can set Allegro to work with
  normal 8-bit ASCII (@code(AL_U_ASCII)), or 16-bit Unicode (@code(U_UNICODE))
  instead, or you can provide some handler functions to make it support
  whatever other text encoding you like (for example it would be easy to add
  support for 32 bit UCS-4 characters, or the Chinese GB-code format).

  There is some limited support for alternative 8-bit codepages, via the
  @code(AL_U_ASCII_CP) mode.  This is very slow, so you shouldn't use it for
  serious work, but it can be handy as an easy way to convert text between
  different codepages.  By default the @code(AL_U_ASCII_CP) mode is set up to
  reduce text to a clean 7-bit ASCII format, trying to replace any accented
  vowels with their simpler equivalents (this is used by the @link(al_message)
  function when it needs to print an error report onto a text mode DOS screen).
  If you want to work with other codepages, you can do this by passing a
  character mapping table to the @link(al_set_ucodepage) function.

  Note that you can use the Unicode routines before you call @link(al_install)
  or @link(al_init).  If you want to work in a text mode other than UTF-8, it
  is best to set it with @link(al_set_uformat) just before you call these. *)

{$IFDEF FPC}
{ Free Pascal. }
 {$LONGSTRINGS ON}
{$ENDIF}

INTERFACE

USES
  albase;

VAR
(* Fixed size, 8-bit ASCII characters. @seealso(al_set_uformat). *)
  AL_U_ASCII: LONGINT;
(* Alternative 8-bit codepage. @seealso(al_set_ucodepage)
   @seealso(al_set_uformat). *)
  AL_U_ASCII_CP: LONGINT;
(* Fixed size, 16-bit Unicode characters. @seealso(al_set_uformat). *)
  AL_U_UNICODE: LONGINT;
(* Variable size, UTF-8 format Unicode characters. @seealso(al_set_uformat). *)
  AL_U_UTF8: LONGINT;
(* Current encoding. *)
  AL_U_CURRENT: LONGINT;



(* Sets the current text encoding format.  This will affect all parts of
   Allegro, wherever you see a function that returns a string, or takes a
   string as a parameter.

   Although you can change the text format on the fly, this is not a good
   idea.  Many strings, for example the names of your hardware drivers and any
   language translations, are loaded when you call @link(al_init), so if you
   change the encoding format after this, they will be in the wrong format, and
   things will not work properly.  Generally you should only call
   @code(al_set_uformat) once, before @code(al_init), and then leave it on the
   same setting for the duration of your program.

   @param(type Should be one of these values: @link(AL_U_ASCII),
     @link(AL_U_ASCII_CP), @link(AL_U_UNICODE), @link(AL_U_UTF8).) *)
  PROCEDURE al_set_uformat (aType: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_uformat';



(* Finds out what text encoding format is currently selected.  This function is
   probably useful only if you are writing an Allegro addon dealing with text
   strings and you use a different codepath for each possible format.

   @returns(The currently selected text encoding format.)
   @seealso(al_set_uformat) *)
  FUNCTION al_get_uformat: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_uformat';



(* When you select the @link(AL_U_ASCII_CP) encoding mode, a set of tables are
   used to convert between 8-bit characters and their Unicode equivalents.  You
   can use this function to specify a custom set of mapping tables, which
   allows you to support different 8-bit codepages.

   Allegro will use the @code(table) parameter when it needs to convert an
   ASCII string to an Unicode string.  But when Allegro converts an Unicode
   string to ASCII, it will use both parameters.  First, it will loop through
   the @code(table) parameter looking for an index position pointing at the
   Unicode value it is trying to convert (ie. the @code(table) parameter is
   also used for reverse matching).  If that fails, the @code(extras) list is
   used.  If that fails too, Allegro will put the character @code(`^'), giving
   up the conversion.

   Note that Allegro comes with a default parameters set internally.  The
   default @code(table) will convert 8-bit characters to @code(`^').  The
   default @code(extras) list reduces Latin-1 and Extended-A characters to 7
   bits in a sensible way (eg. an accented vowel will be reduced to the same
   vowel without the accent).
   @param(table Points to an array of 256 short integers, which contain the
     Unicode value for each character in your codepage.)
   @param(extras If not @nil, points to a list of mapping pairs, which will be
     used when reducing Unicode data to your codepage.  Each pair consists of a
     Unicode value, followed by the way it should be represented in your
     codepage.  The list is terminated by a zero Unicode value.  This allows
     you to create a many->one mapping, where many different Unicode characters
     can be represented by a single codepage value @(eg. for reducing accented
     vowels to 7-bit ASCII@).) *)
  PROCEDURE al_set_ucodepage (CONST table, extras: PWORD); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_ucodepage';



(* Returns the number of characters in the string.  Note that this doesn't have
   to equal the string's size in bytes. *)
  FUNCTION al_ustrlen (s: STRING): LONGINT;



IMPLEMENTATION

  USES
    alsystem;



  FUNCTION ustrlen (s: PCHAR): LONGINT;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_ustrlen (s: STRING): LONGINT;
  BEGIN
    al_ustrlen := ustrlen (PCHAR (s));
  END;



INITIALIZATION
{ Create identifiers. }
  AL_U_ASCII	:= AL_ID('ASC8');
  AL_U_ASCII_CP	:= AL_ID('ASCP');
  AL_U_UNICODE	:= AL_ID('UNIC');
  AL_U_UTF8	:= AL_ID('UTF8');
  AL_U_CURRENT	:= AL_ID('cur.');
END.
