UNIT alUnicode;
(*<Unicode support routines.

  @bold(Note:) I'm not sure how much this is compatible with FPC's Unicode support.
 *)

{$INCLUDE allegro.cfg}

INTERFACE

  USES
    alBase;

  CONST
  (* Fixed size, 8-bit ASCII characters. @seealso(al_set_uformat). *)
    AL_U_ASCII = $41534338; { 'ASC8' }
  (* Alternative 8-bit codepage. @seealso(al_set_ucodepage)
     @seealso(al_set_uformat). *)
    AL_U_ASCII_CP = $41534350; { 'ASCP' }
  (* Fixed size, 16-bit Unicode characters. @seealso(al_set_uformat). *)
    AL_U_UNICODE = $554E4943; { 'UNIC' }
  (* Variable size, UTF-8 format Unicode characters. @seealso(al_set_uformat). *)
    AL_U_UTF8 = $55544638; { 'UTF8' }
  (* Current encoding. @seealso(al_set_uformat) *)
    AL_U_CURRENT = $6375722E; { 'cur.' }

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
     @link(AL_U_ASCII_CP), @link(AL_U_UNICODE), @link(AL_U_UTF8).) 
   @seealso(al_get_uformat) @seealso(al_set_ucodepage) *)
  PROCEDURE al_set_uformat (aType: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_uformat';

(* Finds out what text encoding format is currently selected.  This function is
   probably useful only if you are writing an Allegro addon dealing with text
   strings and you use a different codepath for each possible format.

   @returns(The currently selected text encoding format.)
   @seealso(al_set_uformat) *)
  FUNCTION al_get_uformat: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_uformat';

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
     vowels to 7-bit ASCII@).)
   @seealso(al_set_uformat) *)
  PROCEDURE al_set_ucodepage (CONST table, extras: AL_USHORTptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_ucodepage';

(* @returns(The number of characters in the string.  Note that this doesn't have
   to equal the string's size in bytes.) *)
  FUNCTION al_ustrlen (s: AL_STR): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'ustrlen';

(* Converts the specified string @code(s) from @code(aType) to @code(newtype),
   it checks before doing the conversion, and doesn't bother if the string
   formats are already the same @(either both types are equal, or one is
   ASCII, the other is UTF-8, and the string contains only 7-bit ASCII
   characters@).
   @seealso(al_set_uformat) @seealso(al_uconvert_ascii)
   @seealso(al_uconvert_toascii) *)
  FUNCTION al_uconvert (CONST s: AL_STR; aType, newtype: AL_INT): AL_STRptr;
    INLINE;

(* Converts strings from ASCII into the current encoding format.
   @seealso(al_uconvert) @seealso(al_uconvert_toascii) *)
  FUNCTION al_uconvert_ascii (CONST s: AL_STR): STRING;
    INLINE;

(* Converts strings from the current encoding format into ASCII.
   @seealso(al_uconvert) @seealso(al_uconvert_ascii) *)
  FUNCTION al_uconvert_toascii (CONST s: AL_STR): STRING;
    INLINE;


IMPLEMENTATION

  USES
    Allegro, sysutils;

  FUNCTION uconvert (CONST s: AL_STR; aType: AL_INT; buf: AL_STR; newtype, size: AL_INT): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'uconvert';

  FUNCTION al_uconvert (CONST s: AL_STR; aType, newtype: AL_INT): AL_STRptr;
  VAR
    Buffer: AL_STRptr;
    StrLength: INTEGER;
  BEGIN
    StrLength := Length (s) + 1;
    Buffer := StrAlloc (StrLength);
    RESULT := uconvert (s, aType, Buffer, newtype, StrLength);
    StrDispose (Buffer)
  END;

  FUNCTION al_uconvert_ascii (CONST s: AL_STR): STRING;
  BEGIN
    al_uconvert_ascii := al_uconvert (s, AL_U_ASCII, AL_U_CURRENT);
  END;

  FUNCTION al_uconvert_toascii (CONST s: AL_STR): STRING;
  BEGIN
    al_uconvert_toascii := al_uconvert (s, AL_U_CURRENT, AL_U_ASCII);
  END;

END.
