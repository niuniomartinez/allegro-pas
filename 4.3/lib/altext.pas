UNIT altext;
(*<Text output.

  Allegro provides text output routines that work with both monochrome and
  color fonts, which can contain any number of Unicode character ranges.  The
  grabber program can create fonts from sets of characters drawn in a bitmap
  file (see grabber.txt for more information), and can also import GRX or BIOS
  format font files. *)

{$IFDEF FPC}
{ Free Pascal. }
 {$MODE FPC}
 {$PACKRECORDS C}
{$ELSE}
{ Assumes Codegear Delphi/Turbo. }
 {$A-}
{$ENDIF}
{$H+}



INTERFACE

USES
  albase, albitmap, alfont; { Needs some basic definitions. }



VAR
(* A simple 8x8 fixed size font (the mode 13h BIOS default).  This font
   contains the standard ASCII (U+20 to U+7F), Latin-1 (U+A1 to U+FF), and
   Latin Extended-A (U+0100 to U+017F) character ranges. *)
  al_font: AL_FONTptr; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'font';
(* When Allegro cannot find a glyph it needs in a font, it will instead output
   the character given in this variable.  By default, this is set to the caret
   symbol, @code(^), but you can change this global to use any other character
   instead.*)
  al_404_char: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_404_char';



(* Writes the string onto the bitmap at given position, using the specified
   font, foreground color and background color.  If the background color is -1,
   then the text is written transparently.  If the foreground color is -1 and a
   color font is in use, it will be drawn using the colors from the original
   font bitmap (the one you imported into the grabber program), which allows
   multicolored text output.  For high and true color fonts, the foreground
   color is ignored and always treated as -1.
   @param(bmp The output bitmap.)
   @param(f The font to render.)
   @param(x Horizontal position.) @param(y Vertical position.)
   @param(color Foreground color.  Set to -1 to use multicolor fonts.)
   @param(bg Background color.  Set to -1 to use transparent background.)
   @seealso(al_textout_centre_ex) @seealso(al_textout_right_ex) @seealso(al_textout_justify_ex)*)
  PROCEDURE al_textout_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
(* Like @link(al_textout_ex), but interprets the @code(x) coordinate as the
   centre rather than the left edge of the string.
   @seealso(al_textout_ex) @seealso(al_textout_right_ex) @seealso(al_textout_justify_ex)*)
  PROCEDURE al_textout_centre_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
(* Like @link(al_textout_ex), but interprets the @code(x) coordinate as the
   right rather than the left edge of the string.
   @seealso(al_textout_ex) @seealso(al_textout_centre_ex) @seealso(al_textout_justify_ex)*)
  PROCEDURE al_textout_right_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
(* Draws justified text within the region @code(x1-x2).  If the amount of spare
   space is greater than the @code(diff) value, it will give up and draw
   regular left justified text instead.
   @seealso(al_textout_ex) @seealso(al_textout_centre_ex) @seealso(al_textout_right_ex)*)
  PROCEDURE al_textout_justify_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x1, x2, y, diff, color, bg: LONGINT);



(* Returns the length (in pixels) of a string in the specified font. *)
  FUNCTION al_text_length (f: AL_FONTptr; str: STRING): LONGINT;
(* Returns the height (in pixels) of the specified font. *)
  FUNCTION al_text_height (f: AL_FONTptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'text_height';



IMPLEMENTATION

PROCEDURE textout_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x, y, color, bg: LONGINT); CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'textout_ex';

PROCEDURE al_textout_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
BEGIN
  textout_ex (bmp, f, PCHAR (str), x, y, color, bg);
END;



PROCEDURE textout_centre_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x, y, color, bg: LONGINT); CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'textout_centre_ex';

PROCEDURE al_textout_centre_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
BEGIN
  textout_centre_ex (bmp, f, PCHAR (str), x, y, color, bg);
END;


PROCEDURE textout_right_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x, y, color, bg: LONGINT); CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'textout_right_ex';

PROCEDURE al_textout_right_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
BEGIN
  textout_right_ex (bmp, f, PCHAR (str), x, y, color, bg);
END;



PROCEDURE textout_justify_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x1, x2, y, diff, color, bg: LONGINT); CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'textout_justify_ex';

PROCEDURE al_textout_justify_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x1, x2, y, diff, color, bg: LONGINT);
BEGIN
  textout_justify_ex (bmp, f, PCHAR (str), x1, x2, y, diff, color, bg);
END;



FUNCTION text_length (f: AL_FONTptr; str: PCHAR): LONGINT; CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'text_length';

FUNCTION al_text_length (f: AL_FONTptr; str: STRING): LONGINT;
BEGIN
  al_text_length := text_length (f, PCHAR (str));
END;

END.

