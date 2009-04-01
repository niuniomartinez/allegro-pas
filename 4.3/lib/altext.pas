UNIT altext;
(*< Allegro provides text output routines that work with both monochrome and
    color fonts, which can contain any number of Unicode character ranges.  The
    grabber program can create fonts from sets of characters drawn in a bitmap
    file (see grabber.txt for more information), and can also import GRX or
    BIOS format font files.  The font structure contains a number of hooks that
    can be used to extend it with your own custom drawing code:  see the
    definition in allegro/text.h for details.  *)

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
  al_font: AL_FONTptr; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'font';
  al_404_char: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_404_char';



  PROCEDURE al_textout_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
  PROCEDURE al_textout_centre_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
  PROCEDURE al_textout_right_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
  PROCEDURE al_textout_justify_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x1, x2, y, diff, color, bg: LONGINT);

  FUNCTION al_text_length (f: AL_FONTptr; str: STRING): LONGINT;
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

