UNIT altext;
(*
  ______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\ 
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/
 *
 *	Text drawing.
 *	by Ñuño Martínez <>
 *
 *	See readme.txt for license and copyright information.
 *)

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$LONGSTRINGS ON}
{$ELSE}
{ Assumes Borland Delphi/Turbo. }
 {$A-}
 {$H+}
{$ENDIF}



INTERFACE

USES
  albase, albitmap, alfont; { Needs some basic definitions. }



VAR
  al_font: ^AL_FONTptr; { A pointer to a pointer because we can change it. }
  al_404_char: AL_INTptr;



  PROCEDURE al_textout_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: AL_STRING; x, y, color, bg: AL_INT);
  PROCEDURE al_textout_centre_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: AL_STRING; x, y, color, bg: AL_INT);
  PROCEDURE al_textout_right_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: AL_STRING; x, y, color, bg: AL_INT);
  PROCEDURE al_textout_justify_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: AL_STRING; x1, x2, y, diff, color, bg: AL_INT);

  FUNCTION al_text_length (f: AL_FONTptr; str: AL_STRING): AL_INT;
  FUNCTION al_text_height (f: AL_FONTptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'text_height';



IMPLEMENTATION

PROCEDURE textout_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x, y, color, bg: AL_INT); CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'textout_ex';

PROCEDURE al_textout_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: AL_STRING; x, y, color, bg: AL_INT);
BEGIN
  textout_ex (bmp, f, PCHAR (str), x, y, color, bg);
END;



PROCEDURE textout_centre_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x, y, color, bg: AL_INT); CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'textout_centre_ex';

PROCEDURE al_textout_centre_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: AL_STRING; x, y, color, bg: AL_INT);
BEGIN
  textout_centre_ex (bmp, f, PCHAR (str), x, y, color, bg);
END;


PROCEDURE textout_right_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x, y, color, bg: AL_INT); CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'textout_right_ex';

PROCEDURE al_textout_right_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: AL_STRING; x, y, color, bg: AL_INT);
BEGIN
  textout_right_ex (bmp, f, PCHAR (str), x, y, color, bg);
END;



PROCEDURE textout_justify_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x1, x2, y, diff, color, bg: AL_INT); CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'textout_justify_ex';

PROCEDURE al_textout_justify_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: AL_STRING; x1, x2, y, diff, color, bg: AL_INT);
BEGIN
  textout_justify_ex (bmp, f, PCHAR (str), x1, x2, y, diff, color, bg);
END;



FUNCTION text_length (f: AL_FONTptr; str: PCHAR): AL_INT; CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'text_length';

FUNCTION al_text_length (f: AL_FONTptr; str: AL_STRING): AL_INT;
BEGIN
  al_text_length := text_length (f, PCHAR (str));
END;

END.

