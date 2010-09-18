UNIT alfont;
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
 *	Text fonts.
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
  albase, albitmap, alpalete; { Needs some basic definitions. }



TYPE
  AL_FONTptr = AL_PTR; { Do you think we need it? }



  FUNCTION al_load_font (filename: AL_STRING; palette: AL_PALETTEptr; p: AL_PTR)
	: AL_FONTptr;

  FUNCTION al_grab_font_from_bitmap (bmp: AL_BITMAPptr): AL_FONTptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'grab_font_from_bitmap';

  FUNCTION al_is_color_font (f: AL_FONTptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'is_color_font';

  FUNCTION al_is_mono_font (f: AL_FONTptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'is_mono_font';

  FUNCTION al_is_compatible_font (f1, f2: AL_FONTptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'is_compatible_font';

  PROCEDURE al_destroy_font (f: AL_FONTptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_font';
  
IMPLEMENTATION

  USES
    alcolor;



  FUNCTION load_font (filename: PCHAR; palette: AL_PALETTEptr; p: AL_PTR)
	   : AL_FONTptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_font';

  FUNCTION al_load_font (filename: AL_STRING; palette: AL_PALETTEptr; p: AL_PTR)
	: AL_FONTptr;
  BEGIN
    al_load_font := load_font (PCHAR (filename), palette, p);
  END;

END.

