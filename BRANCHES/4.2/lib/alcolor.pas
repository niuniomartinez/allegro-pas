UNIT alcolor;
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
 *	Color manipulation.
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
  albase; { Needs some basic definitions. }



TYPE
(* RGB color descriptor. *)
  AL_RGBptr = ^AL_RGB;
  AL_RGB = RECORD
    r	  : AL_UCHAR;
    g	  : AL_UCHAR;
    b	  : AL_UCHAR;
    filler: AL_UCHAR;
  END;




  FUNCTION al_makecol (r, g, b: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makecol';
  FUNCTION al_makecol8 (r, g, b: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makecol8';
  FUNCTION al_makecol_depth (color_depth, r, g, b: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makecol_depth';

  FUNCTION al_makeacol (r, g, b, a: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makeacol';
  FUNCTION al_makeacol_depth (color_depth, r, g, b, a: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makeacol_depth';

  PROCEDURE al_hsv_to_rgb (h, s, v: AL_FLOAT; r, g, b: AL_INTptr); CDECL;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'hsv_to_rgb';
  PROCEDURE al_rgb_to_hsv (r, g, b: AL_INT; h, s, v: AL_FLOATptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rgb_to_hsv';



IMPLEMENTATION

END.

