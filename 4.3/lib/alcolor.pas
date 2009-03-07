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

  PROCEDURE al_hsv_to_rgb (h, s, v: AL_FLOAT; r, g, b: AL_INTptr);
{ Next commented declaration is for "al_hsv_to_rgb" but for some reason it
  throws EAccessViolation exception, so this procedure was translated from
  the original C code.  See implementation.
  CDECL;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'hsv_to_rgb'; }
  PROCEDURE al_rgb_to_hsv (r, g, b: AL_INT; h, s, v: AL_FLOATptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rgb_to_hsv';



IMPLEMENTATION

(* al_hsv_to_rgb:
 *   Converts from HSV colorspace to RGB values.  Translated from the original
 *   C code writen by Dave Thomson. *)
PROCEDURE al_hsv_to_rgb (h, s, v: AL_FLOAT; r, g, b: AL_INTptr);
VAR
  f, x, y, z: AL_FLOAT;
  i: AL_INT;
BEGIN
  v := v * 255.0;

  IF s = 0.0 THEN
  BEGIN
  { ok since we don't divide by s, and faster }
    r^ := TRUNC (v + 0.5); g^ := r^; b^ := r^;
  END
  ELSE BEGIN
    WHILE h >= 360.0 DO
      h := h - 360.0;
    h := h / 60.0;
    IF h < 0.0 THEN
      h := h + 6.0;

    i := TRUNC (h);
    f := h - i;
    x := v * s;
    y := x * f;
    v := v + 0.5; { round to the nearest integer below }
    z := v - x;

    CASE i OF
    6:
      BEGIN
	r^ := TRUNC (v);
	g^ := TRUNC (z + y);
	b^ := TRUNC (z);
      END;
    0:
      BEGIN
	r^ := TRUNC (v);
	g^ := TRUNC (z + y);
	b^ := TRUNC (z);
      END;
    1:
      BEGIN
	r^ := TRUNC (v - y);
	g^ := TRUNC (v);
	b^ := TRUNC (z);
      END;
    2:
      BEGIN
	r^ := TRUNC (z);
	g^ := TRUNC (v);
	b^ := TRUNC (z + y);
      END;
    3:
      BEGIN
	r^ := TRUNC (z);
	g^ := TRUNC (v - y);
	b^ := TRUNC (v);
      END;
    4:
      BEGIN
	r^ := TRUNC (z + y);
	g^ := TRUNC (z);
	b^ := TRUNC (v);
      END;
    5:
      BEGIN
	r^ := TRUNC (v);
	g^ := TRUNC (z);
	b^ := TRUNC (v - y);
      END;
    END;
  END;
END;



END.

