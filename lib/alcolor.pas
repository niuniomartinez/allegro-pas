(* Color manipulation.

   In a truecolor video mode the red, green, and blue components for each pixel
   are packed directly into the color value, rather than using a palette lookup
   table. In a 15-bit mode there are 5 bits for each color, in 16-bit modes
   there are 5 bits each of red and blue and six bits of green, and both 24 and
   32-bit modes use 8 bits for each color (the 32-bit pixels simply have an
   extra padding byte to align the data nicely). The layout of these components
   can vary depending on your hardware, but will generally either be RGB or
   BGR. Since the layout is not known until you select the video mode you will
   be using, you must call al_set_gfx_mode before using any of the following
   routines! *)
UNIT alcolor;

{ Defines the frame. }
{$MODE Delphi}
{$MACRO ON}
{$PACKRECORDS C}
{$H+}



INTERFACE

TYPE
(* Pointer to a palette entry. *)
  AL_RGBptr = ^AL_RGB;
(* Palette entry.  It contains an additional field for the purpose of padding
   but you should not usually care about it.  Read chapter Palette routines for
   a description on how to obtain/use this structure. *)
  AL_RGB = RECORD
    r	  : BYTE;
    g	  : BYTE;
    b	  : BYTE;
    filler: BYTE;
  END;



VAR
(* Converts colors from a hardware independent format (red, green, and blue
   values ranging 0-255) to the pixel format required by the current video
   mode, calling the preceding 8, 15, 16, 24, or 32-bit makecol functions as
   appropriate. Example:

  green_color := al_makecol (0, 255, 0);
 *)
  al_makecol: FUNCTION (r, g, b: LONGINT): LONGINT; CDECL;
(* This function convert colors from a hardware independent form (red, green,
   and blue values ranging 0-255) into 8 bit pixel formats.  Converting to an
   8-bit color involves searching the palette to find the closest match, which
   is quite slow unless you have set up an RGB mapping table. Example:

  green_color := al_makecol8 (0, 255, 0);
 *)
  al_makecol8: FUNCTION (r, g, b: LONGINT): LONGINT; CDECL;
(* Converts colors from a hardware independent format (red, green, and blue
   values ranging 0-255) to the pixel format required by the specified color
   depth. Example:

  green_15bit := al_makecol_depth (15, 0, 255, 0);
 *)
  al_makecol_depth: FUNCTION (color_depth, r, g, b: LONGINT): LONGINT; CDECL;
(* Convert RGBA colors into display dependent pixel formats.  In anything less
   than a 32-bit mode, these are the same as calling al_makecol or
   al_makecol_depth, but by using this routine it is possible to create 32-bit
   color values that contain a true 8 bit alpha channel along with the red,
   green, and blue components.  You should only use RGBA format colors as the
   input to al_draw_trans_sprite or al_draw_trans_rle_sprite after calling
   al_set_alpha_blender, rather than drawing them directly to the screen. *)
  al_makeacol: FUNCTION (r, g, b, a: LONGINT): LONGINT; CDECL;
(* Convert RGBA colors into display dependent pixel formats.  In anything less
   than a 32-bit mode, these are the same as calling al_makecol or
   al_makecol_depth, but by using this routine it is possible to create 32-bit
   color values that contain a true 8 bit alpha channel along with the red,
   green, and blue components.  You should only use RGBA format colors as the
   input to al_draw_trans_sprite or al_draw_trans_rle_sprite after calling
   al_set_alpha_blender, rather than drawing them directly to the screen. *)
  al_makeacol_depth: FUNCTION (color_depth, r, g, b, a: LONGINT): LONGINT; CDECL;
(* Convert color values between the HSV and RGB colorspaces. The RGB values
   range from 0 to 255, hue is from 0 to 360, and saturation and value are
   from 0 to 1. Example:

VAR
  r, g, b: AL_INT;
  hue, saturation, value: AL_FLOAT;
BEGIN
          ...
{ Convert a reddish color to HSV format. }
  al_rgb_to_hsv (255, 0, 128, @hue, @saturation, @value);
{ Now put our tin foil hat, and verify that. }
  al_hsv_to_rgb (hue, saturation, value, @r, @g, @b);
END; *)
  al_rgb_to_hsv: PROCEDURE (r, g, b: LONGINT; h, s, v: PDOUBLE); CDECL;



(* Convert color values between the HSV and RGB colorspaces. The RGB values
   range from 0 to 255, hue is from 0 to 360, and saturation and value are
   from 0 to 1. Example:

VAR
  r, g, b: AL_INT;
  hue, saturation, value: AL_FLOAT;
BEGIN
          ...
{ Convert a reddish color to HSV format. }
  al_rgb_to_hsv (255, 0, 128, @hue, @saturation, @value);
{ Now put our tin foil hat, and verify that. }
  al_hsv_to_rgb (hue, saturation, value, @r, @g, @b);
END; *)
  PROCEDURE al_hsv_to_rgb (h, s, v: DOUBLE; r, g, b: PLONGINT);



IMPLEMENTATION

USES
  albase, { Needs some basic definitions. }
  dynlibs;



(* Converts from HSV colorspace to RGB values.  Translated from the original
   C code writen by Dave Thomson. *)
PROCEDURE al_hsv_to_rgb (h, s, v: DOUBLE; r, g, b: PLONGINT);
VAR
  f, x, y, z: DOUBLE;
  i: LONGINT;
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



INITIALIZATION
{ Gets the function and procedure address. }
  @al_makecol := GetProcAddress (__al_library_id__, 'makecol');
  @al_makecol8 := GetProcAddress (__al_library_id__, 'makecol8');
  @al_makecol_depth := GetProcAddress (__al_library_id__, 'makecol_depth');
  @al_makeacol := GetProcAddress (__al_library_id__, 'makeacol');
  @al_makeacol_depth := GetProcAddress (__al_library_id__, 'makeacol_depth');
  @al_rgb_to_hsv := GetProcAddress (__al_library_id__, 'rgb_to_hsv');
END.
