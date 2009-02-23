(* Color manipulation. *)
UNIT alcolor;

{ Defines the frame. }
{$MODE Delphi}
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
   appropriate. *)
  al_makecol: FUNCTION (r, g, b: LONGINT): LONGINT; CDECL;
(* This function convert colors from a hardware independent form (red, green,
   and blue values ranging 0-255) into 8 bit pixel formats. *)
  al_makecol8: FUNCTION (r, g, b: LONGINT): LONGINT; CDECL;
(* Converts colors from a hardware independent format (red, green, and blue
   values ranging 0-255) to the pixel format required by the specified color
   depth. *)
  al_makecol_depth: FUNCTION (color_depth, r, g, b: LONGINT): LONGINT; CDECL;
(* Convert RGBA colors into display dependent pixel formats. *)
  al_makeacol: FUNCTION (r, g, b, a: LONGINT): LONGINT; CDECL;
(* Convert RGBA colors into display dependent pixel formats. *)
  al_makeacol_depth: FUNCTION (color_depth, r, g, b, a: LONGINT): LONGINT; CDECL;
(* Convert color values between the HSV and RGB colorspaces. *)
  al_rgb_to_hsv: PROCEDURE (r, g, b: LONGINT; h, s, v: PDOUBLE); CDECL;



(* Convert color values between the HSV and RGB colorspaces. *)
  PROCEDURE al_hsv_to_rgb (h, s, v: DOUBLE; r, g, b: PLONGINT);



IMPLEMENTATION

USES
  albase; { Needs some basic definitions. }



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
  @al_makecol := al_get_object_address ('makecol');
  @al_makecol8 := al_get_object_address ('makecol8');
  @al_makecol_depth := al_get_object_address ('makecol_depth');
  @al_makeacol := al_get_object_address ('makeacol');
  @al_makeacol_depth := al_get_object_address ('makeacol_depth');
  @al_rgb_to_hsv := al_get_object_address ('rgb_to_hsv');
END.
