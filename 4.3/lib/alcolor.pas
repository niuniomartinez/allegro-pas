UNIT alcolor;
(*< In a truecolor video mode the red, green, and blue components for each
    pixel are packed directly into the color value, rather than using a palette
    lookup table.  In a 15-bit mode there are 5 bits for each color, in 16-bit
    modes there are 5 bits each of red and blue and six bits of green, and both
    24 and 32-bit modes use 8 bits for each color (the 32-bit pixels simply
    have an extra padding byte to align the data nicely).  The layout of these
    components can vary depending on your hardware, but will generally either
    be RGB or BGR.  Since the layout is not known until you select the video
    mode you will be using, you must call @link (al_set_gfx_mode) before using
    any of the following routines!  *)

{$H+}
{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
{$ELSE}
{ Assumes Codegear Delphi/Turbo. }
 {$A-}
{$ENDIF}



INTERFACE

USES
  albase; { Needs some basic definitions. }



TYPE
(* Pointer to @link (AL_RGB). *)
  AL_RGBptr = ^AL_RGB;

(* Palette entry.  It contains an additional field for the purpose of padding
   but you should not usually care about it.  Read chapter "Palette routines"
   for a description on how to obtain/use this structure. *)
  AL_RGB = RECORD
    r	  : BYTE;
    g	  : BYTE;
    b	  : BYTE;
    filler: BYTE;
  END;



(* Converts colors from a hardware independent format (red, green, and blue
   values ranging 0-255) to the pixel format required by the current video
   mode, calling the preceding 8, 15, 16, 24, or 32-bit makecol functions as
   appropriate.

   @returns (the requested RGB triplet in the current color depth.) *)
  FUNCTION al_makecol (r, g, b: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makecol';

(* Converts colors from a hardware independent form (red, green, and blue
   values ranging 0-255) into 8-bit color index.  Converting to an 8-bit color
   involves searching the palette to find the closest match, which is quite
   slow unless you have set up an RGB mapping table.

   @returns (the requested RGB triplet in the specified color depth.) *)
  FUNCTION al_makecol8 (r, g, b: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makecol8';

(* Converts colors from a hardware independent format (red, green, and blue
   values ranging 0-255) to the pixel format required by the specified color
   depth.

   @returns (the requested RGB triplet in the specified color depth.) *)
  FUNCTION al_makecol_depth (color_depth, r, g, b: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makecol_depth';

(* Convert RGBA colors into display dependent pixel formats.  In anything less
   than a 32-bit mode, these are the same as calling @link (al_makecol) or
   @link (al_makecol_depth), but by using these routines it is possible to
   create 32-bit color values that contain a true 8 bit alpha channel along
   with the red, green, and blue components.  You should only use RGBA format
   colors as the input to @link (al_draw_trans_sprite) or @link
   (al_draw_trans_rle_sprite) after calling @link (al_set_alpha_blender),
   rather than drawing them directly to the screen.

   @returns (the requested RGBA quadruplet.) *)
  FUNCTION al_makeacol (r, g, b, a: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makeacol';
  FUNCTION al_makeacol_depth (color_depth, r, g, b, a: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makeacol_depth';



(* Given a color in the format being used by the current video mode, these
   functions extract one of the red, green, blue, or alpha components (ranging
   0-255), calling the preceding 8, 15, 16, 24, or 32-bit get functions as
   appropriate.  The alpha part is only meaningful for 32-bit pixels. *)
  FUNCTION al_getr (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getr';
  FUNCTION al_getg (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getg';
  FUNCTION al_getb (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getn';
  FUNCTION al_geta (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'geta';



(* Given a color in the format being used by the specified color depth, these
   functions extract one of the red, green, blue, or alpha components (ranging
   0-255). The alpha part is only meaningful for 32-bit pixels. *)
  FUNCTION al_getr_depth (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getr_depth';
  FUNCTION al_getg_depth (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getg_depth';
  FUNCTION al_getb_depth (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getn_depth';
  FUNCTION al_geta_depth (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'geta_depth';



(* Convert color values between the HSV and RGB color spaces.  The RGB values
   range from 0 to 255, hue is from 0 to 360, and saturation and value are from
   0 to 1. *)
  PROCEDURE al_hsv_to_rgb (h, s, v: DOUBLE; VAR r, g, b: LONGINT);
  PROCEDURE al_rgb_to_hsv (r, g, b: LONGINT; VAR h, s, v: DOUBLE);



IMPLEMENTATION

  PROCEDURE _hsv_to_rgb_ (h, s, v: DOUBLE; r, g, b: PLONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'hsv_to_rgb';
  PROCEDURE _rgb_to_hsv (r, g, b: LONGINT; h, s, v: PDOUBLE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rgb_to_hsv';



PROCEDURE al_hsv_to_rgb (h, s, v: DOUBLE; VAR r, g, b: LONGINT);
BEGIN
  _hsv_to_rgb_ (h, s, v, @r, @g, @b);
END;

PROCEDURE al_rgb_to_hsv (r, g, b: LONGINT; VAR h, s, v: DOUBLE);
BEGIN
  _rgb_to_hsv (r, g, b, @h, @s, @v);
END;



END.
