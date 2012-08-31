UNIT UnitBitmapInterface;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/

   Shows how to use Allegro.pas in a Lazarus GUI application.

   by Ñuño Martínez <niunio(at)users.sourceforge.net>

   Read the comment in the "example.lpr" file for a global description of the
   example.

   A helpful unit, it helps to integrate Allegro's AL_BITMAP structure and
   LCL's TCanvas.

   I'm sure there's a faster way to draw pixels in the canvas but I can't find
   it in the Lazarus' documentation.  If you find it, please send it me.
 *)

{$mode objfpc}{$H+}

INTERFACE

USES
  allegro, Graphics;

(* Draws the given bitmap in the given canvas. *)
  PROCEDURE BlitBitmap2Canvans (aBitmap: AL_BITMAPptr; aPalette: AL_PALETTE;
                                aCanvas: TCanvas);



IMPLEMENTATION

USES
  sysutils;



(* Help funcion for paletted bitmaps. *)
  PROCEDURE BlitPalettedBitmap (aBitmap: AL_BITMAPptr; aPalette: AL_PALETTE;
                                aCanvas: TCanvas);

  (* Helper function to get the actual RGB component.
     The 8bpp coponent is 0..63 while TColor component is 0..255. *)
    FUNCTION ValRGB (Value: INTEGER): INTEGER; INLINE;
    BEGIN
      RESULT := Value * 4;
    END;

  VAR
    Clr: TColor;
    PalValue: AL_RGB;
    PX, PY: INTEGER;
  BEGIN
  { It uses the slow way, accessing the Pixels property of aCanvas. }
    FOR PY := 0 TO (aBitmap^.h - 1) DO
      FOR PX := 0 TO (aBitmap^.w - 1) DO
      BEGIN
        PalValue := aPalette[al_getpixel (aBitmap, PX, PY)];
        Clr := ValRGB (PalValue.r) +
              (ValRGB (PalValue.g) SHL  8) +
              (ValRGB (PalValue.b) SHL 16);
        aCanvas.Pixels[PX, PY] := Clr;
      END;
  END;



(* Help funcion for high color and true color bitmaps.
   Actually it whould be able to use this procedure also for paletted bitmaps
   if the bitmap palette is set as default, but then this example will not be
   so complete, will it? ;) *)
  PROCEDURE BlitHighTrueBitmap (aBitmap: AL_BITMAPptr; aCanvas: TCanvas);
  VAR
    Clr: TColor;
    ColorDepth, ColorVal, PX, PY: INTEGER;
  BEGIN
    ColorDepth := al_bitmap_color_depth (aBitmap);
  { It uses the slow way, accessing the Pixels property of aCanvas. }
    FOR PY := 0 TO (aBitmap^.h - 1) DO
      FOR PX := 0 TO (aBitmap^.w - 1) DO
      BEGIN
        ColorVal := al_getpixel (aBitmap, PX, PY);
        Clr := al_getr_depth (ColorDepth, ColorVal) +
              (al_getg_depth (ColorDepth, ColorVal) SHL  8) +
              (al_getb_depth (ColorDepth, ColorVal) SHL 16);
        aCanvas.Pixels[PX, PY] := Clr;
      END;
  END;



(* Procedure that draws the given bitmap in the given canvas. *)
  PROCEDURE BlitBitmap2Canvans (aBitmap: AL_BITMAPptr; aPalette: AL_PALETTE;
                                aCanvas: TCanvas);
  BEGIN
  { Different color depths, different methods. }
    IF al_bitmap_color_depth (aBitmap) = 8 THEN
      BlitPalettedBitmap (aBitmap, aPalette, aCanvas)
    ELSE IF al_bitmap_color_depth (aBitmap) IN [15, 16, 24, 32] THEN
      BlitHighTrueBitmap (aBitmap, aCanvas)
    ELSE
      RAISE Exception.Create ('Unsupported Bitmap depth!');
  END;

END.

