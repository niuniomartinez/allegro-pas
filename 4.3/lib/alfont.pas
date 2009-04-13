UNIT alfont;
(*< Allegro provides routines for loading fonts directly from GRX format .fnt
    files, 8x8 or 8x16 BIOS format .fnt files, from bitmap images, from
    datafiles or you can import a multiple-range Unicode font by writing a .txt
    script that specifies a number of different source files for each range of
    characters.

    By default, Allegro can only use bitmapped (non-scalable) fonts.  If you
    want to use TrueType fonts, you will need to use an add-on library which
    allows you to load them on the fly (like AllegTTF or Glyph Keeper, listed
    among others at http://www.allegro.cc/) and render them directly, or
    generate a bitmapped version of a TrueType font with tools like TTF2PCX
    (http://www.talula.demon.co.uk/ttf2pcx/index.html).  *)

{$IFDEF FPC}
{ Free Pascal. }
 {$LONGSTRINGS ON}
{$ENDIF}

INTERFACE

USES
  albase, albitmap, alpalete; { Needs some basic definitions. }



TYPE
(* A pointer to a structure holding an Allegro font, usually created beforehand
   with the grabber tool or Allegro's default font.  Read introduction of
   @code(alfont) unit for a description on how to load/destroy fonts, and unit
   @code(altext) for a description on how to show text. *)
  AL_FONTptr = POINTER;



(* Loads a font from a file.  At present, this supports loading fonts from a
   GRX format .fnt file, a 8x8 or 8x16 BIOS format .fnt file, a datafile or any
   bitmap format that can be loaded by @link(al_load_bitmap).

   If the font contains palette information, then the palette is returned in
   the second parameter, which should be an array of 256 @link(AL_RGB)
   structures (a @link(AL_PALETTE)).  The @code(pal) argument may be @nil.
   In this case, the palette data, if present, is simply not returned.

   Note that another way of loading fonts is embedding them into a datafile and
   using the datafile related functions.

   @returns(a pointer to the font or @nil on error.  Remember that you are
     responsible for destroying the font when you are finished with it to avoid
     memory leaks.) *)
  FUNCTION al_load_font (filename: STRING; palette: AL_PALETTEptr; p: POINTER)
	: AL_FONTptr;

(* Tries to grab a font from a bitmap.  The bitmap can be in any format that
   @link(al_load_bitmap) understands.

   The size of each character is determined by the layout of the image, which
   should be a rectangular grid containing all the ASCII characters from space
   (32) up to the tilde (126).  The way the characters are separated depends on
   the color depth of the image file:
   @unorderedList(
     @item(paletted @(8 bit@) image file Use color 0 for the transparent
       portions of the characters and fill the spaces between each letter with
       color 255.)
     @item(High @(15/16 bit@) and true @(24/32 bit@) color image file use
       bright pink @(maximum red and blue, zero green@) for the transparent
       portions of the characters and fill the spaces between each letter with
       bright yellow @(maximum red and green, zero blue@).)
   )
   Note that in each horizontal row the bounding boxes around the characters
   should align and have the same height.

   Probably the easiest way to get to grips with how this works is to load up
   the `demo.dat' file and export the TITLE_FONT into a PCX file.  Have a look
   at the resulting picture in your paint program:  that is the format a font
   should be in.

   Take care with high and true color fonts:  Allegro will convert these to the
   current color depth when you load the font.  If you try to use a font on a
   bitmap with a different color depth Allegro will do color conversions on the
   fly, which will be rather slow.  For optimal performance you should set the
   color depth to the color depth you want to use before loading any fonts.

   @returns(a pointer to the font or @nil on error.  Remember that you are
     responsible for destroying the font when you are finished with it to avoid
     memory leaks.) *)
  FUNCTION al_load_bitmap_font (filename: STRING; palette: AL_PALETTEptr;
    p: POINTER): POINTER;

(* This function is the work-horse of @link(al_load_bitmap_font), and can be
   used to grab a font from a bitmap in memory.  You can use this if you want
   to generate or modify a font at runtime.  The bitmap should follow the
   layout described for @link(al_load_bitmap_font).

   @returns(a pointer to the font or @nil on error.  Remember that you are
     responsible for destroying the font when you are finished with it to avoid
     memory leaks.) *)
  FUNCTION al_grab_font_from_bitmap (bmp: AL_BITMAPptr): AL_FONTptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'grab_font_from_bitmap';

(* This function checks if the given font is a color font, as opposed to a
   monochrome font.
   @returns(@true if the font is a color font, @false if it is not.) *)
  FUNCTION al_is_color_font (f: AL_FONTptr): BOOLEAN;

(* This function checks if the given font is a mono font, as opposed to a
   color font.
   @returns(@true if the font is a monochrome font, @false if it is not.) *)
  FUNCTION al_is_mono_font (f: AL_FONTptr): BOOLEAN;

(* This function compares the two fonts, which you can use to find out if
   Allegro is capable of merging them.

   @returns(@true if the two fonts are of the same general type @(both are
     color fonts or both are monochrome fonts, for instance@).) *)
  FUNCTION al_is_compatible_font (f1, f2: AL_FONTptr): BOOLEAN;

(* Frees the memory being used by a font structure.  Don't use this on the
   default global Allegro font or any text routines using it could crash.  You
   should use this only on fonts you have loaded manually after you are done
   with them, to prevent memory leaks in your program. *)
  PROCEDURE al_destroy_font (f: AL_FONTptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_font';



IMPLEMENTATION

  USES
    alcolor;



  FUNCTION load_font (filename: PCHAR; palette: AL_PALETTEptr; p: POINTER)
	   : AL_FONTptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_load_font (filename: STRING; palette: AL_PALETTEptr; p: POINTER)
	: AL_FONTptr;
  BEGIN
    al_load_font := load_font (PCHAR (filename), palette, p);
  END;



  FUNCTION load_bitmap_font (filename: PCHAR; palette: AL_PALETTEptr;
	p: POINTER): POINTER; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_load_bitmap_font (filename: STRING; palette: AL_PALETTEptr;
    p: POINTER): POINTER;
  BEGIN
    al_load_bitmap_font := load_bitmap_font (PCHAR (filename), palette, p);
  END;



  FUNCTION is_color_font (f: AL_FONTptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_is_color_font (f: AL_FONTptr): BOOLEAN;
  BEGIN
    al_is_color_font := is_color_font (f) <> 0;
  END;



  FUNCTION is_mono_font (f: AL_FONTptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_is_mono_font (f: AL_FONTptr): BOOLEAN;
  BEGIN
    al_is_mono_font := is_mono_font (f) <> 0;
  END;



  FUNCTION is_compatible_font (f1, f2: AL_FONTptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_is_compatible_font (f1, f2: AL_FONTptr): BOOLEAN;
  BEGIN
    al_is_compatible_font := is_compatible_font (f1, f2) <> 0;
  END;



END.
