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
  albase, albitmap, alpalete; { Needs some basic definitions. }



TYPE
(* A pointer to a structure holding an Allegro font, usually created beforehand
   with the grabber tool or Allegro's default font.  Read introduction of @code
   (alfont) unit for a description on how to load/destroy fonts, and unit @code
   (altext) for a description on how to show text. *)
  AL_FONTptr = POINTER;



(* Loads a font from a file.  At present, this supports loading fonts from a
   GRX format .fnt file, a 8x8 or 8x16 BIOS format .fnt file, a datafile or any
   bitmap format that can be loaded by @link (al_load_bitmap).

   If the font contains palette information, then the palette is returned in
   the second parameter, which should be an array of 256 @link (AL_RGB)
   structures (a @link (AL_PALETTE)).  The @code (pal) argument may be @nil.
   In this case, the palette data, if present, is simply not returned.

   Note that another way of loading fonts is embedding them into a datafile and
   using the datafile related functions.

   @returns (a pointer to the font or @nil on error.  Remember that you are
     responsible for destroying the font when you are finished with it to avoid
     memory leaks.) *)
  FUNCTION al_load_font (filename: STRING; palette: AL_PALETTEptr; p: AL_PTR)
	: AL_FONTptr;

  FUNCTION al_grab_font_from_bitmap (bmp: AL_BITMAPptr): AL_FONTptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'grab_font_from_bitmap';

(* This function checks if the given font is a color font, as opposed to a
   monochrome font.
   @returns (@true if the font is a color font, @false if it is not.) *)
  FUNCTION al_is_color_font (f: AL_FONTptr): BOOLEAN;

(* This function checks if the given font is a mono font, as opposed to a
   color font.
   @returns (@true if the font is a monochrome font, @false if it is not.) *)
  FUNCTION al_is_mono_font (f: AL_FONTptr): BOOLEAN;

(* This function compares the two fonts, which you can use to find out if
   Allegro is capable of merging them.

   @returns (@true if the two fonts are of the same general type (both are
     color fonts or both are monochrome fonts, for instance).) *)
  FUNCTION al_is_compatible_font (f1, f2: AL_FONTptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'is_compatible_font';

(* Frees the memory being used by a font structure.  Don't use this on the
   default global Allegro font or any text routines using it could crash.  You
   should use this only on fonts you have loaded manually after you are done
   with them, to prevent memory leaks in your program. *)
  PROCEDURE al_destroy_font (f: AL_FONTptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_font';



IMPLEMENTATION

  USES
    alcolor;



  FUNCTION load_font (filename: PCHAR; palette: AL_PALETTEptr; p: AL_PTR)
	   : AL_FONTptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_load_font (filename: STRING; palette: AL_PALETTEptr; p: AL_PTR)
	: AL_FONTptr;
  BEGIN
    al_load_font := load_font (PCHAR (filename), palette, p);
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
