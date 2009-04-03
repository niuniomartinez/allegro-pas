UNIT alpalete;
(*< All the Allegro drawing functions use integer parameters to represent
    colors.  In truecolor resolutions these numbers encode the color directly
    as a collection of red, green, and blue bits, but in a regular 256-color
    mode the values are treated as indexes into the current palette, which is a
    table listing the red, green and blue intensities for each of the 256
    possible colors.

    Palette entries are stored in an @link(AL_RGB) structure, which contains
    red, green and blue intensities in the VGA hardware format, ranging from
    0-63. *)

{$IFDEF FPC}
{ Free Pascal. }
 {$MODE FPC}
 {$PACKRECORDS C}
{$ELSE}
{ Assumes Codegear Delphi/Turbo. }
 {$A-}
{$ENDIF}
{$H+}



INTERFACE

USES
  albase, alcolor;



CONST
(* To know thet palette size. *)
  AL_PAL_SIZE = 256;



TYPE
(* Pointer to a @link(AL_PALETTE). *)
  AL_PALETTEptr = ^AL_PALETTE;
(* Color palette description for indexed modes (8bpp).  Remember that color
   components are 0-63. *)
  AL_PALETTE = ARRAY [0..AL_PAL_SIZE-1] OF AL_RGB;
(* Pointer to a @link(AL_RGB_MAP). *)
  AL_RGB_MAPptr = ^AL_RGB_MAP;
(* Speed up reducing RGB values to 8-bit paletted colors. *)
  AL_RGB_MAP = RECORD
    data: ARRAY [0..31, 0..31, 0..31] OF BYTE;
  END;



VAR
(* A palette containing solid black colors, used by the fade routines. *)
  al_black_palette: AL_PALETTE; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'black_palette';
(* The palette used by the Atari ST low resolution desktop. I'm not quite sure
   why this is still here, except that the original grabber and test programs
   use it.  It is probably the only Atari legacy code left in Allegro, and it
   would be a shame to remove it :-)

   The contents of this palette are 16 colors repeated 16 times.  Color entry
   zero is equal to color entry 16, which is equal to color entry 24, etc.
@table(
  @rowhead(@cell(Index) @cell(Color) @cell(RGB values) )
  @row(@cell( 0) @cell(White       ) @cell(@code(63  63  63)))
  @row(@cell( 1) @cell(Red         ) @cell(@code(63   0   0)))
  @row(@cell( 2) @cell(Green       ) @cell(@code( 0  63   0)))
  @row(@cell( 3) @cell(Yellow      ) @cell(@code(63  63   0)))
  @row(@cell( 4) @cell(Blue        ) @cell(@code( 0   0  63)))
  @row(@cell( 5) @cell(Pink        ) @cell(@code(63   0  63)))
  @row(@cell( 6) @cell(Cyan        ) @cell(@code( 0  63  63)))
  @row(@cell( 7) @cell(Grey        ) @cell(@code(16  16  16)))
  @row(@cell( 8) @cell(Light grey  ) @cell(@code(31  31  31)))
  @row(@cell( 9) @cell(Light red   ) @cell(@code(63  31  31)))
  @row(@cell(10) @cell(Light green ) @cell(@code(31  63  31)))
  @row(@cell(11) @cell(Light yellow) @cell(@code(63  63  31)))
  @row(@cell(12) @cell(Light blue  ) @cell(@code(31  31  63)))
  @row(@cell(13) @cell(Light pink  ) @cell(@code(63  31  63)))
  @row(@cell(14) @cell(Light cyan  ) @cell(@code(31  63  63)))
  @row(@cell(15) @cell(Black       ) @cell(@code( 0   0   0)))
) *)
  al_desktop_palette: AL_PALETTE; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'desktop_palette';
(* The default IBM BIOS palette.  This will be automatically selected whenever
   you set a new graphics mode.  The palette contains 16 basic colors plus many
   gradients between them.  If you want to see the values, you can write a
   small Allegro program which saves a screenshot with this palette, or open
   the grabber tool provided with Allegro and create a new palette object,
   which will use this palette by default. *)
  al_default_palette: AL_PALETTE; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'default_palette';

(* To speed up reducing RGB values to 8-bit paletted colors, Allegro uses a 32k
   lookup table (5 bits for each color component).  You must set up this table
   before using the gouraud shading routines, and if present the table will
   also vastly accelerate the @link(al_makecol) and some
   @code(al_create_*_table) functions on 8-bit graphic mode.  RGB tables can be
   precalculated with the rgbmap utility, or generated at runtime with
   @link(al_create_rgb_table). *)
  al_rgb_table: AL_RGB_MAPptr; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rgb_map';



(* Sets the specified palette entry to the specified @link(AL_RGB) triplet.
   Unlike the other palette functions this doesn't do any retrace
   synchronisation, so you should call @link(al_vsync) before it to prevent
   snow problems. *)
  PROCEDURE al_set_color (idx: LONGINT; p: AL_RGBptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_color';

(* Sets the entire palette of 256 colors.  You should provide an array of 256
   RGB structures.  Unlike @link(al_set_color), there is no need to call
   @link(al_vsync) before this function. *)
  PROCEDURE al_set_palette (p: AL_PALETTE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_palette';

(* Sets the palette entries between @code(from) and @code(ato) (inclusive:
   pass 0 and 255 to set the entire palette).  If @code(al_vsync) is not zero it
   waits for the vertical retrace, otherwise it sets the colors immediately. *)
  PROCEDURE al_set_palette_range (p: AL_PALETTE; from, ato, vsync: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_palette_range';

(* Retrieves the specified palette entry. *)
  PROCEDURE al_get_color (idx: LONGINT; p: AL_RGBptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_color';

(* Retrieves the entire palette of 256 colors.  You should provide a
   @link(AL_PALETTE) to store it in. *)
  PROCEDURE al_get_palette (p: AL_PALETTEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_palette';

(* Retrieves the palette entries between @code(from) and @code(ato)
   (inclusive: pass 0 and 255 to set the entire palette). *)
  PROCEDURE al_get_palette_range (p: AL_PALETTEptr; from, ato: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_palette_range';



(* Calculates a temporary palette part way between @code(source) and
   @code(dest), returning it in the @code(aoutput) parameter.  The position between
   the two extremes is specified by the pos value: 0 returns an exact copy of
   source, 64 returns dest, 32 returns a palette half way between the two, etc.
   This routine only affects colors between @code(from) and @code(ato)
   (inclusive: pass 0 and 255 to interpolate the entire palette). *)
  PROCEDURE al_fade_interpolate (source, dest: AL_PALETTE; aoutput: AL_PALETTEptr; apos, from, ato: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_interpolate';

(* Gradually fades a part of the palette from the @code(source) palette to the
   @code(dest) palette.  The @code(speed) is from 1 (the slowest) up to 64
   (instantaneous).  This routine only affects colors between @code(from) and
   @code(ato) (inclusive: pass 0 and 255 to fade the entire palette).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. *)
  PROCEDURE al_fade_from_range (source, dest: AL_PALETTE; speed, from, ato: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_from_range';

(* Gradually fades a part of the palette from a black screen to the specified
   palette.  The @code(speed) is from 1 (the slowest) up to 64
   (instantaneous).  This routine only affects colors between @code(from) and
   @code(ato) (inclusive: pass 0 and 255 to fade the entire palette).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. *)
  PROCEDURE al_fade_in_range (p: AL_PALETTE; speed, from, ato: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_in_range';

(* Gradually fades a part of the current palette to the @code(dest) palette.
   The @code(speed) is from 1 (the slowest) up to 64 (instantaneous).  This
   routine only affects colors between @code(from) and @code(ato) (inclusive:
   pass 0 and 255 to fade the entire palette).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. *)
  PROCEDURE al_fade_out_range (speed, from, ato: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_out_range';

(* Gradually fades from the @code(source) palette to the @code(dest) palette.
   The @code(speed) is from 1 (the slowest) up to 64 (instantaneous).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. *)
  PROCEDURE al_fade_from (source, dest: AL_PALETTE; speed: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_from';


(* Gradually fades from a black screen to the specified palette.  The
   @code(speed) is from 1 (the slowest) up to 64 (instantaneous).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. *)
  PROCEDURE al_fade_in (p: AL_PALETTE; speed: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_in';

(* Gradually fades from the current palette to a black screen.  The
   @code(speed) is from 1 (the slowest) up to 64 (instantaneous).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. *)
  PROCEDURE al_fade_out (speed: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_out';



(* Ugly hack for use in various dodgy situations where you need to convert
   between paletted and truecolor image formats.  Sets the internal palette
   table in the same way as the @link(al_set_palette) function, so the
   conversion will use the specified palette, but without affecting the
   display hardware in any way.  The previous palette settings are stored in an
   internal buffer, and can be restored by calling @link(al_unselect_palette).
   If you call @code(al_select_palette) again, however, the internal buffer
   will be overwritten. *)
  PROCEDURE al_select_palette (p: AL_PALETTE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'select_palette';

(* Restores the palette tables that were in use before the last call to
   @link(al_select_palette). *)
  PROCEDURE al_unselect_palette; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'unselect_palette';


(* Constructs a fake truecolor palette, using three bits for red and green and
   two for the blue.  The @link(al_load_bitmap) function fills the palette
   parameter with this if the file does not contain a palette itself (ie. you
   are reading a truecolor bitmap). *)
  PROCEDURE al_generate_332_palette (p: AL_PALETTEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'generate_332_palette';



(* Searches the specified palette for the closest match to the requested color,
   which are specified in the VGA hardware 0-63 format.  Normally you should
   call @link(al_makecol_depth) instead, but this lower level function may be
   useful if you need to use a palette other than the currently selected one,
   or specifically don't want to use the @link(al_rgb_map) lookup table.

   @returns(the index of the palette for the closest match to the requested
     color.) *)
  FUNCTION al_bestfit_color (pal: AL_PALETTE; r, g, b: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'bestfit_color';

(* Fills the specified RGB mapping table with lookup data for the specified
   palette.  If the @code(callback) function is not @nil, it will be called
   256 times during the calculation, allowing you to display a progress
   indicator. *)
  PROCEDURE al_create_rgb_table (table: AL_RGB_MAPptr; pal: AL_PALETTE;
				 callback: AL_INT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_rgb_table';



IMPLEMENTATION

END.
