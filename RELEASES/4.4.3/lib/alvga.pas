UNIT alvga;
(*<256-color special effects.

  In paletted video modes, translucency and lighting are implemented with a 64k
  lookup table, which contains the result of combining any two colors c1 and
  c2.  You must set up this table before you use any of the translucency or
  lighting routines.  Depending on how you construct the table, a range of
  different effects are possible.  For example, translucency can be implemented
  by using a color halfway between c1 and c2 as the result of the combination.
  Lighting is achieved by treating one of the colors as a light level (0-255)
  rather than a color, and setting up the table appropriately.  A range of
  specialised effects are possible, for instance replacing any color with any
  other color and making individual source or destination colors completely
  solid or invisible.  Color mapping tables can be precalculated with the
  colormap utility, or generated at runtime. *)

{$INCLUDE allegro.cfg }

INTERFACE

USES
  albase, allegro;



TYPE
(* Pointer to @code(AL_COLOR_MAP). *)
  AL_COLOR_MAPptr = ^AL_COLOR_MAP;
(* Clolor map. *)
  AL_COLOR_MAP = ARRAY [0..AL_PAL_SIZE-1, 0..AL_PAL_SIZE-1] OF BYTE;
(* Call-back procedure to be ussed to create color tables using
   @code(al_create_color_table).
   This procedure must have the form:
   @longcode(#
     PROCEDURE (pal: AL_PALETTE; x, y: LONGINT; rgb: AL_RGBptr); CDECL;
   #) *)
  AL_256_BLEND_PROC = POINTER;



(* Fills the specified color mapping table with lookup data for doing
   translucency effects with the specified palette.  When combining the colors
   c1 and c2 with this table, the result will be a color somewhere between the
   two.  The r, g, and b parameters specify the solidity of each color
   component, ranging from 0 (totally transparent) to 255 (totally solid).  For
   50% solidity, pass 128.

   This function treats source color #0 as a special case, leaving the
   destination unchanged whenever a zero source pixel is encountered, so that
   masked sprites will draw correctly.  This function will take advantage of
   the global @code(al_rgb_map) variable to speed up color conversions.  If the
   callback function is not @nil, it will be called 256 times during the
   calculation, allowing you to display a progress indicator. *)
  PROCEDURE al_create_trans_table (table: AL_COLOR_MAPptr; pal: AL_PALETTE;
				r, g, b: LONGINT; callback: AL_INT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_trans_table';

(* Fills the specified color mapping table with lookup data for doing lighting
   effects with the specified palette.  When combining the colors c1 and c2
   with this table, c1 is treated as a light level from 0-255.  At light level
   255 the table will output color c2 unchanged, at light level 0 it will
   output the r, g, b value you specify to this function, and at intermediate
   light levels it will output a color somewhere between the two extremes.  The
   r, g, and b values are in the range 0-63.

   This function will take advantage of the global @code(al_rgb_map) variable to
   speed up color conversions.  If the callback function is not @nil, it will
   be called 256 times during the calculation, allowing you to display a
   progress indicator. *)
  PROCEDURE al_create_light_table (table: AL_COLOR_MAPptr; pal: AL_PALETTE;
				r, g, b: LONGINT; callback: AL_INT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_light_table';

(* Fills the specified color mapping table with lookup data for doing
   customised effects with the specified palette, calling the blend function to
   determine the results of each color combination.

   Your blend routine will be passed a pointer to the palette and the two
   indices of the colors which are to be combined, and should fill in the RGB
   structure with the desired result in 0-63 format.  Allegro will then search
   the palette for the closest match to the RGB color that you requested, so it
   doesn't matter if the palette has no exact match for this color.

   If the callback function is not @nil, it will be called 256 times during the
   calculation, allowing you to display a progress indicator. *)
  PROCEDURE al_create_color_table (table: AL_COLOR_MAPptr; pal: AL_PALETTE;
		blend: AL_256_BLEND_PROC; callback: AL_INT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_color_table';

(* Fills the specified color mapping table with lookup data for doing a
   paletted equivalent of whatever truecolor blender mode is currently
   selected.  After calling @code(al_set_trans_blender),
   @code(al_set_blender_mode), or any of the other truecolor blender mode
   routines, you can use this function to create an 8-bit mapping table that
   will have the same results as whatever 24-bit blending mode you have
   enabled.

   If the callback function is not @nil, it will be called 256 times during the
   calculation, allowing you to display a progress indicator. *)
  PROCEDURE al_create_blender_table (table: AL_COLOR_MAPptr; pal: AL_PALETTE;
				callback: AL_INT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_blender_table';

VAR
(* Pointer to the color mapping table.  You must allocate your own
   @code(AL_COLOR_MAP) either statically or dynamically and set
   @code(al_color_table) to it before using any translucent or lit drawing
   functions in a 256-color video mode! *)
  al_color_table: AL_COLOR_MAPptr; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'color_map';

IMPLEMENTATION

END.
