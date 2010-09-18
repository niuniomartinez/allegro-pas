UNIT alpalete;
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
 *	Color palette manipulation.
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
  albase, alcolor;



CONST
  AL_PAL_SIZE = 256;



TYPE
(* Color palette description for indexed modes (8bpp). *)
  AL_PALETTEptr = ^AL_PALETTE;
  AL_PALETTE = ARRAY [0..AL_PAL_SIZE-1] OF AL_RGB;
(* Speed up reducing RGB values to 8-bit paletted colors. *)
  AL_RGB_MAPptr = ^AL_RGB_MAP;
  AL_RGB_MAP = RECORD
    data: ARRAY [0..31, 0..31, 0..31] OF AL_UCHAR;
  END;

  
VAR
(* Predefined palettes. *)
  al_black_palette, al_desktop_palette, al_default_palette: AL_PALETTEptr;
  


  PROCEDURE al_set_color (idx: AL_INT; p: AL_RGBptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_color';
  PROCEDURE al_set_palette (p: AL_PALETTE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_palette';
  PROCEDURE al_set_palette_range (p: AL_PALETTE; from, ato, retrace: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_palette_range';
  
  PROCEDURE al_get_color (idx: AL_INT; p: AL_RGBptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_color';
  PROCEDURE al_get_palette (p: AL_PALETTEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_palette';
  PROCEDURE al_get_palette_range (p: AL_PALETTEptr; from, ato, retrace: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_palette_range';

  PROCEDURE al_fade_interpolate (spource, dest: AL_PALETTE; aoutput: AL_PALETTEptr; apos, from, ato: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_interpolate';
  PROCEDURE al_fade_from_range (source, dest: AL_PALETTE; speed, from, ato: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_from_range';
  PROCEDURE al_fade_in_range (p: AL_PALETTE; speed, from, ato: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_in_range';
  PROCEDURE al_fade_out_range (speed, from, ato: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_out_range';
  PROCEDURE al_fade_from (source, dest: AL_PALETTE; speed: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_from';
  PROCEDURE al_fade_in (p: AL_PALETTE; speed: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_in';
  PROCEDURE al_fade_out (speed: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_out';

  PROCEDURE al_select_palette (p: AL_PALETTE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'select_palette';
  PROCEDURE al_unselect_palette; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'unselect_palette';

  PROCEDURE al_generate_332_palette (p: AL_PALETTEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'generate_332_palette';

  FUNCTION al_bestfit_color (pal: AL_PALETTE; r, g, b: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'bestfit_color';

  PROCEDURE al_set_rgb_map (map: AL_RGB_MAPptr); CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;

  PROCEDURE al_create_rgb_table (table: AL_RGB_MAPptr; pal: AL_PALETTE;
				 callback: AL_INT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_rgb_table';



IMPLEMENTATION

END.

