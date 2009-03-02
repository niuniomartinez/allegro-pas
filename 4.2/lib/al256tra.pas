UNIT al256tra;
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
 *	256 color transparency and lighting.
 *
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
  albase, alpalete;



TYPE
  AL_COLOR_MAPptr = ^AL_COLOR_MAP;
  AL_COLOR_MAP = ARRAY [0..AL_PAL_SIZE-1, 0..AL_PAL_SIZE-1] OF AL_UCHAR;

  AL_256_BLEND_PROC = AL_PTR;
  { Should be this definition:

  AL_256_BLEND_PROC = PROCEDURE (pal: AL_PALETTE; x, y: AL_INT; rgb: AL_RGBptr); CDECL;

    But it doesn't works! }



  PROCEDURE al_create_trans_table (table: AL_COLOR_MAPptr; pal: AL_PALETTE;
				r, g, b: AL_INT; callback: AL_INT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_trans_table';

  PROCEDURE al_create_light_table (table: AL_COLOR_MAPptr; pal: AL_PALETTE;
				r, g, b: AL_INT; callback: AL_INT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_light_table';

  PROCEDURE al_create_color_table (table: AL_COLOR_MAPptr; pal: AL_PALETTE;
		blend: AL_256_BLEND_PROC; callback: AL_INT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_color_table';

  PROCEDURE al_create_blender_table (table: AL_COLOR_MAPptr; pal: AL_PALETTE;
				callback: AL_INT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_blender_table';

  PROCEDURE al_set_color_map (color_map: AL_COLOR_MAPptr); CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  

IMPLEMENTATION

END.

