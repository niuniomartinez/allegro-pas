UNIT altrutra;
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
 *	true color transparency and lighting.
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
  albase;



TYPE
  AL_BLENDER_FUNC = FUNCTION (x, y, n: AL_ULONG): AL_ULONG; CDECL;



  PROCEDURE al_set_alpha_blender; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_alpha_blender';
  PROCEDURE al_set_write_alpha_blender; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_write_alpha_blender';
  PROCEDURE al_set_trans_blender (r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_trans_blender';
  PROCEDURE al_set_add_blender (r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_add_blender';
  PROCEDURE al_set_burn_blender (r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_burn_blender';
  PROCEDURE al_set_color_blender (r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_color_blender';
  PROCEDURE al_set_difference_blender (r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_difference_blender';
  PROCEDURE al_set_dissolve_blender (r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_dissolve_blender';
  PROCEDURE al_set_dodge_blender (r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_dodge_blender';
  PROCEDURE al_set_hue_blender (r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_hue_blender';
  PROCEDURE al_set_invert_blender (r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_invert_blender';
  PROCEDURE al_set_luminance_blender (r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_luminance_blender';
  PROCEDURE al_set_multiply_blender (r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_multiply_blender';
  PROCEDURE al_set_saturation_blender (r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_saturation_blender';
  PROCEDURE al_set_screen_blender (r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_screen_blender';

  PROCEDURE al_set_blender_mode (b15, b16, b24: AL_BLENDER_FUNC;
				 r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_blender_mode';
  PROCEDURE al_set_blender_mode_ex (b15, b16, b24, b32, b15x, b16x, b24x:
				    AL_BLENDER_FUNC; r, g, b, a: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_blender_mode_ex';



IMPLEMENTATION

END.

