UNIT albltspr;
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
 *	Blitblt bitmap copy and sprites.
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
  albase, albitmap, alfixed; { Needs some basic definitions. }



(* Blitting. *)
  PROCEDURE al_blit (source, dest: AL_BITMAPptr; source_x, source_y, dest_x, dest_y, width, height: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'blit';
  PROCEDURE al_stretch_blit (source, dest: AL_BITMAPptr; source_x, source_y, source_width, source_height, dest_x, dest_y, dest_width, dest_height: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stretch_blit';
  PROCEDURE al_masked_blit (source, dest: AL_BITMAPptr; source_x, source_y, dest_x, dest_y, width, height: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'masked_blit';
  PROCEDURE al_masked_stretch_blit (source, dest: AL_BITMAPptr; source_x, source_y, source_width, source_height, dest_x, dest_y, dest_width, dest_height: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'masked_stretch_blit';



(* Sprites. *)
  PROCEDURE al_draw_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
  PROCEDURE al_stretch_sprite (bmp, sprite: AL_BITMAPptr; x, y, w, h: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stretch_sprite';

  PROCEDURE al_draw_sprite_h_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
  PROCEDURE al_draw_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
  PROCEDURE al_draw_sprite_vh_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
  PROCEDURE al_draw_trans_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
  PROCEDURE al_draw_lit_sprite (bmp, sprite: AL_BITMAPptr; x, y, c: AL_INT);
  
  PROCEDURE al_rotate_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle: AL_FIXED);
  PROCEDURE al_rotate_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle: AL_FIXED);
  
  PROCEDURE al_rotate_scaled_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle, scale: AL_FIXED);
  PROCEDURE al_rotate_scaled_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle, scale: AL_FIXED);

  PROCEDURE al_pivot_sprite (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle: AL_FIXED);
  PROCEDURE al_pivot_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle: AL_FIXED);
  
  PROCEDURE al_pivot_scaled_sprite (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle, scale: AL_FIXED);
  PROCEDURE al_pivot_scaled_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle, scale: AL_FIXED);



IMPLEMENTATION

(* Sprites. *)
PROCEDURE al_draw_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
BEGIN
  IF sprite^.vtable^.color_depth = 8 THEN
    bmp^.vtable^.draw_256_sprite (bmp, sprite, x, y)
  ELSE
    bmp^.vtable^.draw_sprite (bmp, sprite, x, y);
END;



PROCEDURE al_draw_sprite_h_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
BEGIN
  bmp^.vtable^.draw_sprite_h_flip (bmp, sprite, x, y);
END;



PROCEDURE al_draw_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
BEGIN
  bmp^.vtable^.draw_sprite_v_flip (bmp, sprite, x, y);
END;

PROCEDURE al_draw_sprite_vh_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
BEGIN
  bmp^.vtable^.draw_sprite_vh_flip (bmp, sprite, x, y);
END;



PROCEDURE al_draw_trans_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
BEGIN
  IF sprite^.vtable^.color_depth = 32 THEN
    bmp^.vtable^.draw_trans_rgba_sprite (bmp, sprite, x, y)
  ELSE
    bmp^.vtable^.draw_trans_sprite (bmp, sprite, x, y);
END;



PROCEDURE al_draw_lit_sprite (bmp, sprite: AL_BITMAPptr; x, y, c: AL_INT);
BEGIN
  bmp^.vtable^.draw_lit_sprite (bmp, sprite, x, y, c);
END;


  
PROCEDURE al_rotate_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, (x SHL 16) + (sprite^.w * $10000) DIV 2,
						      (y SHL 16) + (sprite^.h * $10000) DIV 2,
						      sprite^.w SHL 15, sprite^.h SHL 15,
						      angle, $10000, 0);
END;



PROCEDURE al_rotate_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, (x SHL 16) + (sprite^.w * $10000) DIV 2,
						      (y SHL 16) + (sprite^.h * $10000) DIV 2,
						      sprite^.w SHL 15, sprite^.h SHL 15,
						      angle, $10000, NOT 0);
END;



PROCEDURE al_rotate_scaled_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle, scale: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, (x SHL 16) + (sprite^.w * scale) DIV 2,
						      (y SHL 16) + (sprite^.h * scale) DIV 2,
						      sprite^.w SHL 15, sprite^.h SHL 15,
						      angle, scale, 0);
END;



PROCEDURE al_rotate_scaled_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle, scale: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, (x SHL 16) + (sprite^.w * scale) DIV 2,
						      (y SHL 16) + (sprite^.h * scale) DIV 2,
						      sprite^.w SHL 15, sprite^.h SHL 15,
						      angle, scale, NOT 0);
END;



PROCEDURE al_pivot_sprite (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, x SHL 16, y SHL 16,
						      cx SHL 16, cy SHL 16,
						      angle, $10000, 0);
END;



PROCEDURE al_pivot_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, x SHL 16, y SHL 16,
						      cx SHL 16, cy SHL 16,
						      angle, $10000, NOT 0);
END;



PROCEDURE al_pivot_scaled_sprite (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle, scale: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, x SHL 16, y SHL 16,
						      cx SHL 16, cy SHL 16,
						      angle, scale, 0);
END;



PROCEDURE al_pivot_scaled_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle, scale: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, x SHL 16, y SHL 16,
						      cx SHL 16, cy SHL 16,
						      angle, scale, NOT 0);
END;



END.

