UNIT alsprrle;
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
 *	RLE sprites.
 *	by Ñuño Martínez <>
 *	Based on the file "gfx.h" of the Allegro library by Shawn Hargreaves.
 *      Some parts of the code was created by the "h2pas" utility.
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
  albase, albitmap;



TYPE
  AL_RLE_SPRITEptr = ^AL_RLE_SPRITE;
  AL_RLE_SPRITE = RECORD (* a RLE compressed sprite *)
    w, h: AL_INT;	 (* width and height in pixels *)
    color_depth: AL_INT; (* color depth of the image *)
    size: AL_INT;	 (* size of sprite data in bytes *)
    dat: AL_PTR;
  END;



(* Creates RLE sprites. *)
  FUNCTION al_get_rle_sprite (bitmap: AL_BITMAPptr): AL_RLE_SPRITEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_rle_sprite';
(* Frees the RLE sprite. *)
  PROCEDURE al_destroy_rle_sprite (sprite: AL_RLE_SPRITEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_rle_sprite';

(* Draw sprite. *)
  PROCEDURE al_draw_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
				x, y: AL_INT);
  PROCEDURE al_draw_trans_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
					x, y: AL_INT);
  PROCEDURE al_draw_lit_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
					x, y, color: AL_INT);


IMPLEMENTATION


PROCEDURE al_draw_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
				x, y: AL_INT);
BEGIN
  bmp^.vtable^.draw_rle_sprite (bmp, spr, x, y);
END;

PROCEDURE al_draw_trans_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
				x, y: AL_INT);
BEGIN
  IF spr^.color_depth = 32 THEN
    bmp^.vtable^.draw_trans_rgba_rle_sprite (bmp, spr, x, y)
  else
    bmp^.vtable^.draw_trans_rle_sprite (bmp, spr, x, y)
END;

PROCEDURE al_draw_lit_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
					x, y, color: AL_INT);
BEGIN
  bmp^.vtable^.draw_lit_rle_sprite (bmp, spr, x, y, color);
END;



END.

