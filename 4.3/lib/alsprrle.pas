UNIT alsprrle;
(*< Run-length compressed sprites.

  Because bitmaps can be used in so many different ways, the bitmap structure
  is quite complicated, and it contains a lot of data.  In many situations,
  though, you will find yourself storing images that are only ever copied to
  the screen, rather than being drawn onto or used as filling patterns, etc.
  If this is the case you may be better off storing your images in
  @link(AL_RLE_SPRITE) or @link (AL_COMPILED_SPRITE) structures rather than
  regular bitmaps.

  RLE sprites store the image in a simple run-length encoded format, where
  repeated zero pixels are replaced by a single length count, and strings of
  non-zero pixels are preceded by a counter giving the length of the solid run.
  RLE sprites are usually much smaller than normal bitmaps, both because of the
  run length compression, and because they avoid most of the overhead of the
  bitmap structure.  They are often also faster than normal bitmaps, because
  rather than having to compare every single pixel with zero to determine
  whether it should be drawn, it is possible to skip over a whole run of zeros
  with a single add, or to copy a long run of non-zero pixels with fast string
  instructions.

  Every silver lining has a cloud, though, and in the case of RLE sprites it is
  a lack of flexibility.  You can't draw onto them, and you can't flip them,
  rotate them, or stretch them.  In fact the only thing you can do with them is
  to blast them onto a bitmap with the @link(al_draw_rle_sprite) function,
  which is equivalent to using @link(al_draw_sprite) with a regular bitmap.
  You can convert bitmaps into RLE sprites at runtime, or you can create RLE
  sprite structures in grabber datafiles by making a new object of type 'RLE
  sprite'.  *)

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
  albase, albitmap;



TYPE
(* Ponter to @link(AL_RLE_SPRITE). *)
  AL_RLE_SPRITEptr = ^AL_RLE_SPRITE;
(* An RLE compressed sprite. @seealso(al_get_rle_sprite) *)
  AL_RLE_SPRITE = RECORD
    w, h: LONGINT;	 (*< width and height in pixels *)
    color_depth: LONGINT; (*< color depth of the image *)
    size: LONGINT;	 (*< size of sprite data in bytes *)
    dat: POINTER;
  END;



(* Creates an RLE sprite based on the specified bitmap (which must be a memory
   bitmap).  Remember to free this RLE sprite later to avoid memory leaks.
   @param(bitmap Pointer to the @link(albitmap bitmap) used to create the
     sprite.)
   @returns(A pointer to the created RLE sprite, or @nil if it could not be
     created.  Remember to free this RLE sprite later to avoid memory
     leaks.)
   @seealso(al_destroy_rle_sprite) @seealso(al_draw_rle_sprite) *)
  FUNCTION al_get_rle_sprite (bitmap: AL_BITMAPptr): AL_RLE_SPRITEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_rle_sprite';

(* Destroys an RLE sprite structure previously returned by
   @link(al_get_rle_sprite).  If you pass a @nil pointer this function won't do
   anything.  Use this once you are done with an RLE sprite to avoid memory
   leaks in your program. 
   @param(sprite The RLE sprite to destroy.) *)
  PROCEDURE al_destroy_rle_sprite (sprite: AL_RLE_SPRITEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_rle_sprite';

(* Draws an RLE sprite onto a bitmap at the specified position.
   @param(bmp Bitmap where the sprite will be draw.)
   @param(spr Sprite to draw.)
   @param(x Horizontal coordinate.) @param(y Vertical coordinate.)
   @seealso(al_draw_sprite) *)
  PROCEDURE al_draw_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
				x, y: LONGINT);

(* Translucent version of @link(al_draw_rle_sprite).  This must only be used
   after you have set up the color mapping table (for 256-color modes) or
   blender functions (for truecolor modes).  The bitmap and sprite must
   normally be in the same color depth, but as a special case you can draw
   32-bit RGBA format sprites onto any hicolor or truecolor bitmap, as long as
   you call @link(al_set_alpha_blender) first.
   @param(bmp Bitmap where the sprite will be draw.)
   @param(spr Sprite to draw.)
   @param(x Horizontal coordinate.) @param(y Vertical coordinate.)
   @seealso(al_draw_rle_sprite) @seealso(al_color_map)
   @seealso(al_set_trans_blender) *)
  PROCEDURE al_draw_trans_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
					x, y: LONGINT);

(* Tinted version of @link(al_draw_rle_sprite).  This must only be used after
   you have set up the color mapping table (for 256-color modes) or blender
   functions (for truecolor modes).
   @param(bmp Bitmap where the sprite will be draw.)
   @param(spr Sprite to draw.)
   @param(x Horizontal coordinate.) @param(y Vertical coordinate.)
   @param(color Tint color.)
   @seealso(al_draw_rle_sprite) @seealso(al_color_map) *)
  PROCEDURE al_draw_lit_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
					x, y, color: LONGINT);


IMPLEMENTATION


PROCEDURE al_draw_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
				x, y: LONGINT);
BEGIN
  bmp^.vtable^.draw_rle_sprite (bmp, spr, x, y);
END;

PROCEDURE al_draw_trans_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
				x, y: LONGINT);
BEGIN
  IF spr^.color_depth = 32 THEN
    bmp^.vtable^.draw_trans_rgba_rle_sprite (bmp, spr, x, y)
  else
    bmp^.vtable^.draw_trans_rle_sprite (bmp, spr, x, y)
END;

PROCEDURE al_draw_lit_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
					x, y, color: LONGINT);
BEGIN
  bmp^.vtable^.draw_lit_rle_sprite (bmp, spr, x, y, color);
END;



END.

