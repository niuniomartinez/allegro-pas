UNIT albltspr;
(*<As far as Allegro is concerned, a bitmap and a sprite are the same thing,
   but to many people the two words imply slightly different things.  The
   function @link (al_draw_sprite) is called so rather than @code
   (al_draw_bitmap) partly because it indicates that it uses a masked drawing
   mode (if it existed, you could expect @code (al_draw_bitmap) to be a simple
   block copy), and partly for historical reasons.  In Allegro 1.0 there were
   actually different structures for sprites and bitmaps, each with their own
   set of abilities.  Allegro 2.0 merged these into a single more flexible
   structure, but retained some names like @code (draw_sprite).

   In wider (non-Allegro) terms, the two words can mean quite different things.
   Generally you can say that sprites are a subset of bitmaps, but even that
   isn't true in 100% of cases.

   @bold (BITMAP:) a widely accepted term that will be understood by anyone
   even remotely connected with computer graphics.  It simply means an image
   built up from a grid of pixels, ie. just about any picture that you are
   likely to come across on a computer (vector graphics formats are the
   exception, but those must be rendered into a bitmap format before they can
   be displayed by most hardware).  A more accurate term but slightly rarer
   term with the same meaning is "pixmap" (pixel-map).

   @bold (SPRITE:) a particular usage of bitmapped images, restricted to video
   games (other types of programmer probably won't be familiar with this term).
   Originally on machines like the C64, sprites were a hardware feature that
   allowed a number of small bitmap images to be loaded into special registers,
   and they could then be superimposed over the main graphics display and moved
   around just by modifying the position register.  They were used for the
   moving objects (player and enemy characters), and enabled the C64 to do much
   more impressive things than would have been possible if all the drawing had
   to be done directly by the puny CPU.

   Later on, a lot of old C64 programmers upgraded to machines like the Atari
   ST, which didn't have any special sprite hardware, but they carried on
   referring to their main moving objects as sprites (the routine to draw such
   a thing would obviously be called @code (draw_sprite)).  A sprite is really
   just a bitmap graphic which is drawn onto the screen, but when you call it a
   sprite rather than a bitmap, this suggests it is a gameplay element that can
   move freely around the world rather than being a static part of the
   environment, and that it will be drawn in a masked overlay mode rather than
   as a solid rectangle (there is also a strong implication that a sprite will
   be animated by cycling through a number of frames, but that isn't always the
   case).

   In recent years some people have started using "sprite" to refer to any
   character graphics, even if they are not in fact drawn as 2d bitmaps, eg.
   @italic ("this game uses 3d polygonal player sprites"). This is a confusing
   misuse of the word (Doom uses sprites, Quake does not), but it does happen.

   The origin of the term @italic (blit) is also rather interesting.  This was
   originally BitBlt, an abbreviation of BITmap BLock Transfer, which was a
   function designed (possibly) by the people at Xerox who did so much of the
   pioneering work on graphics display systems, and subsequently copied by
   virtually everybody doing computer graphics (the Microsoft Windows GDI still
   provides a BitBlt function with identical functionality to the original).
   This routine was a workhorse for all sorts of drawing operations, basically
   copying bitmap graphics from one place to another, but including a number of
   different ROP modes (Raster OPerations) for doing things like XOR, inverting
   pixels, etc.  A whole family of related words grew up around the BitBlt
   function, but "blt" is impossible to speak (try saying "bltter" or "bltting"
   :-) so people added the vowel to make it easier to pronounce.

   Therefore, the act of calling the BitBlt function came to be known as
   @italic ("doing a blit").  The obvious next step was to rename the function
   itself to @code (blit), which generally took place at the same time as
   people decided to simplify the original, removing the different ROP modes on
   the grounds that they aren't needed for games coding and don't work well
   with anything higher than monochrome images in any case.  This leaves us
   with a function called @code (blit), which is an abbreviation for @italic
   ("block transfer").  A strong case could be made for calling this @code
   (blot) instead, but somehow that just doesn't sound the same!

   Anyway, all the routines in this chapter are affected by the clipping
   rectangle of the destination bitmap. *)

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
  albase, albitmap, alfixed; { Needs some basic definitions. }



CONST
(* Drawing modes for al_draw_sprite_ex. *)
  AL_DRAW_SPRITE_NORMAL = 0; {< @ignore }
  AL_DRAW_SPRITE_LIT    = 1; {< @ignore }
  AL_DRAW_SPRITE_TRANS  = 2; {< @ignore }


(* Flipping modes for al_draw_sprite_ex. *)
  AL_DRAW_SPRITE_NO_FLIP = 0; {< @ignore }
  AL_DRAW_SPRITE_H_FLIP  = 1; {< @ignore }
  AL_DRAW_SPRITE_V_FLIP  = 2; {< @ignore }
  AL_DRAW_SPRITE_VH_FLIP = 3; {< @ignore }



(* Copies a rectangular area of the source bitmap to the destination bitmap.
   The @code (source_x) and @code (source_y) parameters are the top left corner
   of the area to copy from the source bitmap, and @code (dest_x) and @code
   (dest_y) are the corresponding position in the destination bitmap.  This
   routine respects the destination clipping rectangle, and it will also clip
   if you try to blit from areas outside the source bitmap.

   You can blit between any parts of any two bitmaps, even if the two memory
   areas overlap (ie. source and dest are the same, or one is sub-bitmap of the
   other).  You should be aware, however, that a lot of SVGA cards don't
   provide separate read and write banks, which means that blitting from one
   part of the screen to another requires the use of a temporary bitmap in
   memory, and is therefore extremely slow.  As a general rule you should avoid
   blitting from the screen onto itself in SVGA modes.

   If the @link (AL_GFX_HW_VRAM_BLIT) bit in the @link (al_gfx_capabilities)
   flag is set, the current driver supports hardware accelerated blits from one
   part of the screen onto another.  This is extremely fast, so when this flag
   is set it may be worth storing some of your more frequently used graphics in
   an offscreen portion of the video memory.

   Unlike most of the graphics routines, @code (al_blit) allows the source and
   destination bitmaps to be of different color depths, so it can be used to
   convert images from one pixel format to another.  In this case, the behavior
   is affected by the @link (AL_COLORCONV_KEEP_TRANS) and @code
   (AL_COLORCONV_DITHER* ) flags of the current color conversion mode: see
   @link (al_set_color_conversion) for more information. *)
  PROCEDURE al_blit (source, dest: AL_BITMAPptr; source_x, source_y, dest_x, dest_y, width, height: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'blit';

(* Like @link (al_blit), except it can scale images (so the source and
   destination rectangles don't need to be the same size) and requires the
   source and destination bitmaps to be of the same color depth.  This routine
   doesn't do as much safety checking as the regular @code (al_blit):  in
   particular you must take care not to copy from areas outside the source
   bitmap, and you cannot blit between overlapping regions, ie. you must use
   different bitmaps for the source and the destination.  Moreover, the source
   must be a memory bitmap. *)
  PROCEDURE al_stretch_blit (source, dest: AL_BITMAPptr; source_x, source_y, source_width, source_height, dest_x, dest_y, dest_width, dest_height: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stretch_blit';

(* Like @link (al_blit), but skips transparent pixels, which are marked by a
   zero in 256-color modes or bright pink for truecolor data (maximum red and
   blue, zero green), and requires the source and destination bitmaps to be of
   the same color depth.  The source and destination regions must not overlap.

   If the @link (AL_GFX_HW_VRAM_BLIT_MASKED) bit in the @link
   (al_gfx_capabilities) flag is set, the current driver supports hardware
   accelerated masked blits from one part of the screen onto another.  This is
   extremely fast, so when this flag is set it may be worth storing some of
   your more frequently used sprites in an offscreen portion of the video
   memory.

   @bold (Warning:)  if the hardware acceleration flag is not set, @code
   (masked_blit) will not work correctly when used with a source image in
   system or video memory so the latter must be a memory bitmap. *)
  PROCEDURE al_masked_blit (source, dest: AL_BITMAPptr; source_x, source_y, dest_x, dest_y, width, height: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'masked_blit';

(* Like @link (al_masked_blit), except it can scale images (so the source and
   destination rectangles don't need to be the same size).  This routine
   doesn't do as much safety checking as the regular @code (al_masked_blit):
   in particular you must take care not to copy from areas outside the source
   bitmap.  Moreover, the source must be a memory bitmap. *)
  PROCEDURE al_masked_stretch_blit (source, dest: AL_BITMAPptr; source_x, source_y, source_width, source_height, dest_x, dest_y, dest_width, dest_height: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'masked_stretch_blit';



(* Draws a copy of the sprite bitmap onto the destination bitmap at the
   specified position.  This is almost the same as @link (al_blit) @code
   (@(sprite, bmp, 0, 0, x, y, sprite^.w, sprite^.h@)), but it uses a masked
   drawing mode where transparent pixels are skipped, so the background image
   will show through the masked parts of the sprite.  Transparent pixels are
   marked by a zero in 256-color modes or bright pink for truecolor data
   (maximum red and blue, zero green). Example:
@longcode (#
VAR
  SpaceShip: AL_BITMAPptr;

          ...
  al_draw_sprite (al_screen, SpaceShip, x, y);
  #)

  If the @link (AL_GFX_HW_VRAM_BLIT_MASKED) bit in the @link
  (al_gfx_capabilities) flag is set, the current driver supports hardware
  accelerated sprite drawing when the source image is a video memory bitmap or
  a sub-bitmap of the screen.  This is extremely fast, so when this flag is set
  it may be worth storing some of your more frequently used sprites in an
  offscreen portion of the video memory.

  @bold (Warning:)  if the hardware acceleration flag is not set, @code
  (al_draw_sprite) will not work correctly when used with a sprite image in
  system or video memory so the latter must be a memory bitmap.

  Although generally not supporting graphics of mixed color depths, as a
  special case this function can be used to draw 256-color source images onto
  truecolor destination bitmaps, so you can use palette effects on specific
  sprites within a truecolor program. . *)
  PROCEDURE al_draw_sprite (bmp, sprite: AL_BITMAPptr; x, y: LONGINT);

(* Like @link (al_draw_sprite), except it can stretch the sprite image to the
   specified width and height and requires the sprite image and destination
   bitmap to be of the same color depth.  Moreover, the sprite image must be a
   memory bitmap. *)
  PROCEDURE al_stretch_sprite (bmp, sprite: AL_BITMAPptr; x, y, w, h: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stretch_sprite';

(* Draws the sprite image onto the destination bitmap using the specified @code
   (mode) argument, optionally flipping the sprite in the orientation specified
   by @code (flip) argument.

   @param (mode defines how is sprite going to be drawn on the destination
     bitmap:
@unorderedList (
  @item (@code (AL_DRAW_SPRITE_NORMAL) draws a masked sprite, like @link
	(al_draw_sprite).)
  @item (@code (AL_DRAW_SPRITE_LIT) draws a tinted sprite, like @link
	(al_draw_lit_sprite).)
  @item (@code (AL_DRAW_SPRITE_TRANS) draws a blended sprite, like @link
	(draw_trans_sprite). )
)
   )
   @param (flip defines the flipping orientation:
@unorderedList (
  @item (@code (AL_DRAW_SPRITE_NO_FLIP) do not perform flipping.)
  @item (@code (AL_DRAW_SPRITE_H_FLIP) flip horizontally.)
  @item (@code (AL_DRAW_SPRITE_V_FLIP) flip vertically.)
  @item (@code (AL_DRAW_SPRITE_VH_FLIP) flip both vertically and horizontally-)
)
   ) *)
  PROCEDURE al_draw_sprite_ex (bmp, sprite: AL_BITMAPptr; x, y, mode, flip: LONGINT);

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



PROCEDURE al_draw_sprite_ex (bmp, sprite: AL_BITMAPptr;
			     x, y, mode, flip: LONGINT);
BEGIN
  bmp^.vtable^.draw_sprite_ex (bmp, sprite, x, y, mode, flip);
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

