UNIT albitmap;
(*< Once you have selected a graphics mode, you can draw things onto the
    display via the @link(al_screen) bitmap.  All the Allegro graphics routines
    draw onto @link(AL_BITMAP) structures, which are areas of memory containing
    rectangular images, stored as packed byte arrays (in 8-bit modes one byte
    per pixel, in 15- and 16-bit modes two bytes per pixel, in 24-bit modes 3
    bytes per pixel and in 32-bit modes 4 bytes per pixel).  You can create and
    manipulate bitmaps in system RAM, or you can write to the special
    @code(al_screen) bitmap which represents the video memory in your graphics
    card.

    Allegro supports several different types of bitmaps:
    @unorderedList(
      @item(The @link(al_screen) bitmap, which represents the hardware video
	memory.  Ultimately you have to draw onto this in order for your image
	to be visible.  It is destroyed by any subsequent calls to
	@link(al_set_gfx_mode), so you should never attempt to destroy it
	yourself.)
      @item(Memory bitmaps, which are located in system RAM and can be used to
	store graphics or as temporary drawing spaces for double buffered
	systems.  These can be obtained by calling @link(al_create_bitmap),
	@link(al_load_bitmap), or by loading a grabber datafile.)
      @item(Sub-bitmaps.  These share image memory with a parent bitmap (which
	can be the screen, a video or system bitmap, a memory bitmap, or
	another sub-bitmap), so drawing onto them will also change their
	parent.  They can be of any size and located anywhere within the parent
	bitmap, and can have their own clipping rectangles, so they are a
	useful way of dividing a bitmap into several smaller units, eg.
	splitting a large virtual screen into multiple sections.
	@bold(Warning:) Make sure not to destroy a bitmap before all of its
	sub-bitmaps, otherwise bad things will happen when you try to access
	one of these sub-bitmaps.)
      @item(Video memory bitmaps.  These are created by the
	@link(al_create_video_bitmap) function, and are usually implemented as
	sub-bitmaps of the screen object.  They must be destroyed by
	@link(al_destroy_bitmap) before any subsequent calls to
	@code(al_set_gfx_mode).)
      @item(System bitmaps.  These are created by the
	@link(al_create_system_bitmap) function, and are a sort of halfway
	house between memory and video bitmaps.  They live in system memory,
	so you aren't limited by the amount of video ram in your card, but they
	are stored in a platform-specific format that may enable better
	hardware acceleration than is possible with a normal memory bitmap (see
	the @link(AL_GFX_HW_SYS_TO_VRAM_BLIT) and
	@link(AL_GFX_HW_SYS_TO_VRAM_BLIT_MASKED) flags in
	@link(al_gfx_capabilities)).  System bitmaps must be accessed in the
	same way as video bitmaps, using the bank switch functions and
	@code(bmp_write* ) macros.  Not every platform implements this type of
	bitmap:  if they aren't available, @code(al_create_system_bitmap) will
	function identically to @code(al_create_bitmap).  They must be
	destroyed by @code(al_destroy_bitmap) before any subsequent calls to
	@code(al_set_gfx_mode).)
    ) *)

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$MODE FPC}
{$ELSE}
{ Assumes Codegear Delphi/Turbo. }
 {$A-}
{$ENDIF}
{$H+}




INTERFACE

USES
  albase, alpalete, alvtable
{$IFNDEF FPC}
 {$IFDEF MSWINDOWS}
{ Delphi needs the Windows unit. }
  , Windows
 {$ENDIF}
{$ENDIF}
  ;


TYPE
(* Pointer to @link(AL_BITMAP). *)
  AL_BITMAPptr = ^AL_BITMAP;

(* Stores the contents of a bitmap.

   There is some stuff in the structure that is liable to change and you
   shouldn't use anything except the next.  The @code(w) and @code(h) fields
   can be used to obtain the size of an existing bitmap:
@longcode(#
CONST
  BS = 'Bitmap size: (%dx%d)\n';
VAR
  bmp: AL_BITMAPptr;
  Message: STRING;
BEGIN
  bmp = al_load_bitmap ('file.bmp', pal);
  StrFmt (Message, BS, [bmp^.w, bmp^.h]);
  al_allegro_message (Message);
END.
#)

   The clipping rectangle is inclusive on the left and top (0 allows drawing to
   position 0) but exclusive on the right and bottom (10 allows drawing to
   position 9, but not to 10).  Note this is not the same format as that of the
   clipping API, which takes inclusive coordinates for all four corners.  All
   the values of this structure should be regarded as read-only.  If you want
   to modify the clipping region, please refrain from changing this structure.
   Use @link(al_set_clip_rect) instead. *)
  AL_BITMAP = RECORD
    w, h: LONGINT;		{< width and height in pixels }
    clip: LONGINT;		{< flag if clipping is turned on }
    cl, cr, ct, cb: LONGINT;	{< clip left, right, top and bottom values }
    vtable: AL_GFX_VTABLEptr;	{< drawing functions }
    write_bank: POINTER;		{< C func on some machines, asm on i386 }
    read_bank: POINTER;		{< C func on some machines, asm on i386 }
    dat: POINTER;		{< the memory we allocated for the bitmap }
    id: DWORD;			{< for identifying sub-bitmaps }
    extra: POINTER;		{< points to a structure with more info }
    x_ofs: LONGINT;		{< horizontal offset (for sub-bitmaps) }
    y_ofs: LONGINT;		{< vertical offset (for sub-bitmaps) }
    seg: LONGINT;		{< bitmap segment }
    line: POINTER;		{< ZERO_SIZE_ARRAY(unsigned char *, line); }
  END;



(* Creates a memory bitmap sized @code(w) by @code(h).  The bitmap will have
   clipping turned on, and the clipping rectangle set to the full size of the
   bitmap.  The image memory will not be cleared, so it will probably contain
   garbage:  you should clear the bitmap before using it.  This routine always
   uses the global pixel format, as specified by calling
   @link(al_set_color_depth).  The minimum height of the bitmap must be 1 and
   width can't be negative.

   @returns(a pointer to the created bitmap, or @nil if the bitmap could not
     be created . Remember to free this bitmap later to avoid memory leaks.) *)
  FUNCTION al_create_bitmap (w, h: LONGINT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_bitmap';

(* Creates a bitmap in a specific color depth (8, 15, 16, 24 or 32 bits per
   pixel).
   @returns(a pointer to the created bitmap, or @nil if the bitmap could not
     be created . Remember to free this bitmap later to avoid memory leaks.)
   @seealso(al_create_bitmap) *)
  FUNCTION al_create_bitmap_ex (bpp, width, height: LONGINT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_bitmap_ex';

(* Creates a sub-bitmap, ie. a bitmap sharing drawing memory with a
   pre-existing bitmap, but possibly with a different size and clipping
   settings.  When creating a sub-bitmap of the mode-X screen, the x position
   must be a multiple of four.  The sub-bitmap width and height can extend
   beyond the right and bottom edges of the parent (they will be clipped), but
   the origin point must lie within the parent region.

   Remember to free the sub bitmap before freeing the parent bitmap to avoid
   memory leaks and potential crashes accessing memory which has been freed.

   @returns(a pointer to the created sub bitmap, or @nil if the sub bitmap
     could not be created.) *)
  FUNCTION al_create_sub_bitmap (parent: AL_BITMAPptr; x, y, w, h: LONGINT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_sub_bitmap';

(* Allocates a video memory bitmap of the specified size.  This can be used to
   allocate offscreen video memory for storing source graphics ready for a
   hardware accelerated blitting operation, or to create multiple video memory
   pages which can then be displayed by calling @link(al_show_video_bitmap).
   Read the introduction of this chapter for a comparison with other types of
   bitmaps and other specific details.

   @bold(Warning:)  video memory bitmaps are usually allocated from the same
   space as the screen bitmap, so they may overlap with it; it is therefore not
   a good idea to use the global screen at the same time as any surfaces
   returned by this function.

   Remember to destroy this bitmap before any subsequent call to
   @link(al_set_gfx_mode).

   @returns(a pointer to the bitmap on success, or @nil if you have run out of
     video ram.) *)
  FUNCTION al_create_video_bitmap (width, height: LONGINT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_video_bitmap';

(* Allocates a system memory bitmap of the specified size.  Read the
   introduction of this chapter for a comparison with other types of bitmaps
   and other specific details.

   Remember to destroy this bitmap before any subsequent call to
   @link(al_set_gfx_mode).

   @returns(a pointer to the bitmap on success, @nil otherwise.) *)
  FUNCTION al_create_system_bitmap (width, height: LONGINT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_system_bitmap';

(* Destroys a memory bitmap, sub-bitmap, video memory bitmap, or system bitmap
   when you are finished with it.  If you pass a @nil pointer this function
   won't do anything.

   The bitmap must not have a mouse cursor shown on it at the time it is
   destroyed. *)
  PROCEDURE al_destroy_bitmap (bmp: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_bitmap';



(* @returns(the color depth of the specified bitmap @(8, 15, 16, 24, or 32@).) *)
  FUNCTION al_bitmap_color_depth (bmp: AL_BITMAPptr): LONGINT;

(* @returns(the mask color for the specified bitmap @(the value which is
    skipped when drawing sprites@).  For 256-color bitmaps this is zero, and
    for truecolor bitmaps it is bright pink @(maximum red and blue, zero
    green@).  A frequent use of this function is to clear a bitmap with the
    mask color so you can later use this bitmap with @link(al_draw_sprite)
    after drawing other stuff on it.) *)
  FUNCTION al_bitmap_mask_color (bmp: AL_BITMAPptr): LONGINT;

(* @returns(@true if the two bitmaps describe the same drawing surface, ie.
    the pointers are equal, one is a sub-bitmap of the other, or they are both
    sub-bitmaps of a common parent.) *)
  FUNCTION al_is_same_bitmap (bmp1, bmp2: AL_BITMAPptr): BOOLEAN;

(* @returns(@true if bmp is a memory bitmap, ie. it was created by calling
    @link(al_create_bitmap) or loaded from a grabber datafile or image file.) *)
  FUNCTION al_is_memory_bitmap (bmp: AL_BITMAPptr): BOOLEAN;

(* @returns(@true if @code(bmp) is the screen bitmap, or a sub-bitmap of the
   screen.) *)
  FUNCTION al_is_screen_bitmap (bmp: AL_BITMAPptr): BOOLEAN;

(* @returns(@true) if bmp is the screen bitmap, a video memory bitmap, or a
   sub-bitmap of either.) *) 
  FUNCTION al_is_video_bitmap (bmp: AL_BITMAPptr): BOOLEAN;

(* @returns(@true if bmp is a system bitmap object, or a sub-bitmap of one.) *)
  FUNCTION al_is_system_bitmap (bmp: AL_BITMAPptr): BOOLEAN;

(* @returns(@true if bmp is a sub-bitmap.) *)
  FUNCTION al_is_sub_bitmap (bmp: AL_BITMAPptr): BOOLEAN;

(* Acquires the specified video bitmap prior to drawing onto it.  You never
   need to call the function explicitly as it is low level, and will only give
   you a speed up if you know what you are doing.  Using it wrongly may cause
   slowdown, or even lock up your program.

   @bold(Note:) You do never need to use @code(al_acquire_bitmap) on a memory
   bitmap, i.e. a normal bitmap created with @link(al_create_bitmap).  It will
   simply do nothing in that case.

   It still can be useful, because e.g. under the current DirectDraw driver of
   Allegro, most drawing functions need to lock a video bitmap before drawing
   to it.  But doing this is very slow, so you will get much better performance
   if you acquire the screen just once at the start of your main redraw
   function, then call multiple drawing operations which need the bitmap
   locked, and only release it when done.

   Multiple acquire calls may be nested, but you must make sure to match up the
   acquire_bitmap and @link(al_release_bitmap) calls.  Be warned that DirectX
   and X11 programs activate a mutex lock whenever a surface is locked, which
   prevents them from getting any input messages, so you must be sure to
   release all your bitmaps before using any timer, keyboard, or other
   non-graphics routines!

   Note that if you are using hardware accelerated VRAM->VRAM functions, you
   should not call @code(al_acquire_bitmap).  Such functions need an unlocked
   target bitmap under DirectX, so there is now just the opposite case from
   before - if the bitmap is already locked with @code(al_acquire_bitmap), the
   drawing operation has to unlock it.

   @bold(Note:) For backwards compatibility, the unlocking behavior of such
   functions is permanent.  That is, if you call @code(al_acquire_bitmap)
   first, then call e.g. an accelerated blit, the DirectX bitmap will be
   unlocked internally (it won't affect the nesting counter of acquire/release
   calls).

   There is no clear cross-platform way in this Allegro version to know which
   drawing operations need a locked/unlocked state.  For example a normal
   rectfill most probably is accelerated under DirectX, and therefore needs the
   screen unlocked, but an @code(XOR) rectfill, or one with blending
   activated, most probably is not, and therefore locks the screen. And while
   the DirectX driver will do automatic unlocking, there is no such thing under
   X11, where the function is used to synchronize X11 calls from different
   threads.  Your best bet is to never use @code(al_acquire_bitmap) - changes
   are you are doing something in the wrong way if you think you need it.

   @bold(Warning:) This function can be very dangerous to use, since the whole
   program may get locked while the bitmap is locked.  So the lock should only
   be held for a short time, and you should not call anything but drawing
   operations onto the locked video bitmap while a lock is in place.
   Especially don't call things like @link(al_show_mouse) (or
   @link(al_scare_mouse) which calls that) or @link(al_readkey), since it will
   most likely deadlock your entire program. *)
  PROCEDURE al_acquire_bitmap (bmp: AL_BITMAPptr);

(* Releases a bitmap that was previously locked by calling
   @link(al_acquire_bitmap).  If the bitmap was locked multiple times, you must
   release it the same number of times before it will truly be unlocked. *)
  PROCEDURE al_release_bitmap (bmp: AL_BITMAPptr);



(* Generates a 256-color palette suitable for making a reduced color version of
   the specified truecolor image.

   @param(rsvdcols points to a table indicating which colors it is allowed to
     modify:  zero for free colors which may be set to whatever the optimiser
     likes, negative values for reserved colors which cannot be used,
     and positive values for fixed palette entries that must not be changed,
     but can be used in the optimisation.)
   @returns(the number of different colors recognised in the provided bitmap,
     zero if the bitmap is not a truecolor image or there wasn't enough memory
     to perform the operation, and negative if there was any internal error in
     the color reduction code.) *)
  FUNCTION  al_generate_optimized_palette (image: AL_BITMAPptr; pal: AL_PALETTEptr; rsvdcols: ARRAY OF CHAR): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'generate_optimized_palette';



(* Loads a bitmap from a file.  The palette data will be stored in the second
   parameter, which should be an @link(AL_PALETTE) structure.  At present this
   function supports BMP, LBM, PCX, and TGA files, determining the type from
   the file extension.

   If the file contains a truecolor image, you must set the video mode or call
   @link(al_set_color_conversion) before loading it.  In this case, if the
   destination color depth is 8-bit, the palette will be generated by calling
   @link(al_generate_optimized_palette) on the bitmap;  otherwise, the
   returned palette will be generated by calling @link(al_generate_332_palette)

   The @code(pal) argument may be @nil.  In this case, the palette data are
   simply not returned.  Additionally, if the file is a truecolor image and the
   destination color depth is 8-bit, the color conversion process will use the
   current palette instead of generating an optimized one.

   Example:
@longcode(#
VAR
  bmp: AL_BITMAPptr;
  palette: AL_PALETTE;
          ...
  bmp := al_load_bitmap ('image.pcx', @palette);
  IF bmp = NIL THEN
    abort_on_error ('Couldn''t load image.pcx!');
          ...
   al_destroy_bitmap (bmp);
#)
   *)
  FUNCTION al_load_bitmap (filename: STRING; pal: AL_PALETTEptr): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_bitmap';



(* Writes a bitmap into a file, using the specified palette, which should be an
   @link(AL_PALETTE) structure.  The output format is determined from the
   filename extension: at present this function supports BMP, PCX and TGA
   formats.

   Two things to watch out for:  on some video cards it may be faster to copy
   the screen to a memory bitmap and save the latter, and if you use this to
   dump the screen into a file you may end up with an image much larger than
   you were expecting, because Allegro often creates virtual screens larger
   than the visible screen.  You can get around this by using a sub-bitmap to
   specify which part of the screen to save, eg:
@longcode(#
VAR
  bmp: AL_BITMAPptr;
  palette: AL_PALETTE;
          ...
  al_get_palette (@palette);
  bmp := al_create_sub_bitmap (al_screen, 0, 0, AL_SCREEN_W, AL_SCREEN_H);
  al_save_bitmap ('dump.pcx', bmp, @palette);
  al_destroy_bitmap (bmp);
#)
  *)
  FUNCTION al_save_bitmap (filename: STRING; bmp: AL_BITMAPptr; pal: AL_PALETTEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_bitmap';



IMPLEMENTATION

USES
  algraph;

CONST
(* Identify bitmap type *)
  AL_BMP_ID_VIDEO     = $80000000;
  AL_BMP_ID_SYSTEM    = $40000000;
  AL_BMP_ID_SUB       = $20000000;
  AL_BMP_ID_PLANAR    = $10000000;
  AL_BMP_ID_NOBLIT    = $08000000;
  AL_BMP_ID_LOCKED    = $04000000;
  AL_BMP_ID_AUTOLOCK  = $02000000;
  AL_BMP_ID_MASK      = $01FFFFFF;



FUNCTION al_bitmap_color_depth (bmp: AL_BITMAPptr): LONGINT;
BEGIN
  al_bitmap_color_depth := bmp^.vtable^.color_depth;
END;



FUNCTION al_bitmap_mask_color (bmp: AL_BITMAPptr): LONGINT;
BEGIN
  al_bitmap_mask_color := bmp^.vtable^.mask_color;
END;



FUNCTION al_is_same_bitmap (bmp1, bmp2: AL_BITMAPptr): BOOLEAN;
VAR
  m1, m2: DWORD;
BEGIN
  IF (bmp1 = NIL) OR (bmp2 = NIL) THEN
    al_is_same_bitmap := FALSE
  ELSE
    IF bmp1 = bmp2 THEN
      al_is_same_bitmap := TRUE
    ELSE BEGIN
      m1 := (bmp1^.id AND AL_BMP_ID_MASK);
      m2 := (bmp2^.id AND AL_BMP_ID_MASK);
      al_is_same_bitmap := ((m1 <> 0) AND (m1 = m2));
    END;
END;


FUNCTION al_is_memory_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
BEGIN
  al_is_memory_bitmap := (bmp^.id AND (AL_BMP_ID_VIDEO OR AL_BMP_ID_SYSTEM)) = 0;
END;



FUNCTION al_is_screen_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
BEGIN
  al_is_screen_bitmap := al_is_same_bitmap (bmp, al_screen);
END;



FUNCTION al_is_video_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
BEGIN
  al_is_video_bitmap := (bmp^.id AND AL_BMP_ID_VIDEO) <> 0;
END;



FUNCTION al_is_system_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
BEGIN
  al_is_system_bitmap := (bmp^.id AND AL_BMP_ID_SYSTEM) <> 0;
END;



FUNCTION al_is_sub_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
BEGIN
  al_is_sub_bitmap := (bmp^.id AND AL_BMP_ID_SUB) <> 0;
END;



PROCEDURE al_acquire_bitmap (bmp: AL_BITMAPptr);
BEGIN
  IF bmp <> NIL THEN
    bmp^.vtable^.acquire (bmp);
END;



PROCEDURE al_release_bitmap (bmp: AL_BITMAPptr);
BEGIN
  IF bmp <> NIL THEN
    bmp^.vtable^.release (bmp);
END;



END.

