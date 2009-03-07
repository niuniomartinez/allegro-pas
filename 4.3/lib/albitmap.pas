UNIT albitmap;
(*< Once you have selected a graphics mode, you can draw things onto the
    display via the @link (al_screen) bitmap.  All the Allegro graphics
    routines draw onto @link (AL_BITMAP) structures, which are areas of memory
    containing rectangular images, stored as packed byte arrays (in 8-bit modes
    one byte per pixel, in 15- and 16-bit modes two bytes per pixel, in 24-bit
    modes 3 bytes per pixel and in 32-bit modes 4 bytes per pixel).  You can
    create and manipulate bitmaps in system RAM, or you can write to the special
    @code (al_screen) bitmap which represents the video memory in your graphics
    card.

    Allegro supports several different types of bitmaps:
    @unorderedList (
      @item (The @link (al_screen) bitmap, which represents the hardware video
	memory.  Ultimately you have to draw onto this in order for your image
	to be visible.  It is destroyed by any subsequent calls to @link
	(al_set_gfx_mode), so you should never attempt to destroy it yourself.)
      @litem (Memory bitmaps, which are located in system RAM and can be used
	to store graphics or as temporary drawing spaces for double buffered
	systems.  These can be obtained by calling @link (al_create_bitmap),
	@link (al_load_bitmap), or by loading a grabber datafile.)
      @item (Sub-bitmaps.  These share image memory with a parent bitmap (which
	can be the screen, a video or system bitmap, a memory bitmap, or
	another sub-bitmap), so drawing onto them will also change their
	parent.  They can be of any size and located anywhere within the parent
	bitmap, and can have their own clipping rectangles, so they are a
	useful way of dividing a bitmap into several smaller units, eg.
	splitting a large virtual screen into multiple sections.  @bold
	(Warning:) Make sure not to destroy a bitmap before all of its
	sub-bitmaps, otherwise bad things will happen when you try to access
	one of these sub-bitmaps.)
      @item (Video memory bitmaps.  These are created by the @link
	(al_create_video_bitmap) function, and are usually implemented as
	sub-bitmaps of the screen object.  They must be destroyed by @link
	(al_destroy_bitmap) before any subsequent calls to @code
	(al_set_gfx_mode).)
      @item (System bitmaps.  These are created by the @link
	(al_create_system_bitmap) function, and are a sort of halfway house
	between memory and video bitmaps.  They live in system memory, so you
	aren't limited by the amount of video ram in your card, but they are
	stored in a platform-specific format that may enable better hardware
	acceleration than is possible with a normal memory bitmap (see the
	@link (AL_GFX_HW_SYS_TO_VRAM_BLIT) and @link
	(AL_GFX_HW_SYS_TO_VRAM_BLIT_MASKED) flags in @link
	(al_gfx_capabilities)).  System bitmaps must be accessed in the same
	way as video bitmaps, using the bank switch functions and @code
	(bmp_write* ) macros.  Not every platform implements this type of
	bitmap:  if they aren't available, @code (al_create_system_bitmap) will
	function identically to @code (al_create_bitmap).  They must be
	destroyed by @code (al_destroy_bitmap) before any subsequent calls to
	@code (al_set_gfx_mode).)
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
(* Pointer to @link (AL_BITMAP). *)
  AL_BITMAPptr = ^AL_BITMAP;
(* Stores the contents of a bitmap.

   There is some stuff in the structure that is liable to change and you
   shouldn't use anything except the next.  The @code (w) and @code (h) fields
   can be used to obtain the size of an existing bitmap:
@longcode (#
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
   Use @link (al_set_clip_rect) instead. *)
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



(* Creates a memory bitmap sized @code (w) by @code (h).  The bitmap will have
   clipping turned on, and the clipping rectangle set to the full size of the
   bitmap.  The image memory will not be cleared, so it will probably contain
   garbage:  you should clear the bitmap before using it.  This routine always
   uses the global pixel format, as specified by calling @link
   (al_set_color_depth).  The minimum height of the bitmap must be 1 and width
   can't be negative.  Example:
@longcode (#
VAR
  bmp: AL_BITMAPptr;
          ...
  bmp := al_create_bitmap (AL_SCREEN_W, 10);
  IF bmp = NIL THEN
    abort_on_error ('Couldn''t create bitmap!');
          ...
  al_destroy_bitmap (bmp);
  #)

   @returns (a pointer to the created bitmap, or @nil if the bitmap could not
     be created . Remember to free this bitmap later to avoid memory leaks.  *)
  FUNCTION al_create_bitmap (w, h: LONGINT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_bitmap';

(* Creates a bitmap in a specific color depth (8, 15, 16, 24 or 32 bits per
   pixel).
   @returns (a pointer to the created bitmap, or @nil if the bitmap could not
     be created . Remember to free this bitmap later to avoid memory leaks.
   @seealso (al_create_bitmap) *)
  FUNCTION al_create_bitmap_ex (bpp, width, height: LONGINT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_bitmap_ex';
  FUNCTION al_create_sub_bitmap (parent: AL_BITMAPptr; x, y, w, h: LONGINT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_sub_bitmap';
(* Frees the bitmap resources. *)
  PROCEDURE al_destroy_bitmap (bmp: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_bitmap';
(* Gets bitmap information. *)
  FUNCTION al_bitmap_color_depth (bmp: AL_BITMAPptr): LONGINT;
  FUNCTION al_bitmap_mask_color (bmp: AL_BITMAPptr): LONGINT;
  FUNCTION al_is_same_bitmap (bmp1, bmp2: AL_BITMAPptr): BOOLEAN;
  FUNCTION al_is_sub_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
(* Acquire and release bitmap. *)
  PROCEDURE al_acquire_bitmap (bmp: AL_BITMAPptr);
  PROCEDURE al_release_bitmap (bmp: AL_BITMAPptr);
(* Palette. *)
  FUNCTION  al_generate_optimized_palette (image: AL_BITMAPptr; pal: AL_PALETTEptr; rsvdcols: ARRAY OF CHAR): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'generate_optimized_palette';

(* Loading and saving bitmaps. *)
  FUNCTION al_load_bitmap (filename: AL_STRING; pal: AL_PALETTEptr): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_bitmap';

  FUNCTION al_save_bitmap (filename: AL_STRING; bmp: AL_BITMAPptr; pal: AL_PALETTEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_bitmap';



IMPLEMENTATION

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



FUNCTION al_bitmap_mask_color (bmp: AL_BITMAPptr): AL_INT;
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

