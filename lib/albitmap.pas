(* Defines the bitmap. *)
UNIT albitmap;
(*	Based on the file "gfx.h" of the Allegro library by Shawn Hargreaves.
 *	Some parts of the code was created by the "h2pas" utility. *)

{ Defines the frame. }
{$MODE Delphi}
{$PACKRECORDS C}
{$H+}



INTERFACE

USES
  alpalete, alvtable;



TYPE
(* Pointer to a bitmap. *)
  AL_BITMAPptr = ^AL_BITMAP;
(* Stores the contents of a bitmap. *)
  AL_BITMAP = RECORD
    w, h: LONGINT;		{<width and height in pixels }
    clip: LONGINT;		{<flag if clipping is turned on }
    cl, cr, ct, cb: LONGINT;	{<clip left, right, top and bottom values }
    vtable: AL_GFX_VTABLEptr;	{<drawing functions }
    write_bank: POINTER;	{<C func on some machines, asm on i386 }
    read_bank: POINTER;		{<C func on some machines, asm on i386 }
    dat: POINTER;		{<the memory we allocated for the bitmap }
    id: DWORD;			{<for identifying sub-bitmaps }
    extra: POINTER;		{<points to a structure with more info }
    x_ofs: LONGINT;		{<horizontal offset (for sub-bitmaps) }
    y_ofs: LONGINT;		{<vertical offset (for sub-bitmaps) }
    seg: LONGINT;		{<bitmap segment }
    line: POINTER;		{<ZERO_SIZE_ARRAY(unsigned char *, line); }
  END;



CONST
(* Identify bitmap type.  Internal use only. *)
  AL_BMP_ID_VIDEO     = $80000000;
  AL_BMP_ID_SYSTEM    = $40000000;
  AL_BMP_ID_SUB       = $20000000;
  AL_BMP_ID_PLANAR    = $10000000;
  AL_BMP_ID_NOBLIT    = $08000000;
  AL_BMP_ID_LOCKED    = $04000000;
  AL_BMP_ID_AUTOLOCK  = $02000000;
  AL_BMP_ID_MASK      = $01FFFFFF;



VAR
(* Creates bitmaps. *)
  al_create_bitmap: FUNCTION (w, h: LONGINT): AL_BITMAPptr; CDECL;
  al_create_bitmap_ex: FUNCTION (bpp, w, h: LONGINT): AL_BITMAPptr; CDECL;
  al_create_sub_bitmap: FUNCTION (parent: AL_BITMAPptr; x, y, w, h: LONGINT): AL_BITMAPptr; CDECL;
(* Frees the bitmap resources. *)
  al_destroy_bitmap: PROCEDURE (bmp: AL_BITMAPptr); CDECL;

(* Gets bitmap information. *)
  FUNCTION al_bitmap_color_depth (bmp: AL_BITMAPptr): LONGINT;
  FUNCTION al_bitmap_mask_color (bmp: AL_BITMAPptr): LONGINT;
  FUNCTION al_is_same_bitmap (bmp1, bmp2: AL_BITMAPptr): BOOLEAN;
  FUNCTION al_is_sub_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
(* Acquire and release bitmap. *)
  PROCEDURE al_acquire_bitmap (bmp: AL_BITMAPptr);
  PROCEDURE al_release_bitmap (bmp: AL_BITMAPptr);

VAR
(* Palette. *)
  al_generate_optimized_palette: FUNCTION (image: AL_BITMAPptr; pal: AL_PALETTEptr; rsvdcols: ARRAY OF CHAR): LONGINT; CDECL;

(* Loading and saving bitmaps. *)
  al_load_bitmap: FUNCTION (filename: STRING; pal: AL_PALETTEptr): AL_BITMAPptr; CDECL;
  al_save_bitmap: FUNCTION (filename: STRING; bmp: AL_BITMAPptr; pal: AL_PALETTEptr): LONGINT; CDECL;



IMPLEMENTATION

USES
  albase;



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



INITIALIZATION
  @al_create_bitmap := al_get_object_address ('create_bitmap');
  @al_create_bitmap_ex := al_get_object_address ('create_bitmap_ex');
  @al_create_sub_bitmap := al_get_object_address ('create_sub_bitmap');
  @al_destroy_bitmap := al_get_object_address ('destroy_bitmap');
  @al_generate_optimized_palette := al_get_object_address ('generate_optimized_palette');
  @al_load_bitmap := al_get_object_address ('load_bitmap');
  @al_save_bitmap := al_get_object_address ('save_bitmap');
END.
