UNIT albitmap;
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
 *	Bitmap manipulation.
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
  albase, alpalete, alvtable
{$IFNDEF FPC}
 {$IFDEF MSWINDOWS}
{ Delphi needs the Windows unit. }
  , Windows
 {$ENDIF}
{$ENDIF}
  ;


TYPE
  AL_BITMAPptr = ^AL_BITMAP;
  AL_BITMAP = RECORD
    w, h: AL_INT;		{ width and height in pixels }
    clip: AL_INT;		{ flag if clipping is turned on }
    cl, cr, ct, cb: AL_INT;	{ clip left, right, top and bottom values }
    vtable: AL_GFX_VTABLEptr;	{ drawing functions }
    write_bank: AL_PTR;		{ C func on some machines, asm on i386 }
    read_bank: AL_PTR;		{ C func on some machines, asm on i386 }
    dat: AL_PTR;		{ the memory we allocated for the bitmap }
    id: DWORD;			{ for identifying sub-bitmaps }
    extra: AL_PTR;		{ points to a structure with more info }
    x_ofs: AL_INT;		{ horizontal offset (for sub-bitmaps) }
    y_ofs: AL_INT;		{ vertical offset (for sub-bitmaps) }
    seg: AL_INT;		{ bitmap segment }
    line: AL_PTR;		{ ZERO_SIZE_ARRAY(unsigned char *, line); }
  END;



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



(* Creates bitmaps. *)
  FUNCTION al_create_bitmap (w, h: AL_INT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_bitmap';
  FUNCTION al_create_bitmap_ex (bpp, w, h: AL_INT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_bitmap_ex';
  FUNCTION al_create_sub_bitmap (parent: AL_BITMAPptr; x, y, w, h: AL_INT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_sub_bitmap';
(* Frees the bitmap resources. *)
  PROCEDURE al_destroy_bitmap (bmp: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_bitmap';
(* Gets bitmap information. *)
  FUNCTION al_bitmap_color_depth (bmp: AL_BITMAPptr): AL_INT;
  FUNCTION al_bitmap_mask_color (bmp: AL_BITMAPptr): AL_INT;
  FUNCTION al_is_same_bitmap (bmp1, bmp2: AL_BITMAPptr): BOOLEAN;
  FUNCTION al_is_sub_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
(* Acquire and release bitmap. *)
  PROCEDURE al_acquire_bitmap (bmp: AL_BITMAPptr);
  PROCEDURE al_release_bitmap (bmp: AL_BITMAPptr);
(* Palette. *)
  FUNCTION  al_generate_optimized_palette (image: AL_BITMAPptr; pal: AL_PALETTEptr; rsvdcols: ARRAY OF AL_CHAR): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'generate_optimized_palette';

(* Loading and saving bitmaps. *)
  FUNCTION al_load_bitmap (filename: AL_STRING; pal: AL_PALETTEptr): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_bitmap';

  FUNCTION al_save_bitmap (filename: AL_STRING; bmp: AL_BITMAPptr; pal: AL_PALETTEptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_bitmap';



IMPLEMENTATION



FUNCTION al_bitmap_color_depth (bmp: AL_BITMAPptr): AL_INT;
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

