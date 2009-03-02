UNIT alvtable;
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
 *      Virtual table to be used by AL_BITMAP and such.
 *
 *      By Ñuño Martínez.
 *	Based on the file "gfx.h" of the Allegro library by Shawn Hargreaves.
 *      Some parts of the code were converted by the "h2pas" utility.
 *
 *	See readme.txt for license and copyright information.
 *)

INTERFACE

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$LONGSTRINGS ON}
{$ELSE}
{ Assumes Borland Delphi/Turbo. }
 {$A-}
 {$H+}
{$ENDIF}



USES
  albase, alfixed;

TYPE
(* Some internal stuff to define virtual methods. *)
  AL_BITMAPptr = AL_PTR;
  AL_RLE_SPRITEptr = AL_PTR;
  _BMP_METHOD_ = PROCEDURE (bmp: AL_BITMAPptr); CDECL;
  _LINE_METHOD_ = PROCEDURE (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT); CDECL;
  _FLINE_METHOD_ = PROCEDURE (bmp: AL_BITMAPptr; a, b, c, d: AL_INT); CDECL;
  _CIRCLE_METHOD_ = PROCEDURE (bmp: AL_BITMAPptr; x, y, r, c: AL_INT); CDECL;
  _ELLIPSE_METHOD_ = PROCEDURE (bmp: AL_BITMAPptr; x, y, rx, ry, c: AL_INT); CDECL;
  _SPR_METHOD_ = PROCEDURE (bmp, spr: AL_BITMAPptr; x, y: AL_INT); CDECL;
  _RLE_SPR_METHOD_ = PROCEDURE (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr; x, y: AL_INT); CDECL;
  _BLIT_METHOD_ = PROCEDURE (org, dest: AL_BITMAPptr; o_x, o_y, d_x, d_y, w, h: AL_INT); CDECL;

(* The bitmap virtual table. *)
  AL_GFX_VTABLEptr = ^AL_GFX_VTABLE;
  AL_GFX_VTABLE = RECORD
    color_depth: AL_INT;
    mask_color: AL_INT;
    unwrite_bank: AL_PTR;  (* C function on some machines, asm on i386 *)
  { Note that you would use these methods directly but it isn't recommendable.
    Use the Allegro.pas Drawing Primitives instead. }
    set_clip: _BMP_METHOD_;
    acquire: _BMP_METHOD_;
    release: _BMP_METHOD_;
    create_sub_bitmap: FUNCTION (parent: AL_BITMAPptr; x, y, w, h: AL_INT): AL_BITMAPptr; CDECL;
    created_sub_bitmap: PROCEDURE (bmp, parent: AL_BITMAPptr); CDECL;
    getpixel: FUNCTION (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT; CDECL;
    putpixel: PROCEDURE (bmp: AL_BITMAPptr; x, y, color: AL_INT); CDECL;
    vline: _FLINE_METHOD_;
    hline: _FLINE_METHOD_;
    hfill: _FLINE_METHOD_;
    line: _LINE_METHOD_;
    fastline: _LINE_METHOD_;
    rectfill: _LINE_METHOD_;
    triangle: FUNCTION (bmp: AL_BITMAPptr; x1, y1, x2, y2, x3, y3, color: AL_INT): AL_INT; CDECL;
    draw_sprite: _SPR_METHOD_;
    draw_256_sprite: _SPR_METHOD_;
    draw_sprite_v_flip: _SPR_METHOD_;
    draw_sprite_h_flip: _SPR_METHOD_;
    draw_sprite_vh_flip: _SPR_METHOD_;
    draw_trans_sprite: _SPR_METHOD_;
    draw_trans_rgba_sprite: _SPR_METHOD_;
    draw_lit_sprite: PROCEDURE (bmp, sprite: AL_BITMAPptr; x, y, color: AL_INT); CDECL;
    draw_rle_sprite: _RLE_SPR_METHOD_;
    draw_trans_rle_sprite: _RLE_SPR_METHOD_;
    draw_trans_rgba_rle_sprite: _RLE_SPR_METHOD_;
    draw_lit_rle_sprite: PROCEDURE (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr; x, y, color: AL_INT); CDECL;
    draw_character: AL_PTR; { Unsuported methods were defined as simple pointers. }
    draw_glyph: AL_PTR;
    blit_from_memory: _BLIT_METHOD_;
    blit_to_memory: _BLIT_METHOD_;
    blit_from_system: _BLIT_METHOD_;
    blit_to_system: _BLIT_METHOD_;
    blit_to_self: _BLIT_METHOD_;
    blit_to_self_forward: _BLIT_METHOD_;
    blit_to_self_backward: _BLIT_METHOD_;
    blit_between_formats: _BLIT_METHOD_;
    masked_blit: _BLIT_METHOD_;
    clear_to_color: PROCEDURE (bmp: AL_BITMAPptr; color: AL_INT); CDECL;
    pivot_scaled_sprite_flip: PROCEDURE (bmp, sprite: AL_BITMAPptr; x, y, cx, cy, angle, scale: AL_FIXED; v_flip: AL_INT); CDECL;
    do_stretch_blit: PROCEDURE (source, dest: AL_BITMAPptr; s_x, s_y, s_w, s_h, d_x, d_y, d_w, d_h, masked: AL_INT); CDECL;
    draw_gouraud_sprite: AL_PTR;
    draw_sprite_end: AL_SIMPLE_PROC;
    blit_end: AL_SIMPLE_PROC;
    polygon: PROCEDURE (bmp: AL_BITMAPptr; vertices: AL_INT; points: AL_INTptr; color: AL_INT); CDECL;
    rect: _LINE_METHOD_;
    circle: _CIRCLE_METHOD_;
    circlefill: _CIRCLE_METHOD_;
    ellipse: _ELLIPSE_METHOD_;
    ellipsefill: _ELLIPSE_METHOD_;
    arc: AL_PTR;
    spline: AL_PTR;
    floodfill: PROCEDURE (bmp: AL_BITMAPptr; x, y, c: AL_INT); CDECL;
    polygon3d: AL_PTR;
    polygon3d_f: AL_PTR;
    triangle3d: AL_PTR;
    triangle3d_f: AL_PTR;
    quad3d: AL_PTR;
    quad3d_f: AL_PTR;
  END;



IMPLEMENTATION

END.

