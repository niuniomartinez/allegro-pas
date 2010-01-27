UNIT alvtable;
(*< @exclude
 *
 *      Virtual table to be used by AL_BITMAP.  FOR INTERNAL USE ONLY.
 *)

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
{$ENDIF}

INTERFACE

USES
  albase, alfixed;

TYPE
(* Some internal stuff to define virtual methods. *)
  AL_BITMAPptr = POINTER;
  AL_RLE_SPRITEptr = POINTER;
  _BMP_METHOD_ = PROCEDURE (bmp: AL_BITMAPptr); CDECL;
  _LINE_METHOD_ = PROCEDURE (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT); CDECL;
  _FLINE_METHOD_ = PROCEDURE (bmp: AL_BITMAPptr; a, b, c, d: LONGINT); CDECL;
  _CIRCLE_METHOD_ = PROCEDURE (bmp: AL_BITMAPptr; x, y, r, c: LONGINT); CDECL;
  _ELLIPSE_METHOD_ = PROCEDURE (bmp: AL_BITMAPptr; x, y, rx, ry, c: LONGINT); CDECL;
  _SPR_METHOD_ = PROCEDURE (bmp, spr: AL_BITMAPptr; x, y: LONGINT); CDECL;
  _RLE_SPR_METHOD_ = PROCEDURE (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr; x, y: LONGINT); CDECL;
  _BLIT_METHOD_ = PROCEDURE (org, dest: AL_BITMAPptr; o_x, o_y, d_x, d_y, w, h: LONGINT); CDECL;

(* The bitmap virtual table. *)
  AL_GFX_VTABLEptr = ^AL_GFX_VTABLE;
  AL_GFX_VTABLE = RECORD
  { Note that you would use these methods directly but it isn't recommendable.
    Use the Allegro.pas Drawing Primitives instead. }
    color_depth: LONGINT;
    mask_color: LONGINT;
    unwrite_bank: _BMP_METHOD_;  (* C function on some machines, asm on i386 *)
    set_clip: _BMP_METHOD_;
    acquire: _BMP_METHOD_;
    release: _BMP_METHOD_;
    create_sub_bitmap: FUNCTION (parent: AL_BITMAPptr; x, y, w, h: LONGINT): AL_BITMAPptr; CDECL;
    created_sub_bitmap: PROCEDURE (bmp, parent: AL_BITMAPptr); CDECL;
    getpixel: FUNCTION (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT; CDECL;
    putpixel: PROCEDURE (bmp: AL_BITMAPptr; x, y, color: LONGINT); CDECL;
    vline: _FLINE_METHOD_;
    hline: _FLINE_METHOD_;
    hfill: _FLINE_METHOD_;
    line: _LINE_METHOD_;
    fastline: _LINE_METHOD_;
    rectfill: _LINE_METHOD_;
    triangle: FUNCTION (bmp: AL_BITMAPptr; x1, y1, x2, y2, x3, y3, color: LONGINT): LONGINT; CDECL;
    draw_sprite: _SPR_METHOD_;
    draw_256_sprite: _SPR_METHOD_;
    draw_sprite_v_flip: _SPR_METHOD_;
    draw_sprite_h_flip: _SPR_METHOD_;
    draw_sprite_vh_flip: _SPR_METHOD_;
    draw_trans_sprite: _SPR_METHOD_;
    draw_trans_rgba_sprite: _SPR_METHOD_;
    draw_lit_sprite: PROCEDURE (bmp, sprite: AL_BITMAPptr; x, y, color: LONGINT); CDECL;
    draw_rle_sprite: _RLE_SPR_METHOD_;
    draw_trans_rle_sprite: _RLE_SPR_METHOD_;
    draw_trans_rgba_rle_sprite: _RLE_SPR_METHOD_;
    draw_lit_rle_sprite: PROCEDURE (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr; x, y, color: LONGINT); CDECL;
    draw_character: POINTER; { Unsuported methods were defined as simple pointers. }
    draw_glyph: POINTER;
    blit_from_memory: _BLIT_METHOD_;
    blit_to_memory: _BLIT_METHOD_;
    blit_from_system: _BLIT_METHOD_;
    blit_to_system: _BLIT_METHOD_;
    blit_to_self: _BLIT_METHOD_;
    blit_to_self_forward: _BLIT_METHOD_;
    blit_to_self_backward: _BLIT_METHOD_;
    blit_between_formats: _BLIT_METHOD_;
    masked_blit: _BLIT_METHOD_;
    clear_to_color: PROCEDURE (bmp: AL_BITMAPptr; color: LONGINT); CDECL;
    pivot_scaled_sprite_flip: PROCEDURE (bmp, sprite: AL_BITMAPptr; x, y, cx, cy, angle, scale: AL_FIXED; v_flip: LONGINT); CDECL;
    do_stretch_blit: PROCEDURE (source, dest: AL_BITMAPptr; s_x, s_y, s_w, s_h, d_x, d_y, d_w, d_h, masked: LONGINT); CDECL;
    draw_gouraud_sprite: PROCEDURE (bmp, spr: AL_BITMAPptr; x, y, c1, c2, c3, c4: LONGINT); CDECL;
    draw_sprite_end: AL_SIMPLE_PROC;
    blit_end: AL_SIMPLE_PROC;
    polygon: PROCEDURE (bmp: AL_BITMAPptr; vertices: LONGINT; points: PLONGINT; color: LONGINT); CDECL;
    rect: _LINE_METHOD_;
    circle: _CIRCLE_METHOD_;
    circlefill: _CIRCLE_METHOD_;
    ellipse: _ELLIPSE_METHOD_;
    ellipsefill: _ELLIPSE_METHOD_;
    arc: POINTER;
    spline: POINTER;
    floodfill: PROCEDURE (bmp: AL_BITMAPptr; x, y, c: LONGINT); CDECL;
    polygon3d: PROCEDURE (bmp: AL_BITMAPptr; _type: LONGINT; texture: AL_BITMAPptr; vc: LONGINT; vtx: POINTER); CDECL;
    polygon3d_f: PROCEDURE (bmp: AL_BITMAPptr; _type: LONGINT; texture: AL_BITMAPptr; vc: LONGINT; vtx: POINTER); CDECL;
    triangle3d: PROCEDURE (bmp: AL_BITMAPptr; _type: LONGINT; texture: AL_BITMAPptr; v1, v2, v3: POINTER); CDECL;
    triangle3d_f: PROCEDURE (bmp: AL_BITMAPptr; _type: LONGINT; texture: AL_BITMAPptr; v1, v2, v3: POINTER); CDECL;
    quad3d: PROCEDURE (bmp: AL_BITMAPptr; _type: LONGINT; texture: AL_BITMAPptr; v1, v2, v3, v4: POINTER); CDECL;
    quad3d_f: PROCEDURE (bmp: AL_BITMAPptr; _type: LONGINT; texture: AL_BITMAPptr; v1, v2, v3, v4: POINTER); CDECL;

    draw_sprite_ex: PROCEDURE (bmp, sprite: AL_BITMAPptr; x, y, mode, flip:
      LONGINT);
  END;



IMPLEMENTATION

END.

