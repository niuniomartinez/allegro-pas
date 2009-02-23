(* Drawing primitives. *)
UNIT aldraw;

{ Defines the frame. }
{$MODE Delphi}
{$PACKRECORDS C}
{$H+}



INTERFACE

USES
  albitmap; { Needs some basic definitions. }


VAR
(* Clipping. *)
  al_set_clip_rect: PROCEDURE (bmp: AL_BITMAPptr; x1, y1, x2, y2: LONGINT); CDECL;
  al_add_clip_rect: PROCEDURE (bmp: AL_BITMAPptr; x1, y1, x2, y2: LONGINT); CDECL;
  PROCEDURE al_set_clip_state (bmp: AL_BITMAPptr; state: BOOLEAN);

(* Drawing modes. *)
CONST
(* Flags for "al_drawing_mode". *)
  AL_DRAW_MODE_SOLID		= 0;
  AL_DRAW_MODE_XOR		= 1;
  AL_DRAW_MODE_COPY_PATTERN	= 2;
  AL_DRAW_MODE_SOLID_PATTERN	= 3;
  AL_DRAW_MODE_MASKED_PATTERN	= 4;
  AL_DRAW_MODE_TRANS		= 5;

VAR
  al_drawing_mode: PROCEDURE (mode: LONGINT; pattern: AL_BITMAPptr;
			     x_anchor, y_anchor: LONGINT); CDECL;

  al_xor_mode: PROCEDURE (aOn: LONGINT); CDECL;

  al_solid_mode: PROCEDURE; CDECL;

(* Clears the given bitmap. *)
  al_clear_bitmap: PROCEDURE (bitmap: AL_BITMAPptr); CDECL;
  al_clear_to_color: PROCEDURE (bitmap: AL_BITMAPptr; color: LONGINT); CDECL;

(* Drawing primitives. *)
  FUNCTION  al_getpixel	   (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;
  PROCEDURE al_putpixel	   (bmp: AL_BITMAPptr; x, y, color: LONGINT);
  PROCEDURE al_vline	   (bmp: AL_BITMAPptr; x, y1, y2, color: LONGINT);
  PROCEDURE al_hline	   (bmp: AL_BITMAPptr; x1, y, x2, color: LONGINT);
  PROCEDURE al_line	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
  PROCEDURE al_fastline	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
  PROCEDURE al_rect	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
  PROCEDURE al_rectfill	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
  PROCEDURE al_circle	   (bmp: AL_BITMAPptr; x, y, r, color: LONGINT);
  PROCEDURE al_circlefill  (bmp: AL_BITMAPptr; x, y, r, color: LONGINT);
  PROCEDURE al_ellipse	   (bmp: AL_BITMAPptr; x, y, rx, ry, color: LONGINT);
  PROCEDURE al_ellipsefill (bmp: AL_BITMAPptr; x, y, rx, ry, color: LONGINT);
  PROCEDURE al_floodfill   (bmp: AL_BITMAPptr; x, y, color: LONGINT);
  PROCEDURE al_polygon     (bmp: AL_BITMAPptr; vertices: LONGINT; points: ARRAY OF LONGINT; color: LONGINT);



IMPLEMENTATION

USES
  albase, dynlibs;



(* Clipping. *)
PROCEDURE al_set_clip_state (bmp: AL_BITMAPptr; state: BOOLEAN);
BEGIN
  IF state THEN
    bmp^.clip := -1
  ELSE
    bmp^.clip := 0;
END;



(* Drawing primitives. *)
FUNCTION al_getpixel (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;
BEGIN
  al_getpixel := bmp^.vtable^.getpixel (bmp, x, y);
END;

PROCEDURE al_putpixel (bmp: AL_BITMAPptr; x, y, color: LONGINT);
BEGIN
  bmp^.vtable^.putpixel (bmp, x, y, color);
END;

PROCEDURE al_vline (bmp: AL_BITMAPptr; x, y1, y2, color: LONGINT);
BEGIN
  bmp^.vtable^.vline (bmp, x, y1, y2, color);
END;

PROCEDURE al_hline (bmp: AL_BITMAPptr; x1, y, x2, color: LONGINT);
BEGIN
  bmp^.vtable^.hline (bmp, x1, y, x2, color);
END;

PROCEDURE al_line (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
BEGIN
  bmp^.vtable^.line (bmp, x1, y1, x2, y2, color);
END;

PROCEDURE al_fastline (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
BEGIN
  bmp^.vtable^.fastline (bmp, x1, y1, x2, y2, color);
END;

PROCEDURE al_rect (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
BEGIN
  bmp^.vtable^.rect (bmp, x1, y1, x2, y2, color);
END;

PROCEDURE al_rectfill (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
BEGIN
  bmp^.vtable^.rectfill (bmp, x1, y1, x2, y2, color);
END;

PROCEDURE al_circle (bmp: AL_BITMAPptr; x, y, r, color: LONGINT);
BEGIN
  bmp^.vtable^.circle (bmp, x, y, r, color);
END;

PROCEDURE al_circlefill (bmp: AL_BITMAPptr; x, y, r, color: LONGINT);
BEGIN
  bmp^.vtable^.circlefill (bmp, x, y, r, color);
END;

PROCEDURE al_ellipse (bmp: AL_BITMAPptr; x, y, rx, ry, color: LONGINT);
BEGIN
  bmp^.vtable^.ellipse (bmp, x, y, rx, ry, color);
END;

PROCEDURE al_ellipsefill (bmp: AL_BITMAPptr; x, y, rx, ry, color: LONGINT);
BEGIN
  bmp^.vtable^.ellipsefill (bmp, x, y, rx, ry, color);
END;

PROCEDURE al_floodfill (bmp: AL_BITMAPptr; x, y, color: LONGINT);
BEGIN
  bmp^.vtable^.floodfill (bmp, x, y, color);
END;

PROCEDURE al_polygon (bmp: AL_BITMAPptr; vertices: LONGINT; points: ARRAY OF LONGINT; color: LONGINT);
BEGIN
  bmp^.vtable^.polygon (bmp, vertices, @points[0], color);
END;



INITIALIZATION
{ Get procedure address. }
  @al_set_clip_rect := GetProcAddress (__al_library_id__, 'set_clip_rect');
  @al_add_clip_rect := GetProcAddress (__al_library_id__, 'add_clip_rect');
  @al_drawing_mode := GetProcAddress (__al_library_id__, 'drawing_mode');
  @al_xor_mode := GetProcAddress (__al_library_id__, 'xor_mode');
  @al_solid_mode := GetProcAddress (__al_library_id__, 'solid_mode');
  @al_clear_bitmap := GetProcAddress (__al_library_id__, 'clear_bitmap');
  @al_clear_to_color := GetProcAddress (__al_library_id__, 'clear_to_color');
END.

