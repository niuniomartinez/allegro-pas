UNIT aldraw;
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
 *	Drawing primitives.
 *	by Ñuño Martínez <>
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
  albase, albitmap; { Needs some basic definitions. }

(* Clipping. *)
  PROCEDURE al_set_clip_rect (bmp: AL_BITMAPptr; x1, y1, x2, y2: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_clip_rect';
  PROCEDURE al_add_clip_rect (bmp: AL_BITMAPptr; x1, y1, x2, y2: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'add_clip_rect';
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

  PROCEDURE al_drawing_mode (mode: AL_INT; pattern: AL_BITMAPptr;
			     x_anchor, y_anchor: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'drawing_mode';

  PROCEDURE al_xor_mode (aOn: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'xor_mode';

  PROCEDURE al_solid_mode; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'solid_mode';

(* Clears the given bitmap. *)
  PROCEDURE al_clear_bitmap (bitmap: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_bitmap';
  PROCEDURE al_clear_to_color (bitmap: AL_BITMAPptr; color: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_to_color';

(* Drawing primitives. *)
  FUNCTION  al_getpixel	   (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT;
  PROCEDURE al_putpixel	   (bmp: AL_BITMAPptr; x, y, color: AL_INT);
  PROCEDURE al_vline	   (bmp: AL_BITMAPptr; x, y1, y2, color: AL_INT);
  PROCEDURE al_hline	   (bmp: AL_BITMAPptr; x1, y, x2, color: AL_INT);
  PROCEDURE al_line	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
  PROCEDURE al_fastline	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
  PROCEDURE al_rect	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
  PROCEDURE al_rectfill	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
  PROCEDURE al_circle	   (bmp: AL_BITMAPptr; x, y, r, color: AL_INT);
  PROCEDURE al_circlefill  (bmp: AL_BITMAPptr; x, y, r, color: AL_INT);
  PROCEDURE al_ellipse	   (bmp: AL_BITMAPptr; x, y, rx, ry, color: AL_INT);
  PROCEDURE al_ellipsefill (bmp: AL_BITMAPptr; x, y, rx, ry, color: AL_INT);
  PROCEDURE al_floodfill   (bmp: AL_BITMAPptr; x, y, color: AL_INT);
  PROCEDURE al_polygon     (bmp: AL_BITMAPptr; vertices: AL_INT; points: ARRAY OF AL_INT; color: AL_INT);



IMPLEMENTATION

(* Clipping. *)
PROCEDURE al_set_clip_state (bmp: AL_BITMAPptr; state: BOOLEAN);
BEGIN
  IF state THEN
    bmp^.clip := -1
  ELSE
    bmp^.clip := 0;
END;



(* Drawing primitives. *)
FUNCTION al_getpixel (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT;
BEGIN
  al_getpixel := bmp^.vtable^.getpixel (bmp, x, y);
END;

PROCEDURE al_putpixel (bmp: AL_BITMAPptr; x, y, color: AL_INT);
BEGIN
  bmp^.vtable^.putpixel (bmp, x, y, color);
END;

PROCEDURE al_vline (bmp: AL_BITMAPptr; x, y1, y2, color: AL_INT);
BEGIN
  bmp^.vtable^.vline (bmp, x, y1, y2, color);
END;

PROCEDURE al_hline (bmp: AL_BITMAPptr; x1, y, x2, color: AL_INT);
BEGIN
  bmp^.vtable^.hline (bmp, x1, y, x2, color);
END;

PROCEDURE al_line (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
BEGIN
  bmp^.vtable^.line (bmp, x1, y1, x2, y2, color);
END;

PROCEDURE al_fastline (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
BEGIN
  bmp^.vtable^.fastline (bmp, x1, y1, x2, y2, color);
END;

PROCEDURE al_rect (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
BEGIN
  bmp^.vtable^.rect (bmp, x1, y1, x2, y2, color);
END;

PROCEDURE al_rectfill (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
BEGIN
  bmp^.vtable^.rectfill (bmp, x1, y1, x2, y2, color);
END;

PROCEDURE al_circle (bmp: AL_BITMAPptr; x, y, r, color: AL_INT);
BEGIN
  bmp^.vtable^.circle (bmp, x, y, r, color);
END;

PROCEDURE al_circlefill (bmp: AL_BITMAPptr; x, y, r, color: AL_INT);
BEGIN
  bmp^.vtable^.circlefill (bmp, x, y, r, color);
END;

PROCEDURE al_ellipse (bmp: AL_BITMAPptr; x, y, rx, ry, color: AL_INT);
BEGIN
  bmp^.vtable^.ellipse (bmp, x, y, rx, ry, color);
END;

PROCEDURE al_ellipsefill (bmp: AL_BITMAPptr; x, y, rx, ry, color: AL_INT);
BEGIN
  bmp^.vtable^.ellipsefill (bmp, x, y, rx, ry, color);
END;

PROCEDURE al_floodfill (bmp: AL_BITMAPptr; x, y, color: AL_INT);
BEGIN
  bmp^.vtable^.floodfill (bmp, x, y, color);
END;

PROCEDURE al_polygon (bmp: AL_BITMAPptr; vertices: AL_INT; points: ARRAY OF AL_INT; color: AL_INT);
BEGIN
  bmp^.vtable^.polygon (bmp, vertices, @points[0], color);
END;



END.

