UNIT aldraw;
(*< Drawing primitives.

    All these routines are affected by the current drawing mode and the
    clipping rectangle of the destination bitmap.  Unless specified otherwise,
    all coordinates for drawing operations are inclusive, and they, as well as
    lengths, are specified in pixel units. *)

INTERFACE

USES
  albase, albitmap; { Needs some basic definitions. }



(* Each bitmap has an associated clipping rectangle, which is the area of the
   image that it is OK to draw onto.  Nothing will be drawn to positions
   outside this space.  This function sets the clipping rectangle for the
   specified bitmap.  Pass the coordinates of the top-left and bottom-right
   corners of the clipping rectangle in this order;  these are both inclusive,
   i.e. @code(al_set_clip_rect @(bitmap, 16, 16, 32, 32@)) will allow drawing
   to (16, 16) and (32, 32), but not to (15, 15) and (33, 33).

   Drawing operations will be performed (at least partially) on the bitmap as
   long as the first coordinates of its clipping rectangle are not greater than
   the second coordinates and its intersection with the actual image is
   non-empty.  If either condition is not fulfilled, drawing will be turned off
   for the bitmap, e.g.: @code(al_set_clip_rect @(bmp, 0, 0, -1, -1@)) will
   disable drawing on @code(bmp).

   Note that passing "out-of-bitmap" coordinates is allowed, but they are
   likely to be altered (and so the coordinates returned by
   @link(al_get_clip_rect) will be different).  However, such modifications are
   guaranteed to preserve the external effect of the clipping rectangle, that
   is not to modify the actual area of the image that it is OK to draw onto. *)
  PROCEDURE al_set_clip_rect (bmp: AL_BITMAPptr; x1, y1, x2, y2: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_clip_rect';

(* Sets the clipping rectangle of the specified bitmap as the intersection of
   its current clipping rectangle and the rectangle described by the four
   coordinates. *)
  PROCEDURE al_add_clip_rect (bmp: AL_BITMAPptr; x1, y1, x2, y2: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'add_clip_rect';

(* Returns the clipping rectangle for the specified bitmap.  *)
  PROCEDURE al_get_clip_rect (bmp: AL_BITMAPptr; VAR x1, y1, x2, y2: LONGINT);

(* Turns on (if state is @true) or off (if state is @false) clipping for the
   specified bitmap.  Turning clipping off may slightly speed up some drawing
   operations (usually a negligible difference, although every little helps)
   but will result in your program dying a horrible death if you try to draw
   beyond the edges of the bitmap. *)
  PROCEDURE al_set_clip_state (bmp: AL_BITMAPptr; state: BOOLEAN);



(* Drawing modes. *)
CONST
(* Flag for @link(al_drawing_mode).

   The pixels of the bitmap being drawn onto are simply replaced by those
   produced by the drawing function. *)
  AL_DRAW_MODE_SOLID		= 0;
(* Flag for @link(al_drawing_mode).

   Pixels are written to the bitmap with an exclusive-or operation rather than
   a simple copy, so drawing the same shape twice will erase it.  Because it
   involves reading as well as writing the bitmap memory, xor drawing is a lot
   slower than the normal replace mode. *)
  AL_DRAW_MODE_XOR		= 1;
(* Flag for @link(al_drawing_mode).

   Pixels are simply copied from the pattern bitmap onto the destination
   bitmap.  This allows the use of multicolored patterns, and means that the
   color you pass to the drawing routine is ignored.  This is the fastest of
   the patterned modes. *)
  AL_DRAW_MODE_COPY_PATTERN	= 2;
(* Flag for @link(al_drawing_mode).

   Each pixel in the pattern bitmap is compared with the mask color, which is
   zero in 256-color modes or bright pink for truecolor data (maximum red and
   blue, zero green).  If the pattern pixel is solid, a pixel of the color you
   passed to the drawing routine is written to the destination bitmap,
   otherwise a zero is written.  The pattern is thus treated as a monochrome
   bitmask, which lets you use the same pattern to draw different shapes in
   different colors, but prevents the use of multicolored patterns. *)
  AL_DRAW_MODE_SOLID_PATTERN	= 3;
(* Flag for @link(al_drawing_mode).

   It is almost the same as @link(AL_DRAW_MODE_SOLID_PATTERN), but the masked
   pixels are skipped rather than being written as zeros, so the background
   shows through the gaps. *)
  AL_DRAW_MODE_MASKED_PATTERN	= 4;
(* Flag for @link(al_drawing_mode).

   The global @link(al_color_map) table or truecolor blender functions are
   used to overlay pixels on top of the existing image.  This must only be used
   after you have set up the color mapping table (for 256 color modes) or
   blender functions (for truecolor modes).  Because it involves reading as
   well as writing the bitmap memory, translucent drawing is very slow if you
   draw directly to video RAM, so wherever possible you should use a memory
   bitmap instead. *)
  AL_DRAW_MODE_TRANS		= 5;



(* Sets the graphics drawing mode.  This only affects the geometric routines
   like @link(al_putpixel), lines, rectangles, circles, polygons, floodfill,
   etc, not the text output, blitting, or sprite drawing functions.  The mode
   should be one of the following constants:
@unorderedList(
  @item(@link(AL_DRAW_MODE_SOLID))
  @item(@link(AL_DRAW_MODE_XOR))
  @item(@link(AL_DRAW_MODE_COPY_PATTERN))
  @item(@link(AL_DRAW_MODE_SOLID_PATTERN))
  @item(@link(AL_DRAW_MODE_MASKED_PATTERN))
  @item(@link(AL_DRAW_MODE_TRANS))
)
   With the patterned modes, you provide a pattern bitmap which is tiled across
   the surface of the shape.  Allegro stores a pointer to this bitmap rather
   than copying it, so you must not destroy the bitmap while it is still
   selected as the pattern.  The width and height of the pattern must be powers
   of two, but they can be different, eg. a 64x16 pattern is fine, but a 17x3
   one is not.  The pattern is tiled in a grid starting at point
   @code(@(x_anchor, y_anchor@)).  Normally you should just pass zero for these
   values, which lets you draw several adjacent shapes and have the patterns
   meet up exactly along the shared edges.  Zero alignment may look peculiar if
   you are moving a patterned shape around the screen, however, because the
   shape will move but the pattern alignment will not, so in some situations
   you may wish to alter the anchor position. *)
  PROCEDURE al_drawing_mode (mode: LONGINT; pattern: AL_BITMAPptr;
			     x_anchor, y_anchor: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'drawing_mode';

(* This is a shortcut for toggling xor drawing mode on and off.  Calling
   @code(al_xor_mode @(1@)) is equivalent to @link(al_drawing_mode)
   @code(@(AL_DRAW_MODE_XOR, NIL, 0, 0@)).  Calling @code(al_xor_mode @(0@)) is
   equivalent to @code(al_drawing_mode @(A_DRAW_MODE_SOLID, NIL, 0, 0@)). *)
  PROCEDURE al_xor_mode (aOn: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'xor_mode';

(* This is a shortcut for selecting solid drawing mode.  It is equivalent to
   calling @link(al_drawing_mode) @code(@(AL_DRAW_MODE_XOR, NIL, 0, 0@)). *)
  PROCEDURE al_solid_mode; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'solid_mode';



(* Clears the bitmap to color 0. *)
  PROCEDURE al_clear_bitmap (bitmap: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_bitmap';

(* Clears the bitmap to the specified color. *)
  PROCEDURE al_clear_to_color (bitmap: AL_BITMAPptr; color: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_to_color';

(* Reads a pixel from point (x, y) in the bitmap.

   @returns(-1 if the point lies outside the bitmap @(ignoring the clipping
     rectangle@), otherwise the value of the pixel in the color format of the
     bitmap.

     @bold(Warning:) -1 is also a valid value for pixels contained in 32-bit
     bitmaps with alpha channel @(when R,G,B,A are all equal to 255@) so you
     can't use the test against -1 as a predicate for such bitmaps.  In this
     cases, the only reliable predicate is check if it is inside the bitmap.

     To extract the individual color components, use the getr() / getg() / getb() / geta() family of functions) *)
  FUNCTION  al_getpixel	   (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;

(* Writes a pixel to the specified position in the bitmap, using the current
   drawing mode and the bitmap's clipping rectangle. *)
  PROCEDURE al_putpixel	   (bmp: AL_BITMAPptr; x, y, color: LONGINT);

(* Draws a vertical line onto the bitmap, from point (x, y1) to (x, y2). *)
  PROCEDURE al_vline	   (bmp: AL_BITMAPptr; x, y1, y2, color: LONGINT);

(* Draws a horizontal line onto the bitmap, from point (x1, y) to (x2, y). *)
  PROCEDURE al_hline	   (bmp: AL_BITMAPptr; x1, y, x2, color: LONGINT);

(* Draws a line onto the bitmap, from point (x1, y1) to (x2, y2). *)
  PROCEDURE al_line	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);

(* Faster version of the previous function.  Note that pixel correctness is not
   guaranteed for this function. *)
  PROCEDURE al_fastline	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);

(* Draws an outline rectangle with the two points as its opposite corners. *)
  PROCEDURE al_rect	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);

(* Draws a solid, filled rectangle with the two points as its opposite
   corners. *)
  PROCEDURE al_rectfill	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);

(* Draws a circle with the specified centre and radius. *)
  PROCEDURE al_circle	   (bmp: AL_BITMAPptr; x, y, r, color: LONGINT);

(* Draws a filled circle with the specified centre and radius. *)
  PROCEDURE al_circlefill  (bmp: AL_BITMAPptr; x, y, r, color: LONGINT);

(* Draws an ellipse with the specified centre and radius. *)
  PROCEDURE al_ellipse	   (bmp: AL_BITMAPptr; x, y, rx, ry, color: LONGINT);

(* Draws a filled ellipse with the specified centre and radius. *)
  PROCEDURE al_ellipsefill (bmp: AL_BITMAPptr; x, y, rx, ry, color: LONGINT);

(* Floodfills an enclosed area, starting at point (x, y), with the specified
   color. *)
  PROCEDURE al_floodfill   (bmp: AL_BITMAPptr; x, y, color: LONGINT);

(* Draws a filled polygon with an arbitrary number of corners.  Pass the number
   of vertices and an array containing a series of x, y points (a total of
   vertices*2 values). *)
  PROCEDURE al_polygon     (bmp: AL_BITMAPptr; vertices: LONGINT; points: ARRAY OF LONGINT; color: LONGINT);



IMPLEMENTATION

(* Clipping. *)
PROCEDURE al_get_clip_rect (bmp: AL_BITMAPptr; VAR x1, y1, x2, y2: LONGINT);
BEGIN
  x1 := bmp^.cl;
  y1 := bmp^.ct;
  x2 := bmp^.cl-1;
  y2 := bmp^.cb-1;
END;



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



END.

