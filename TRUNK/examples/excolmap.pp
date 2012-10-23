PROGRAM excolmap;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/
 *
 *	This program demonstrates how to create custom graphic effects with the
 *	al_create_color_table function.  Allegro drawing routines are affected
 *	by any color table you might have set up.  In the first part of this
 *	example, a greyscale color table is set.  The result is that a simple
 *	rectfill call, instead of drawing a rectangle with color zero, uses the
 *	already drawn pixels to determine the pixel to be drawn (read the
 *	comment of ReturnGreyColor for a precise description of the algorithm).
 *	In the second part of the test, the color table is changed to be an
 *	inverse table, meaning that any pixel drawn will be shown as its color
 *	values had been inverted.
 *
 *	by Ñuño Martínez <>
 *	from an example of Allegro Game Library by Grzegorz Ludorowski.
 *
 *	See readme.txt for license and copyright information.
 *)

{$IFDEF FPC}
{ Free Pascal. }
  {$LONGSTRINGS ON}
{$ENDIF}

USES
  allegro,
  alvga;    { 8 bit paletted mode special effects. }



VAR
(* RGB -> color mapping table. Not needed, but speeds things up. *)
  rgb_table: AL_RGB_MAP;
(* Greyscale & negative color mapping table. *)
  greyscale_table, negative_table: AL_COLOR_MAP;
  pal: AL_PALETTE;
  background: AL_BITMAPptr;
  temp: AL_BITMAPptr;



(* Here comes our custom function.  It's designed to take the input colors
 * (red, green & blue) and return a greyscale color for it. This way, when
 * any drawing function draws say over green, it draws the greyscale color
 * for green.
 * 'pal' is the palette we are looking in to find the colors.
 * Now, imagine we want to draw a pixel with color A, over color B.
 * Once the table is created, set, and the drawing mode is TRANSLUCENT, then
 * A is the 'x' color passed to the function and B is the 'y' color passed
 * to the function.
 * Since we want a greyscale effect with no matter what A (or 'x') color, we
 * ignore it and use y to look at the palette.
 * NOTE:
 * When you return the rgb value, you don't need to search the palette for
 * the nearest color, Allegro does this automatically.
 *)
  PROCEDURE ReturnGreyColor (pal: AL_PALETTE; x, y: LONGINT; rgb: AL_RGBptr);
	CDECL;
  VAR
    C: INTEGER;
  BEGIN
  { first create the greyscale color. }
    c := TRUNC (pal[y].r * 0.3 + pal[y].g * 0.5 + pal[y].b * 0.2);

  { Now assign to our rgb triplet the palette greyscale color... }
    rgb^.r := c; rgb^.g := c; rgb^.b := c;
  END;



(* The negative_color function is quite the same like the grayscale one,
 * since we are ignoring the value of the drawn color (aka x).
 *)
  PROCEDURE ReturnNegativeColor (pal: AL_PALETTE; x, y: LONGINT;
				 rgb: AL_RGBptr); CDECL;
  BEGIN
  { To get the negative color, substract the color values of red, green
    and blue from the full (63) color value. }
    rgb^.r := 63 - pal[y].r;
    rgb^.g := 63 - pal[y].g;
    rgb^.b := 63 - pal[y].b;
  END;


  PROCEDURE GenerateBackground;
  VAR
    I: INTEGER;
  BEGIN
  { First get some usual colors. }
    al_generate_332_palette (pal);

  { Now remap the first 64 for a perfect greyscale gradient. }
    FOR i := 0 TO 63 DO
    BEGIN
      pal[i].r := i;
      pal[i].g := i;
      pal[i].b := i;
    END;

  { Draws some things on the screen using not-greyscale colors. }
    FOR i := 1 TO 3000 DO
    BEGIN
      al_circlefill (background, Random (320), Random (200),
		 Random (25), 64 + Random (192));
    END;
  END;



VAR
  x, y, deltax, deltay: INTEGER;

BEGIN { The program starts here. }
  deltax := 1; deltay := 1;

  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
{ This activates the vsync signal if no one is provided by the system. }
  al_install_timer;

  temp := al_create_bitmap (320, 200);
  background := al_create_bitmap (320, 200);

  GenerateBackground;

{ This isn't needed, but it speeds up the color table calculations. }
  al_create_rgb_table (rgb_table, pal, NIL);
  al_color_table := @rgb_table;

{ Build a color lookup table for greyscale effect. }
  al_create_color_table (@greyscale_table, pal, @ReturnGreyColor, NIL);

{ Build a color lookup table for negative effect. }
  al_create_color_table (@negative_table, pal, @ReturnNegativeColor, NIL);

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 320, 200, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
      BEGIN
	al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
      { Shows an error message. }
	al_message (al_error);
	EXIT;
      END;

  al_set_palette (pal);

{ We have set the drawing mode to TRANS.  This makes all the drawing functions
  use the general color_map table, which is _NOT_ translucent, since we are
  using a custom color_map table. }
  al_drawing_mode (AL_DRAW_MODE_TRANS, NIL, 0, 0);

{ Select the greyscale table. }
  al_color_table := @greyscale_table;

  x := 50; y := 50;
  al_blit (background, temp, 0, 0, 0, 0, 320, 200);
  al_rectfill (temp, x, y, x+50, y+50, 0);

  al_blit (temp, al_screen, 0, 0, 0, 0, 320, 200);

  WHILE NOT al_keypressed DO
  BEGIN
    INC (x, deltax);
    INC (y, deltay);

    IF (x < 1) OR (x > 320-50) THEN
      deltax := (-deltax);

    IF (y < 1) OR (y > 200-50) THEN
      deltay := (-deltay);

    al_blit (background, temp, 0, 0, 0, 0, 320, 200);
    al_textout_centre_ex (temp, al_font, 'Greyscale effect',
			AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2,
			al_makecol (0, 0, 255), -1);
    al_rectfill (temp, x, y, x+50, y+50, 0);
    al_vsync;
    al_blit (temp, al_screen, 0, 0, 0, 0, 320, 200);
  END;

  al_clear_keybuf;

{ Now it's time for the negative part. The negative example is easier to see
  with greyscale colors.  Therefore we will change the color of the background
  to a greyscale one, but only in a restricted area... }

  al_rectfill (background, AL_SCREEN_H DIV 4, AL_SCREEN_H DIV 4,
		background^.w - AL_SCREEN_W DIV 4,
		background^.h - AL_SCREEN_H DIV 4, 0);

{ This should go inside the next loop, but since we won't use the background
  image any more, we can optimize it's speed printing the text now. }

  al_textout_centre_ex (background, al_font, 'Negative effect',
			AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2,
			al_makecol (0, 0, 0), -1);

{ Switch the active color table... }
  al_color_table := @negative_table;

  al_blit (background, temp, 0, 0, 0, 0, 320, 200);
  al_rectfill (temp, x, y, x+50, y+50, 0);

  al_blit (temp, al_screen, 0, 0, 0, 0, 320, 200);

  WHILE NOT al_keypressed DO
  BEGIN
    INC (x, deltax);
    INC (y, deltay);

    IF (x < 1) OR (x > 320-50) THEN
      deltax := (-deltax);

    IF (y < 1) OR (y > 200-50) THEN
      deltay := (-deltay);

    al_blit (background, temp, 0, 0, 0, 0, 320, 200);
    al_rectfill (temp, x, y, x+50, y+50, 0);
    al_vsync;
    al_blit (temp, al_screen, 0, 0, 0, 0, 320, 200);
  END;

{ Shutdown Allegro. }
  al_destroy_bitmap (background);
  al_destroy_bitmap (temp);

{ End of the program. }
END.
