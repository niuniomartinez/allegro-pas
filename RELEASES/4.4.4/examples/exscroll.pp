PROGRAM exscroll;
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
 *	This program demonstrates how to use hardware scrolling.
 *	The scrolling should work on anything that supports virtual
 *	screens larger than the physical screen.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an examplo of the Allegro Game Library, by Shawn Hargreaves.
 *
 *	See README file for license and copyright information.
 *)

  USES
    allegro;

  VAR
    Scroller: AL_BITMAPptr;
    Black: AL_RGB = (r: 0; g: 0; b: 0; filler: 0);
    x, NextX, h: INTEGER;

BEGIN
  x := 0;
  h :=100;

  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 320, 240, 640, 240) THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Unable to set a 320x240 mode with 540x240 virtual dimensions'#10);
    EXIT;
  END;

{ the scrolling area is twice the width of the screen (640x240) }
  Scroller := al_create_sub_bitmap (al_screen, 0, 0, AL_SCREEN_W * 2, AL_SCREEN_H);

  al_set_palette (al_desktop_palette);
  al_set_color (0, @Black);

  al_rectfill (Scroller, 0, 0, AL_SCREEN_W, 100, 6);
  al_rectfill (Scroller, 0, 100, AL_SCREEN_W, AL_SCREEN_H, 2);

  REPEAT
  { advance the scroller, wrapping every 320 pixels }
    NextX := x + 1;
    IF NextX >= 320 THEN
      NextX := 0;

  { draw another column of the landscape }
    al_acquire_bitmap (Scroller);
    al_vline (Scroller, NextX + AL_SCREEN_W - 1, 0, h, 6);
    al_vline (Scroller, NextX + AL_SCREEN_W - 1, h + 1, AL_SCREEN_H, 2);
    al_release_bitmap (Scroller);

  { scroll the screen }
    al_scroll_screen (NextX, 0);

  { duplicate the landscape column so we can wrap the scroller }
    IF NextX > 0 THEN
    BEGIN
      al_acquire_bitmap (Scroller);
      al_vline (Scroller, x, 0, h, 6);
      al_vline (Scroller, x, h + 1, AL_SCREEN_H, 2);
      al_release_bitmap (Scroller);
    END;

  { randomly alter the landscape position }
    IF RANDOM (2) = 1 THEN
    BEGIN
      IF h > 5 THEN
	DEC (h);
    END
    ELSE BEGIN
      IF h < 195 THEN
	INC (h);
    END;

    x := NextX;

    al_rest (0);
  UNTIL al_keypressed;

  al_destroy_bitmap (Scroller);

  al_clear_keybuf;

END.
