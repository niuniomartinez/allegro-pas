PROGRAM exhsvrgb;
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
 *	This program shows how to convert HSV values on to RGB ones.
 *	
 *	by Ñuño Martínez <>
 *
 *	See readme.txt for license and copyright information.
 *)

{$H+}

USES
  sysutils,
{ It needs some Allegro.pas units. }
  albase,   { Base definitions. }
  alcolor,  { Color manipulation. }
  aldraw,   { Drawing primitives. }
  algraph,  { Graphic mode configuration. }
  alkeybrd, { Keyboard input. }
  alpalete, { Color palette manipulation. }
  alsystem, { System initialization. }
  altext;   { Text drawing. }



  (* test:
   *   Does the color test. *)
  PROCEDURE test (colordepth: INTEGER);
  VAR
    pal: AL_PALETTE;
    x, y: INTEGER;
    h, s, v: DOUBLE; { Hue, saturation, value. }
    r, g, b: LONGINT;   { Red, green, blue. }
  BEGIN
  { Set the screen mode }
    al_set_color_depth (colordepth);

    IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
      IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 640, 480, 0, 0) THEN
	EXIT;

  { In case this is a 256 color mode, we'd better make sure that the
    palette is set to something sensible. This function generates a
    standard palette with a nice range of different colors... }
    al_generate_332_palette (@pal);
    al_set_palette (pal);

    al_acquire_screen;

    al_clear_to_color (al_screen, al_makecol (0, 0, 0));

    al_textout_ex (al_screen, al_font, INTTOSTR (colordepth)+' bit color...',
		   0, 0, al_makecol (255, 255, 255), -1);

    al_textout_centre_ex (al_screen, al_font, 'HSV -> RGB demonstration',
			  AL_SCREEN_W DIV 2, 16,
			  al_makecol (255, 255, 255), -1);

  { Draw some nice smooth color gradients... }
    FOR x := 0 TO 300 DO
    BEGIN
      FOR y := 0 TO 300 DO
      BEGIN
      { Calculate hue. }
	h := (y / 300.0) * 360; { From 0 to 360. }
	IF x < 151 THEN { Left side, hue and value. }
	BEGIN
	{ Calculate value. }
	  v := x / 150.0; { From 0 to 1. }
	{ Calculate red, green and blue values. }
	  al_hsv_to_rgb (h, 1, v, r, g, b);
	END
	ELSE BEGIN { Right side, hue and saturation. }
	{ Calculate saturation. }
	  s := (150 - (x - 150)) / 150.0; { From 1 to 0. }
	{ Calculate red, green and blue values. }
	  al_hsv_to_rgb (h, s, 1, r, g, b);
	END;
      { Draw the pixel. }
        al_putpixel (al_screen, 192 + x, 50 + y, al_makecol (r, g, b));
      END;
    END;

    al_textout_centre_ex (al_screen, al_font, '<press a key>',
			  AL_SCREEN_W DIV 2, AL_SCREEN_H - 16,
			  al_makecol (255, 255, 255), -1);

    al_release_screen;

    al_readkey();
END;



BEGIN { The program starts here. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;

{ Try each of the possible possible color depths... }
  test ( 8);
  test (15);
  test (16);
  test (24);
  test (32);

{ Shutdown Allegro. }
  al_exit;

{ End of the program. }
END.
