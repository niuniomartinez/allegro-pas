PROGRAM extruec;
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
 *	This program shows how to specify colors in the various different
 *	truecolor pixel formats. The example shows the same screen (a few
 *	text lines and three coloured gradients) in all the color depth
 *	modes supported by your video card. The more color depth you have,
 *	the less banding you will see in the gradients.
 *	
 *	by Ñuño Martínez <>
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See readme.txt for license and copyright information.
 *)

{$IFDEF FPC}
{ Free Pascal. }
  {$LONGSTRINGS ON}
{$ENDIF}

USES
  sysutils,
  allegro;



  (* test:
   *   Does the color test. *)
  PROCEDURE test (colordepth: INTEGER);
  VAR
    pal: AL_PALETTE;
    x: INTEGER;
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

  { use the al_makecol function to specify RGB values... }
    al_textout_ex (al_screen, al_font, 'Red',     32,  80, al_makecol (255,   0,   0), al_makecol (0, 0, 0));
    al_textout_ex (al_screen, al_font, 'Green',   32, 100, al_makecol (  0, 255,   0), al_makecol (0, 0, 0));
    al_textout_ex (al_screen, al_font, 'Blue',    32, 120, al_makecol (  0,   0, 255), al_makecol (0, 0, 0));
    al_textout_ex (al_screen, al_font, 'Yellow',  32, 140, al_makecol (255, 255,   0), al_makecol (0, 0, 0));
    al_textout_ex (al_screen, al_font, 'Cyan',    32, 160, al_makecol (  0, 255, 255), al_makecol (0, 0, 0));
    al_textout_ex (al_screen, al_font, 'Magenta', 32, 180, al_makecol (255,   0, 255), al_makecol (0, 0, 0));
    al_textout_ex (al_screen, al_font, 'Grey',    32, 200, al_makecol (128, 128, 128), al_makecol (0, 0, 0));

  { or we could draw some nice smooth color gradients... }
    FOR x := 0 TO 255 DO
    BEGIN
      al_vline (al_screen, 192 + x, 112, 176, al_makecol (x, 0, 0));
      al_vline (al_screen, 192 + x, 208, 272, al_makecol (0, x, 0));
      al_vline (al_screen, 192 + x, 304, 368, al_makecol (0, 0, x));
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

{ Yry each of the possible possible color depths... }
  test ( 8);
  test (15);
  test (16);
  test (24);
  test (32);

{ Shutdown Allegro. }
  al_exit;

{ End of the program. }
END.
