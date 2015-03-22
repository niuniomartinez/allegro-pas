PROGRAM exxfade;
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
 *	This program demonstrates how to load and display bitmap files
 *	in truecolor video modes, and how to crossfade between them.
 *	You have to use this example from the command line to specify
 *	as parameters a number of graphic files. Use at least two
 *	files to see the graphical effect. The example will crossfade
 *	from one image to another with each key press until you press
 *	the ESC key.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See README file for license and copyright information.
 *)

  USES
    allegro, alblend;

  VAR
    Arguments: ARRAY [0..3] OF STRING = (
      'exxfade', 'allegro.pcx', 'mysha.pcx', 'planet.pcx'
    );



  FUNCTION Show (Name: STRING): INTEGER;
  VAR
    Bmp, Buffer: AL_BITMAPptr;
    Pal: AL_PALETTE;
    Alpha: INTEGER;
  BEGIN
  { load the file }
    Bmp := al_load_bitmap (Name, @Pal);
    IF Bmp = NIL THEN
    BEGIN
      Show := -1;
      EXIT;
    END;

    Buffer := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);
    al_blit (al_screen, Buffer, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);

    al_set_palette(pal);

  { fade it in on top of the previous picture }
    FOR Alpha := 0 TO (255 DIV 8) DO
    BEGIN
      al_set_trans_blender (0, 0, 0, Alpha * 8);
      al_draw_trans_sprite (Buffer, Bmp,
	(AL_SCREEN_W - Bmp^.w) DIV 2, (AL_SCREEN_H - Bmp^.h) DIV 2);
      al_vsync;
      al_blit (Buffer, al_screen, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);
      IF al_keypressed THEN
      BEGIN
	 al_destroy_bitmap (Bmp);
	 al_destroy_bitmap (Buffer);
	 IF (al_readkey AND $FF) = 27 THEN
	    Show := 1
	 ELSE
	    Show := 0;
	EXIT;
      END;

    { slow down animation for modern machines }
      al_rest (2);
    END;

    al_blit (Bmp, al_screen,
	0, 0, (AL_SCREEN_W - bmp^.w) DIV 2, (AL_SCREEN_H - Bmp^.h) DIV 2,
	Bmp^.w, Bmp^.h);

    al_destroy_bitmap(bmp);
    al_destroy_bitmap(buffer);

    IF (al_readkey AND $FF) = 27 THEN
      Show := 1
    ELSE
      Show := 0;
  END;



  VAR
    i: INTEGER;

BEGIN
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;

{ use a default set of images if the user doesn't specify any }
  IF ParamCount > 1 THEN
    FOR i := 0 TO ParamCount DO
      Arguments[i] := ParamStr (i);

  al_install_keyboard;
  al_install_timer;

{ set the best color depth that we can find }
  al_set_color_depth (16);
  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
  BEGIN
    al_set_color_depth (15);
    IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
    BEGIN
      al_set_color_depth (32);
      IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
      BEGIN
	al_set_color_depth (24);
	IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
	BEGIN
	  al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
	  al_message ('Error setting graphics mode'#10+al_error+''#10);
	    EXIT;
	END;
      END;
    END;
  END;

{ load all images in the same color depth as the display }
  al_set_color_conversion (AL_COLORCONV_TOTAL);

{ process all the files on our command line }
  i := 1;
  WHILE TRUE DO
  BEGIN
    CASE Show (Arguments[i]) OF
    -1:
    { error }
      BEGIN
	al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
	al_message ('Error loading image file '''+Arguments[i]+''''#10);
	EXIT;
      END;
    0:
    { ok! }
      ;
    1:
    { quit }
      BEGIN
	al_exit;
	EXIT;
      END;
    END;

    INC (i);
    IF i >= Length (Arguments) THEN
      i := 1;
  END;
END.
