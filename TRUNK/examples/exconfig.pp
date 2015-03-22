PROGRAM exconfig;
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
 *	This is a very simple program showing how to use the Allegro
 *	configuration (ini file) routines.  A first look at the example
 *	shows nothing more than a static graphic and the wait for
 *	a key press.  However, the way this graphic is displayed is
 *	configured through a custom exconfig.ini file which is loaded
 *	manually.  From this file the example obtains parameters like
 *	fullscreen/windowed mode, a specific graphic resolution to set
 *	up, which graphic to show, how to blit it on the screen, etc.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Lennart Steinke.
 *
 *	See README file for license and copyright information.
 *)

USES
  allegro, sysutils;


VAR
   w, h, bpp: INTEGER;
   windowed: INTEGER;
   count: INTEGER;
   data: STRING;

   title: STRING;
   filename: STRING;
   r, g, b: INTEGER;

   background: AL_BITMAPptr;
   display: INTEGER;
   pal: AL_PALETTE;

   x, y: INTEGER;

BEGIN { The program starts here. }

{ You should always do this at the start of Allegro programs. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;

{ Set up the keyboard handler. }
  al_install_keyboard;

{ Save the current ini file, then set the program specific one. }
  al_push_config_state;
  al_set_config_file ('exconfig.ini');

{ The gfx mode is stored in 3 variables. }
  w := al_get_config_int ('graphics', 'width', 320);
  h := al_get_config_int ('graphics', 'height', 200);
  bpp := al_get_config_int ('graphics', 'bpp', 8);

{ Should we use a windowed mode?
  In the config file this is stored as either FALSE or TRUE.
  So we need to read a string and see what it contains.
  If the entry is not found, we use "FALSE" by default. }
  IF al_get_config_string ('graphics', 'windowed', 'TRUE') = 'FALSE' THEN
    windowed := AL_GFX_AUTODETECT_FULLSCREEN
  ELSE
    windowed := AL_GFX_AUTODETECT_WINDOWED;

{ The title string. }
  title := al_get_config_string ('content', 'headline', '<no headline>');

{ The title color.
  This is stored as three ints in one line, so we need to extract them. }
  data := al_get_config_string ('content', 'headercolor', '255 255 255');
  count := Pos (' ', data);
  r := StrToInt (Copy (data, 1, count - 1));
  data := Copy (data, count + 1, length (data));
  count := Pos (' ', data);
  g := StrToInt (Copy (data, 1, count - 1));
  b := StrToInt (Copy (data, count + 1, length (data)));

{ The image file to read. }
  filename := al_get_config_string ('content', 'image', 'mysha.pcx');

{ And it's tiling mode. }
  display := al_get_config_int ('content', 'display', 0);
  IF (display < 0) OR (display > 2) THEN
  BEGIN
    al_message ('content.display must be within 0..2'#10);
    display := 0;
  END;

{ Restore the old config file. }
  al_pop_config_state;

{ Set the graphics mode. }
  al_set_color_depth (bpp);
  IF NOT al_set_gfx_mode (windowed, w, h, 0, 0) THEN
  BEGIN
    al_message ('Unable to set mode '+IntToStr(w)+'x'+IntToStr(h)+
		' with '+IntToStr(bpp)+'bpp'#10);
    EXIT;
  END;

{ Clear the screen. }
  al_clear_bitmap (al_screen);

{ Load the image. }
  background := al_load_bitmap (filename, @pal);
  IF background <> NIL THEN
  BEGIN
    al_set_palette (pal);

    CASE display OF
    0: { Stretch }
      al_stretch_blit (background, al_screen, 0, 0, background^.w,
			background^.h, 0, 0, AL_SCREEN_W, AL_SCREEN_H);
    1: { Center }
      al_blit (background, al_screen, 0, 0,
	(AL_SCREEN_W - background^.w)DIV 2, (AL_SCREEN_H - background^.h)DIV 2,
	background^.w, background^.h);
    2: { Tile }
      BEGIN
	y := 0;
	WHILE y < AL_SCREEN_H DO
	BEGIN
	  x := 0;
	  WHILE x < AL_SCREEN_W DO
	  BEGIN
	    al_blit (background, al_screen, 0, 0, x, y,
		     background^.w, background^.h);
	    INC (x, background^.w);
	  END;
	  INC (y, background^.h);
	END;
      END;
    END;
    al_destroy_bitmap (background);
  END
  ELSE
    al_textout_centre_ex (al_screen, al_font, filename+' not found',
			  AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2,
			  al_makecol (r,g,b), -1);

  al_textout_centre_ex (al_screen, al_font, title,
			AL_SCREEN_W DIV 2, 20, al_makecol (r,g,b), -1);
  al_readkey();

{ End of the program. }
END.
