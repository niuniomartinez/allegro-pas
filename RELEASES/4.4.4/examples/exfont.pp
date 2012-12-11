PROGRAM exfont;
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
 *	This is a very simple program showing how to load and manipulate fonts.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Evert Glebbeek.
 *
 *	See README file for license and copyright information.
 *)

  USES
    allegro;

  VAR
    f1, f2, f3, f4, f5: AL_FONTptr;
    MyFont: AL_FONTptr;

BEGIN (* The program starts here. *)

{ You should always do this at the start of Allegro programs. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;

  al_install_keyboard;

{ set a graphics mode sized 320x200 }
  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 320, 200, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;

{ set the color palette }
  al_set_palette (al_desktop_palette);

{ clear the screen to white }
  al_clear_to_color (al_screen, al_makecol (255, 255, 255));

{ We will use the lower case letters from Allegro's normal font and the
  uppercase letters from the font in unifont.dat }
  f1 := al_load_font ('unifont.dat', NIL, NIL);
  IF f1 = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Cannot find unifont.dat in current directory.'#10);
    EXIT;
  END;

{ Extract character ranges }
  f2 := al_extract_font_range (al_font, Ord (' '), Ord ('A') - 1);
  f3 := al_extract_font_range (f1,      Ord ('A'), Ord ('Z'));
  f4 := al_extract_font_range (al_font, Ord ('Z') + 1, Ord ('z'));

{ Merge fonts }
  f5 := al_merge_fonts (f2, f3);
  MyFont := al_merge_fonts (f4, f5);

{ Destroy temporary fonts }
  al_destroy_font (f1);
  al_destroy_font (f2);
  al_destroy_font (f3);
  al_destroy_font (f4);
  al_destroy_font (f5);

{ write some text to the screen with black letters and transparent
  background }
  al_textout_centre_ex (al_screen, MyFont, 'Hello, World!',
	AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2,
	al_makecol (0,0,0), -1);

{ wait for a key press }
  al_readkey;

{ Release resources before exit. }
  al_destroy_font (MyFont);
END.
