PROGRAM expal;
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
 *	This program demonstrates how to manipulate the palette. It draws a set
 *	of concentric circles onto the screen and animates them by cycling
 *	the palette.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See README file for license and copyright information.
 *)

USES
  allegro;



VAR
  palette: AL_PALETTE;
  temp: AL_RGB;
  c: INTEGER;

BEGIN { The program starts here. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
{ This activates the vsync signal if no one is provided by the system. }
  al_install_timer;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;

{ First set the palette to black to hide what we are doing. }
  al_set_palette (al_black_palette);

{ Draw some circles onto the screen. }
  al_acquire_screen;

  FOR c := 255 DOWNTO 1 DO
    al_circlefill (al_screen, AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2, c, c);

  al_release_screen;

{ Fill our palette with a gradually altering sequence of colors. }
  FOR c :=0 TO 63 DO
  BEGIN
    palette[c].r := 63;
    palette[c].g := 0;
    palette[c].b := 63 - c;
  END;
  FOR c := 64 TO 127 DO
  BEGIN
    palette[c].r := 127-c;
    palette[c].g := c-64;
    palette[c].b := 0;
  END;
  FOR c := 128 TO 191 DO
  BEGIN
    palette[c].r := 0;
    palette[c].g := 191-c;
    palette[c].b := c-128;
  END;
  FOR c := 192 TO 255 DO
  BEGIN
    palette[c].r := c - 192;
    palette[c].g := 0;
    palette[c].b := 63;
  END;

{ Animate the image by rotating the palette. }
  WHILE NOT al_keypressed DO
  BEGIN
    temp := palette[255];
    FOR c := 255 DOWNTO 1 DO
    BEGIN
      palette[c] := palette[c-1];
    END;
    palette[0] := temp;
    al_set_palette (palette);
    al_vsync;
  END;

{ End of the program. }
END.
