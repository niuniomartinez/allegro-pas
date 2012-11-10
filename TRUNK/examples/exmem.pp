PROGRAM exmem;
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
 *	This program demonstrates the use of memory bitmaps. It creates
 *	a small temporary bitmap in memory, draws some circles onto it,
 *	and then blits lots of copies of it onto the screen.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See README file for license and copyright information.
 *)

  USES
    allegro;

VAR
  MemoryBitmap: AL_BITMAPptr;
  x, y: INTEGER;
BEGIN { The program starts here. }

  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;

{ Set up the keyboard handler. }
  al_install_keyboard;

{ Set a graphics mode sized 320x200. }
  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 320, 200, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;

  al_set_palette (al_desktop_palette);

{ make a memory bitmap sized 20x20 }
  MemoryBitmap := al_create_bitmap (20, 20);

{ draw some circles onto it }
  al_clear_bitmap (MemoryBitmap);
  FOR x := 0 TO 15 DO
    al_circle (MemoryBitmap, 10, 10, x, al_palette_color^[x]);

{ blit lots of copies of it onto the screen }
  al_acquire_screen;

  FOR y := AL_SCREEN_H DIV 20 DOWNTO 0 DO
    FOR x := AL_SCREEN_W DIV 20 DOWNTO 0 DO
      al_blit (MemoryBitmap, al_screen, 0, 0, x * 20, y * 20, 20, 20);
 
  al_release_screen;

{ free the memory bitmap }
  al_destroy_bitmap (MemoryBitmap);

  al_readkey;
END.
