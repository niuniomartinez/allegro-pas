PROGRAM exmouse;
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
 *	This program demonstrates how to get mouse input.  The
 *	first part of the test retrieves the raw mouse input data
 *	and displays it on the screen without using any mouse
 *	cursor.  When you press a key the standard arrow-like mouse
 *	cursor appears.  You are not restricted to this shape,
 *	and a second keypress modifies the cursor to be several
 *	concentric colored circles.  They are not joined together,
 *	so you can still see bits of what's behind when you move the
 *	cursor over the printed text message.
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
{ It needs some Allegro.pas units. }
  allegro,
  aldraw,   { Drawing primitives. }
  algraph,  { Graphic mode configuration. }
  almouse,  { Mouse input. }
  altext;   { Text drawing. }



  (* MyPrintf:
   *   Helper function. *)
  PROCEDURE MyPrintf (x, y: INTEGER; Fmt: STRING; Value: INTEGER);
  VAR
    text_buff: STRING;
  BEGIN
    FmtStr (text_buff, Fmt, [Value]);
    al_textout_ex (al_screen, al_font, text_buff, x, y, al_makecol(0, 0, 0),
		   al_makecol (255, 255, 255));
  END;



VAR
  mickeyx, mickeyy: LONGINT;
  custom_cursor: AL_BITMAPptr;
  c: INTEGER;
BEGIN { The program starts here. }

  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_timer;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 320, 200, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message (al_error);
    { Shutdown Allegro. }
      al_exit;
      EXIT;
    END;

  al_set_palette (al_desktop_palette);
  al_clear_to_color (al_screen, al_makecol (255, 255, 255));

{ Detect mouse presence } 
  IF al_install_mouse < 0 THEN
  BEGIN
    al_textout_centre_ex (al_screen, al_font,
			  'No mouse detected, but you need one!',
			  AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2,
			  al_makecol (0, 0, 0),	al_makecol (255, 255, 255));
    al_readkey;
  { Shutdown Allegro. }
    al_exit;
    EXIT;
  END;

  c := 0;

  WHILE NOT al_keypressed DO
  BEGIN
  { On most platforms (eg. DOS) things will still work correctly
    without this call, but it is a good idea to include it in any
    programs that you want to be portable, because on some platforms
    you may not be able to get any mouse input without it. }
    al_poll_mouse;

    al_acquire_screen;

  { The mouse position is stored in the variables mouse_x and mouse_y. }
    MyPrintf (16, 48, 'mouse_x = %-5d', al_mouse_x);
    MyPrintf (16, 64, 'mouse_y = %-5d', al_mouse_y);

  { Or you can use this function to measure the speed of movement.
    Note that we only call it every fourth time round the loop:
    there's no need for that other than to slow the numbers down
    a bit so that you will have time to read them... }
    INC (c);
    IF (c AND 3) = 0 THEN
      al_get_mouse_mickeys (mickeyx, mickeyy);

    MyPrintf (16,  88, 'mickey_x = %-7d', mickeyx);
    MyPrintf (16, 104, 'mickey_y = %-7d', mickeyy);

  { The mouse button state is stored in the variable al_mouse_b. }
    IF (al_mouse_b AND 1) <> 0 THEN
      al_textout_ex (al_screen, al_font, 'left button is pressed ', 16, 128,
		     al_makecol (0, 0, 0), al_makecol (255, 255, 255))
    ELSE
      al_textout_ex (al_screen, al_font, 'left button not pressed ', 16, 128,
		     al_makecol (0, 0, 0), al_makecol (255, 255, 255));

    IF (al_mouse_b AND 2) <> 0 THEN
      al_textout_ex (al_screen, al_font, 'right button is pressed ', 16, 144,
		     al_makecol (0, 0, 0), al_makecol (255, 255, 255))
    ELSE
      al_textout_ex (al_screen, al_font, 'right button not pressed ', 16, 144,
		     al_makecol (0, 0, 0), al_makecol (255, 255, 255));

    IF (al_mouse_b AND 4) <> 0 THEN
      al_textout_ex (al_screen, al_font, 'middle button is pressed ', 16, 160,
		     al_makecol (0, 0, 0), al_makecol (255, 255, 255))
    ELSE
      al_textout_ex (al_screen, al_font, 'middle button not pressed ', 16, 160,
		     al_makecol (0, 0, 0), al_makecol (255, 255, 255));

  { The wheel position is stored in the variable al_mouse_z. }
    MyPrintf (16, 184, 'mouse_z = %-5d', al_mouse_z);

    al_release_screen;

    al_vsync;

  END;

  al_clear_keybuf;

{ To display a mouse pointer, call al_show_mouse().  There are several
  things you should be aware of before you do this, though.  For one,
  it won't work unless you call al_install_timer() first.  For another,
  you must never draw anything onto the screen while the mouse
  pointer is visible. So before you draw anything, be sure to turn 
  the mouse off with al_show_mouse(NIL), and turn it back on again when
  you are done. }
  al_clear_to_color (al_screen, al_makecol (255, 255, 255));
  al_textout_centre_ex (al_screen, al_font, 'Press a key to change cursor',
			AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2,
			al_makecol (0, 0, 0),
			al_makecol (255, 255, 255));
  al_show_mouse (al_screen);
  al_readkey;
  al_show_mouse (NIL);

{ Create a custom mouse cursor bitmap... }
  custom_cursor := al_create_bitmap (32, 32);
  al_clear_to_color (custom_cursor, al_bitmap_mask_color (al_screen));
  FOR c := 0 TO 7 DO
    al_circle (custom_cursor, 16, 16, c*2, c);

{ Select the custom cursor and set the focus point to the middle of it. }
  al_set_mouse_sprite (custom_cursor);
  al_set_mouse_sprite_focus (16, 16);

  al_clear_to_color (al_screen, al_makecol (255, 255, 255));
  al_textout_centre_ex (al_screen, al_font, 'Press a key to quit',
			AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2,
			al_makecol (0, 0, 0),
			al_makecol (255, 255, 255));
  al_show_mouse (al_screen);
  al_readkey;
  al_show_mouse (NIL);

  al_destroy_bitmap (custom_cursor);

{ Shutdown Allegro. }
  al_exit;

{ End of the program. }
END.

