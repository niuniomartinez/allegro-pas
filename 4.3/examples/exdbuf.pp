PROGRAM exdbuf;
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
 *	This program demonstrates the use of double buffering.
 *	It moves a circle across the screen, first just erasing and
 *	redrawing directly to the screen, then with a double buffer.
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
{ It needs some Allegro.pas units. }
  allegro,
  albltspr, { Image blitting and sprite drawing. }
  aldraw,   { Drawing primitives. }
  algraph,  { Graphic mode configuration. }
  altext;   { Text drawing. }



VAR
  buffer: AL_BITMAPptr;
  c: INTEGER;

BEGIN { The program starts here. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
{ This activates the vsync signal if no one is provided by the system. }
  al_install_timer;


  al_install_keyboard;

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

{ Allocate the memory buffer. }
  buffer := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);

{ First without any buffering...
  Note use of the global al_retrace_counter to control the speed. We also
  compensate screen size (AL_GFX_SAFE) with a virtual 320 screen width.  }
  al_clear_keybuf();
  c := al_retrace_count + 32;
  WHILE al_retrace_count - c <= 320 + 32 DO
  BEGIN
    al_acquire_screen;
      al_clear_to_color (al_screen, al_makecol (255, 255, 255));
      al_circlefill (al_screen, (al_retrace_count - c) * AL_SCREEN_W DIV 320,
		     AL_SCREEN_H DIV 2, 32, al_makecol (0, 0, 0));
      al_textout_ex (al_screen, al_font, 'No buffering', 0, 0,
		     al_makecol (0, 0, 0), -1);
    al_release_screen();
    IF al_keypressed THEN BREAK;
  END;

{ And now with a double buffer... }
  al_clear_keybuf;
  c := al_retrace_count + 32;
  WHILE al_retrace_count - c <= 320 + 32 DO
  BEGIN
    al_clear_to_color (buffer, al_makecol (255, 255, 255));
    al_circlefill (buffer, (al_retrace_count - c) * AL_SCREEN_W DIV 320,
		   AL_SCREEN_H DIV 2, 32, al_makecol (0, 0, 0));
    al_textout_ex (buffer, al_font, 'Double buffering', 0, 0,
		   al_makecol (0, 0, 0), -1);
    al_blit (buffer, al_screen, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);
    IF al_keypressed THEN BREAK;
  END;

  al_destroy_bitmap (buffer);

{ Shutdown Allegro. }
  al_exit;

{ End of the program. }
END.

