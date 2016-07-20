PROGRAM extimer;
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
 *    This program demonstrates how to use the timer routines.
 *    These can be a bit of a pain, because you have to be sure
 *    you lock all the memory that is used inside your interrupt
 *    handlers.  The first part of the example shows a basic use of
 *    timing using the blocking function al_rest. The second part
 *    shows how to use three timers with different frequencies in
 *    a non blocking way.
 *
 *	by Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See README file for license and copyright information.
 *)

USES
  allegro,
  sysutils;


VAR
  c, x, y, z: INTEGER;



(* timer interrupt handler *)
  PROCEDURE inc_x; CDECL;
  BEGIN
    INC (x);
  END;

  PROCEDURE inc_y; CDECL;
  BEGIN
    INC (y);
  END;

  PROCEDURE inc_z; CDECL;
  BEGIN
    INC (z);
  END;



BEGIN { The program starts here. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Error initializing Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_timer;

{ Set a graphics mode. }
  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 320, 200, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;

  al_set_palette (al_desktop_palette);
  al_clear_to_color (al_screen, al_makecol (255, 255, 255));

{ use al_rest to delay for a specified number of milliseconds }
  al_textout_centre_ex (
    al_screen, al_font,
    'Timing five second:',
    AL_SCREEN_W DIV 2, 48,
    al_makecol (0, 0, 0), al_makecol (255, 255, 255)
  );

  FOR c := 1 TO 5 DO
  BEGIN
    al_textout_centre_ex (
      al_screen, al_font,
      IntToStr (c),
      AL_SCREEN_W DIV 2, 62 + c * 10,
      al_makecol (0, 0, 0), al_makecol (255, 255, 255)
    );
    al_rest(1000);
    IF al_keypressed THEN
      EXIT;
  END;

  al_textout_centre_ex (
    al_screen, al_font,
    'Press a key to set up interrupts',
    AL_SCREEN_W DIV 2, 142,
    al_makecol (0, 0, 0), al_makecol (255, 255, 255)
  );
  al_readkey();

{ the speed can be specified in milliseconds (this is once a second) }
  al_install_int (@inc_x, 1000);

{ or in beats per second (this is 10 ticks a second) }
  al_install_int_ex (@inc_y, AL_BPS_TO_TIMER (10));

{ or in seconds (this is 10 seconds a tick) }
  al_install_int_ex (@inc_z, AL_SECS_TO_TIMER (10));

{ Initialize variables. }
  x := 0; y := 0; z := 0;

{ the interrupts are now active... }
  WHILE NOT al_keypressed DO
  BEGIN
    al_textprintf_centre_ex (
      al_screen, al_font,
      AL_SCREEN_W DIV 2, 176,
      al_makecol (0, 0, 0), al_makecol (255, 255, 255),
      'x=%d, y=%d, z=%d', [x, y, z]
    );
    al_rest(1);
  END;
END.

END_OF_MAIN()
