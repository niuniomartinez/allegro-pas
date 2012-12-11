PROGRAM exscale;
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
 *	This example demonstrates how to use PCX files, palettes and stretch
 *	lits.  It loads a PCX file, sets its palette and does some random
 *	al_stretch_blits.  Don't worry - it's VERY slowed down using
 *	al_vsync().
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Grzegorz Ludorowski.
 *
 *	See README file for license and copyright information.
 *)

USES
  sysutils,
  allegro;

VAR
  MyPalette: AL_PALETTE;
  ScrBuffer: AL_BITMAPptr;
  PCXName: STRING;

BEGIN { The program starts here. }

  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 640, 480, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 640, 480, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;

  PCXName := ExtractFilePath (ParamStr (0)) + 'mysha.pcx';
  ScrBuffer := al_load_pcx (PCXName, @MyPalette);
  IF ScrBuffer = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
  { Show an error message. }
    al_message ('Error reading '+PCXName);
    EXIT;
  END;

  al_set_palette (MyPalette);
  al_blit (ScrBuffer, al_screen, 0, 0, 0, 0, ScrBuffer^.w, ScrBuffer^.h);

  WHILE NOT al_keypressed DO
  BEGIN
    al_vsync;
    al_stretch_blit (ScrBuffer, al_screen, 0, 0, RANDOM (ScrBuffer^.w),
		RANDOM (ScrBuffer^.h), RANDOM (AL_SCREEN_W), RANDOM (AL_SCREEN_H),
		   RANDOM (AL_SCREEN_W), RANDOM (AL_SCREEN_H));
  END;

  al_destroy_bitmap (ScrBuffer);
END.
