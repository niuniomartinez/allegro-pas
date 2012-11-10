PROGRAM exsprite;
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
 *	This example demonstrates al_rotate_scaled_sprite functions.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Trent Gamblin.
 *
 *	See README file for license and copyright information.
 *)

USES
  allegro, alblend, alfixed;

TYPE
  MODES = (NORMAL, ALPHA, LIT);

VAR
   dBuf, b, b2: AL_BITMAPptr;
   Mode: MODES;
   fAngle, fScale, Increment: DOUBLE;
   Direction : INTEGER;
   Angle, Scale: AL_FIXED;
   X, Y: INTEGER;

BEGIN { The program starts here. }
   Mode := NORMAL;
   fAngle := 0;
   fScale := 1;
   Increment := 0.002;
   Direction := -1;

  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;

  al_set_color_depth (32);
  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
  BEGIN
    IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 640, 480, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Shows an error message. }
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;
  END;

  dBuf := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);
  IF dBuf = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Could not create double buffer.');
    EXIT;
  END;

{ Load the alpha bitmap.  This is used for alpha and normal modes. }
  al_set_color_conversion (AL_COLORCONV_KEEP_ALPHA);
  b := al_load_bitmap ('inkblot.tga', NIL);
  IF b = NIL THEN
  BEGIN
    al_destroy_bitmap (dBuf);
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Could not load inkblot.tga.');
    EXIT;
  END;

{ Make a non-alpha copy for drawing lit. }
  b2 := al_create_bitmap (b^.w, b^.h);
  IF b2 = NIL THEN
  BEGIN
    al_destroy_bitmap (dBuf);
    al_destroy_bitmap (b);
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Could not load inkblot.tga.');
    EXIT;
  END;
  al_clear_to_color (b2, al_makeacol (255, 255, 255, 255));
  al_set_alpha_blender;
  al_draw_trans_sprite (b2, b, 0, 0);

  WHILE al_key[AL_KEY_ESC] = 0 DO
  BEGIN
    Angle := al_ftofix (fAngle);
    Scale := al_ftofix (fScale);
    x := TRUNC (AL_SCREEN_W - (b^.w * fScale)) DIV 2;
    y := TRUNC (AL_SCREEN_H - (b^.h * fscale)) DIV 2;

  { XXX flickers badly due to no buffering. }
    al_clear_to_color (dBuf, al_makecol (255, 255, 255));
    al_textout_centre_ex (dbuf, al_font, 'Press ''n'' for next mode.', AL_SCREEN_W DIV 2, AL_SCREEN_H - 30, al_makecol (0, 0, 0), -1);
    CASE Mode OF
    NORMAL:
      BEGIN
	al_rotate_scaled_sprite (dbuf, b, x, y, Angle, Scale);
	al_textout_centre_ex (dbuf, al_font, 'Normal', AL_SCREEN_W DIV 2, AL_SCREEN_H - 20, al_makecol (0, 0, 0), -1);
      END;
    ALPHA:
      BEGIN
	al_set_alpha_blender;
	al_rotate_scaled_sprite_trans (dbuf, b, x, y, Angle, Scale);
	al_textout_centre_ex (dbuf, al_font, 'Alpha', AL_SCREEN_W DIV 2, AL_SCREEN_H - 20, al_makecol (0, 0, 0), -1);
      END;
    LIT:
      BEGIN
	al_set_trans_blender (255, 0, 0, 0);
	al_rotate_scaled_sprite_lit (dbuf, b2, x, y, Angle, Scale, 128);
	al_textout_centre_ex (dbuf, al_font, 'Lit', AL_SCREEN_W DIV 2, AL_SCREEN_H - 20, al_makecol (0, 0, 0), -1);
      END;
    END;
    al_blit (dbuf, al_screen, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);

    fAngle := fAngle + 1;
    fScale := fScale + Direction * Increment;
    IF fScale < 0.5 THEN
    BEGIN
      Direction := -Direction;
      fScale := 0.5;
    END
    ELSE IF fScale > 1 THEN
    BEGIN
      Direction := -Direction;
      fScale := 1;
    END;

  { Comment next line to make it faster. }
    al_rest (5);

    IF al_keypressed THEN
      IF al_readkey SHR 8 = AL_KEY_N THEN
	IF Mode = LIT THEN
	  Mode := NORMAL
	ELSE
	  INC (Mode);
  END;

  al_destroy_bitmap (b);
  al_destroy_bitmap (b2);
  al_destroy_bitmap (dBuf);

{ End of the program. }
END.
