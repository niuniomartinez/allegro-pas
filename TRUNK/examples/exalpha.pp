PROGRAM exalpha;
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
 *	This program demonstrates how to use the 32 bit RGBA
 *	nslucency functions to store an alpha channel along with
 *	a bitmap graphic.  Two images are loaded from disk. One will
 *	be used for the background and the other as a sprite. The
 *	example generates an alpha channel for the sprite image,
 *	composing the 32 bit RGBA bitmap during runtime, and draws
 *	it at the position of the mouse cursor.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See README file for license and copyright information.
 *)


  USES
    sysutils,
  { It needs some Allegro.pas units. }
    allegro, alblend;



(* Helper function. *)
  FUNCTION CLAMP (a, b, c: LONGINT): LONGINT; INLINE;
  BEGIN
    IF b < a THEN
      CLAMP := a
    ELSE IF b > c THEN
      CLAMP := c
    ELSE
      CLAMP := b;
  END;



VAR
  buf, filename: STRING;
  background, alpha, sprite, buffer: AL_BITMAPptr;
  bpp, x, y, c, a: LONGINT;
  ret: BOOLEAN;
  color_depths: ARRAY [0..4] OF INTEGER = ( 15, 16, 32, 24, 0 );

BEGIN { The program starts here. }
  bpp := -1;
  ret := TRUE;

  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_timer;
  al_install_mouse;

{ What color depth should we use? }
  IF Paramcount > 0 THEN
  BEGIN
    Buf := ParamStr (1);
    IF (Buf[1] = '-') OR (Buf[1] = '/') THEN
    BEGIN
      DELETE (Buf, 1, 1);
      bpp := StrToInt (Buf);
      IF NOT bpp IN [16, 15, 32, 24] THEN
      BEGIN
	al_message ('Invalid color depth ' + Buf);
	EXIT;
      END;
    END;
  END;

{ Set the graphics mode. }
  IF bpp > 0 THEN
  BEGIN
  { Set a user-requested color depth. }
    al_set_color_depth (bpp);
    ret := al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0);
  END
  ELSE BEGIN
  { Autodetect what color depths are available. }
    a := 0;
    REPEAT
      bpp := color_depths[a];
      al_set_color_depth (bpp);
      ret := al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0);
      IF ret THEN BREAK;
      INC (a);
    UNTIL color_depths[a] < 1;
  END;

{ Did the video mode set properly? }
  IF NOT ret THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
  { Show an error message. }
    al_message ('Unable to set any graphic mode'#10+al_error+''#10);
    EXIT;
  END;

{ load the background picture }
  filename := 'allegro.pcx';
  background := al_load_bitmap (filename, NIL);
  IF background = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Error reading allegro.pcx!');
    EXIT;
  END;

{ make a copy of it }
  al_set_color_depth (32);
  sprite := al_create_bitmap (background^.w, background^.h);
  al_blit (background, sprite, 0, 0, 0, 0, background^.w, background^.h);

{ load the alpha sprite image. Note that we specifically force this
  to load in a 32 bit format by calling al_set_color_depth(). That is
  because the disk file is actually only a 256 color graphic: if it
  was already a 32 bit RGBA sprite, we would probably want to use
  al_set_color_conversion(AL_COLORCONV_NONE) instead. }
  filename := 'mysha.pcx';
  alpha := al_load_bitmap (filename, NIL);
  IF alpha = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Error reading mysha.pcx!');
    EXIT;
  END;

{ normally we would have loaded an RGBA image directly from disk. Since
 I don't have one lying around, and am too lazy to draw one (or I could
 rationalise this by saying that I'm trying to save download size by
 reusing graphics :-) I'll just have to generate an alpha channel in
 code. I do this by using greyscale values from the mouse picture as an
 alpha channel for the Allegro image. Don't worry about this code: you
 wouldn't normally need to write anything like this, because you'd just
 get the right graphics directly out of a datafile. }
  al_drawing_mode (AL_DRAW_MODE_TRANS, NIL, 0, 0);
  al_set_write_alpha_blender;

  FOR Y := 0 TO (sprite^.h - 1) DO
  BEGIN
    FOR x := 0 TO (sprite^.w - 1) DO
    BEGIN
    { NOTE: Don't know why al_getpixel returns an incorrect value. }
      c := al_getpixel (alpha, x, y);
      a := al_getr (c) + al_getg (c) + al_getb (c);
      a := CLAMP (0, a DIV 2 - 128, 255);

      al_putpixel (sprite, x, y, a);
    END;
  END;

  al_destroy_bitmap (alpha);

  al_set_color_depth (bpp);

{ darken the background image down a bit }
  al_drawing_mode (AL_DRAW_MODE_TRANS, NIL, 0, 0);
  al_set_multiply_blender (0, 0, 0, 255);
  al_rectfill (background, 0, 0, background^.w, background^.h,
	al_makecol (32, 16, 128));
  al_solid_mode;

{ create a double buffer bitmap }
  buffer := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);

{ scale the background image to be the same size as the screen }
  al_stretch_blit (background, buffer, 0, 0, background^.w, background^.h,
	0, 0, AL_SCREEN_W, AL_SCREEN_H);

  al_textout_ex (buffer, al_font, Format ('%dx%d, %dpp', [AL_SCREEN_W, AL_SCREEN_H, bpp]),
	0, 0, al_makecol (255, 255, 255), -1);

  al_destroy_bitmap(background);
  background := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);
  al_blit (buffer, background, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);

  WHILE NOT al_keypressed DO
  BEGIN
  { draw the alpha sprite }
    x := al_mouse_x - sprite^.w DIV 2;
    y := al_mouse_y - sprite^.h DIV 2;

    al_set_alpha_blender;
    al_draw_trans_sprite (buffer, sprite, x, y);

  { flip it across to the screen }
    al_blit (buffer, al_screen, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);

  { replace the background where we drew the sprite }
    al_blit (background, buffer, x, y, x, y, sprite^.w, sprite^.h);
  END;

  al_clear_keybuf;

  al_destroy_bitmap (background);
  al_destroy_bitmap (sprite);
  al_destroy_bitmap (buffer);
END.
