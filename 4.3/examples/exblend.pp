PROGRAM exblend;
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
 *	This program demonstrates how to use the translucency functions in
 *	truecolor video modes.  Two image files are loaded from disk and
 *	displayed moving slowly around the screen.  One of the images will be
 *	tinted to different colors.  The other image will be faded out with a
 *	varying alpha strength, and drawn on top of the other image.
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
  albitmap, { Bitmap manipulation. }
  albltspr, { Image blitting and sprite drawing. }
  alcolor,  { Color manipulation. }
  aldraw,   { Drawing primitives. }
  alfile,   { File functions. }
  alfixed,  { Fixed point. }
  algraph,  { Graphic mode configuration. }
  alkeybrd, { Keyboard input. }
  alpalete, { Color palette manipulation. }
  altext,   { Text drawing. }
  altimer,  { Time control.  Recommendable if using mouse. }
  altrutra; { True color transparency and lighting. }



  FUNCTION MIN (a, b: LONGINT): LONGINT;
  BEGIN
    IF a < b THEN
      MIN := a
    ELSE
      MIN := b;
  END;



  FUNCTION MAX (a, b: LONGINT): LONGINT;
  BEGIN
    IF a > b THEN
      MAX := a
    ELSE
      MAX := b;
  END;



VAR
  buf, filename: STRING;
  pal: AL_PALETTE;
  image1, image2: AL_BITMAPptr;
  buffer: AL_BITMAPptr;
  r, g, b, a: LONGINT;
  x, y, w, h: LONGINT;
  x1, y1, x2, y2: LONGINT;
  prevx1, prevy1, prevx2, prevy2: LONGINT;
  timer: LONGINT;
  bpp: INTEGER;
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

{ What color depth should we use? }
  IF Paramcount > 0 THEN
  BEGIN
    Buf := ParamStr (1);
    IF (Buf[1] = '-') OR (Buf[1] = '/') THEN
    BEGIN
      DELETE (Buf, 1, 1);
      bpp := StrToInt (Buf);
      IF NOT bpp IN [15, 16, 24, 32] THEN
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
    al_message (al_error);
  { Shutdown Allegro. }
    al_exit;
    EXIT;
  END;

{ Specifiy that images should be loaded in a truecolor pixel format. }
  al_set_color_conversion (AL_COLORCONV_TOTAL);

{ Get the path of the executable. }
  buf := ExtractFilePath (ParamStr (0));

{ Load the first picture. }
  filename := buf + 'allegro.pcx';
  image1 := al_load_bitmap (filename, @pal);
  IF image1 = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Error reading allegro.pcx!');
  { Shutdown Allegro. }
    al_exit;
    EXIT;
  END;

{ Load the second picture. }
  filename := buf + 'mysha.pcx';
  image2 := al_load_bitmap (filename, @pal);
  IF image2 = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Error reading mysha.pcx!');
  { Shutdown Allegro. }
    al_exit;
    EXIT;
  END;

{ Create a double buffer bitmap. }
  buffer := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);

{ Note that because we loaded the images as truecolor bitmaps, we don't
  need to bother setting the palette, and we can display both on screen
  at the same time even though the source files use two different 256
  color palettes... }

  prevx1 := 0; prevy1 := 0; prevx2 := 0; prevy2 := 0;

  al_textout_ex (al_screen, al_font, INTTOSTR (bpp) + ' bpp', 0, AL_SCREEN_H - 8, al_makecol(255, 255, 255), al_makecol (0, 0, 0));

  WHILE NOT al_keypressed DO
  BEGIN
    timer := al_retrace_count;
    al_clear_bitmap (buffer);
  { The first image moves in a slow circle while being tinted to
    different colors... }
    x1 := 160 + al_fixtoi (al_fixsin (al_itofix (timer) DIV 16) * 160);
    y1 := 140 - al_fixtoi (al_fixcos (al_itofix (timer) DIV 16) * 140);
    r  := 127 - al_fixtoi (al_fixcos (al_itofix (timer) DIV  6) * 127);
    g  := 127 - al_fixtoi (al_fixcos (al_itofix (timer) DIV  7) * 127);
    b  := 127 - al_fixtoi (al_fixcos (al_itofix (timer) DIV  8) * 127);
    a  := 127 - al_fixtoi (al_fixcos (al_itofix (timer) DIV  9) * 127);
    al_set_trans_blender (r, g, b, 0);
    al_draw_lit_sprite (buffer, image1, x1, y1, a);
    al_textout_ex (al_screen, al_font, 'light: ' + INTTOSTR (a) + ' ', 0, 0, al_makecol (r, g, b), al_makecol (0, 0, 0));

  { The second image moves in a faster circle while the alpha value
    fades in and out... }
    x2 := 160 + al_fixtoi (al_fixsin (al_itofix(timer) DIV 10) * 160);
    y2 := 140 - al_fixtoi (al_fixcos (al_itofix(timer) DIV 10) * 140);
    a  := 127 - al_fixtoi (al_fixcos (al_itofix(timer) DIV  4) * 127);

    al_set_trans_blender (0, 0, 0, a);
    al_draw_trans_sprite (buffer, image2, x2, y2);
    al_textout_ex (al_screen, al_font, 'alpha: ' + INTTOSTR (a) + ' ', 0, 8, al_makecol(a, a, a), al_makecol(0, 0, 0));

  { Copy the double buffer across to the screen. }
    al_vsync;

    x := MIN (x1, prevx1);
    y := MIN (y1, prevy1);
    w := MAX (x1, prevx1) + 320 - x;
    h := MAX (y1, prevy1) + 200 - y;
    al_blit (buffer, al_screen, x, y, x, y, w, h);

    x := MIN (x2, prevx2);
    y := MIN (y2, prevy2);
    w := MAX (x2, prevx2) + 320 - x;
    h := MAX (y2, prevy2) + 200 - y;
    al_blit (buffer, al_screen, x, y, x, y, w, h);

    prevx1 := x1;
    prevy1 := y1;
    prevx2 := x2;
    prevy2 := y2;
  END;

  al_clear_keybuf;

{ Shutdown Allegro. }
  al_destroy_bitmap (image1);
  al_destroy_bitmap (image2);
  al_destroy_bitmap (buffer);
  al_exit;

{ End of the program. }
END.

