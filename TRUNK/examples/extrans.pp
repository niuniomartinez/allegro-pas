PROGRAM extrans;
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
 *	This program demonstrates how to use the lighting and translucency
 *	functions.  The first part of the example will show a dark screen
 *	iluminated by a spotlight you can move with your mouse.  After a
 *	keypress the example shows the full bitmap and the spotlight changes to
 *	be a reduced version of the background with 50% of translucency.
 *
 *	The translucency effect is easy to do in all color depths.  However,
 *	the lighting effect has to be performed in a different way depending on
 *	whether the screen is in 8bit mode or another color depth.  This is
 *	because additive drawing mode uses a different set of routines for
 *	truecolor modes.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of the Allegro Game Library, by Owen Embury.
 *
 *	See README file for license and copyright information.
 *)

  USES
    sysutils,
  { It needs some Allegro.pas units. }
    allegro,
    alblend, { True color transparency and lighting. }
    alvga;   { 8 bit paletted mode special effects. }



  VAR
  (* RGB -> color mapping table. Not needed, but speeds things up. *)
    rgb_table: AL_RGB_MAP;
  (* Lighting color mapping table. *)
    light_table: AL_COLOR_MAP;
  (* Translucency color mapping table. *)
    trans_table: AL_COLOR_MAP;

    pal: AL_PALETTE;
    s, spotlight, truecolor_spotlight,
    background: AL_BITMAPptr;
    i, x, y: INTEGER;
    filename: STRING;

BEGIN { The program starts here. }

  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_timer;
  al_install_mouse;

{ We can set the color depth by command line. }
  IF ParamCount > 0 THEN
    al_set_color_depth (StrToInt (ParamStr (1)));
{ Otherwhise it will use 8bpp. }

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 320, 200, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;

{ Load the main screen image. }
  filename := ExtractFilePath (ParamStr (0)) + 'allegro.pcx';

  background := al_load_bitmap (filename, @pal);
  IF background = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
  { Show an error message. }
    al_message ('Error reading allegro.pcx');
    EXIT;
  END;

{ This isn't needed, but it speeds up the color table calculations. }
  al_create_rgb_table (rgb_table, pal, NIL);
  al_rgb_table := @rgb_table;

{ Build a color lookup table for lighting effects. }
  al_create_light_table (@light_table, pal, 0, 0, 0, NIL);

{ Build a color lookup table for translucent drawing. }
  al_create_trans_table (@trans_table, pal, 128, 128, 128, NIL);

  al_set_palette (pal);

  s := al_create_bitmap (320, 200);
  spotlight := al_create_bitmap_ex (8, 128, 128);
  truecolor_spotlight := al_create_bitmap_ex (32, 128, 128);

{ Generate an 8bpp spotlight image. }
  al_clear_bitmap (spotlight);
  FOR i := 0 TO 255 DO
    al_circlefill (spotlight, 64, 64, 64 - i DIV 4, i);

{ Select the lighting table. }
  al_color_table := @light_table;

{ Display a spotlight effect. }
  REPEAT
    al_poll_mouse;

    x := al_mouse_x - AL_SCREEN_W DIV 2 - 64 + 160;
    y := al_mouse_y - AL_SCREEN_H DIV 2 - 64 + 100;

    al_clear_bitmap (s);

  { Unluckily we have to do something 'weird' for truecolor modes. }
    IF al_bitmap_color_depth (al_screen) <> 8 THEN
    BEGIN
    { Copy background on the truecolor spotlight. }
      al_blit (background, truecolor_spotlight, x, y , 0, 0, 127, 127);
    { Set special write alpha blender. }
      al_set_write_alpha_blender;
      al_drawing_mode (AL_DRAW_MODE_TRANS, NIL, 0, 0);
    { Draw the alpha channel. }
      al_rectfill (truecolor_spotlight, 0, 0, 128, 128, 0);
      al_draw_trans_sprite (truecolor_spotlight, spotlight, 0, 0);
    { Set alpha blender and draw on the 's' bitmap. }
      al_drawing_mode (AL_DRAW_MODE_SOLID, NIL, 0, 0);
      al_set_alpha_blender;
      al_draw_trans_sprite (s, truecolor_spotlight, x, y);
    { Restore a not-alpha blender. }
      al_set_trans_blender (0, 0, 0, 128);
    END
    ELSE BEGIN
      al_blit (background, s, x, y, x, y, 128, 128);
      al_draw_trans_sprite (s, spotlight, x, y);
    END;

    al_blit (s, al_screen, 0, 0,
	     AL_SCREEN_W DIV 2 - 160, AL_SCREEN_H DIV 2 - 100, 320, 200);

  { Reduce CPU usage. }
    al_rest (20);

  UNTIL al_keypressed;

  al_clear_keybuf;

{ For the next part we want spotlight and s to share color depth. }
  IF al_bitmap_color_depth (spotlight) <> al_bitmap_color_depth (s) THEN
  BEGIN
    al_destroy_bitmap (spotlight);
    spotlight := al_create_bitmap (128, 128);
  END;

{ Generate an overlay image (just shrink the main image). }
  al_stretch_blit (background, spotlight, 0, 0, 320, 200, 0, 0, 128, 128);

{ Select the translucency table. }
  al_color_table := @trans_table;

{ Select translucency blender. }
  al_set_trans_blender (0, 0, 0, 128);

{ Display a translucent overlay. }
  REPEAT
    al_poll_mouse;

    x := al_mouse_x - AL_SCREEN_W DIV 2 - 64 + 160;
    y := al_mouse_y - AL_SCREEN_H DIV 2 - 64 + 100;

    al_blit (background, s, 0, 0, 0, 0, 320, 200);
    al_draw_trans_sprite (s, spotlight, x, y);

    al_blit (s, al_screen, 0, 0,
		AL_SCREEN_W DIV 2 - 160, AL_SCREEN_H DIV 2 - 100, 320, 200);

  { Reduce CPU usage. }
    al_rest (20);

  UNTIL al_keypressed;

  al_clear_keybuf;

{ Shutdown Allegro. }
  al_destroy_bitmap (s);
  al_destroy_bitmap (spotlight);
  al_destroy_bitmap (truecolor_spotlight);
  al_destroy_bitmap (background);

{ End of the program. }
END.
