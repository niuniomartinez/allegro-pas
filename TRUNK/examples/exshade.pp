PROGRAM exshade;
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
 *    This program demonstrates how to draw Gouraud shaded (lit)
 *    sprites.  In an apparently black screen, a planet like sprite
 *    is drawn close to the middle of the screen. In a similar
 *    way to how the first test of extrans works, you move the
 *    cursor on the screen with the mouse. Attached to this mouse
 *    you can imagine a virtual spotlight illuminating the scene
 *    around. Depending on where the mouse is, the Gouraud shaded
 *    sprite will show the direction of the light.
 *
 *    by Ñuño Martínez <>
 *    from an example program for the Allegro library, by Patrick Hogan
 *)

USES
  sysutils,
  allegro, alvga, alblend;



(* Considered a line between (x1, y1) and (x2, y2), the longer the line,
 * the smaller the return value will be. If the line length is zero, the
 * function returns the maximum value of 255.
 *)
  FUNCTION Distance (x1, y1, x2, y2: INTEGER): INTEGER;
  VAR
    dx, dy, temp: INTEGER;
  BEGIN
    dx := x2 - x1;
    dy := y2 - y1;
    temp := Trunc (sqrt ((dx * dx) + (dy * dy)) * 2);
    IF temp > 255 THEN
      temp := 255;
    Distance := 255 - temp;
  END;


VAR
  Pal: AL_PALETTE;
  Buffer, Planet: AL_BITMAPptr;
  Buf: STRING[255];
  ScreenW, ScreenH, SpriteW, SpriteH: INTEGER;
(* RGB -> color mapping table. Not needed, but speeds things up *)
  rgb_table: AL_RGB_MAP;
  light_table : AL_COLOR_MAP;
(* Program starts here. *)
BEGIN
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_mouse;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 320, 240, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 240, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message (al_error);
      EXIT;
    END;

{ Loads planet bitmap. }
  Buf := IncludeTrailingPathDelimiter (ExtractFileDir (ParamStr (0))) + 'planet.pcx';
  Planet := al_load_bitmap (buf, @pal);
  IF Planet = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Error reading ' + Buf);
    EXIT;
  END;

  Buffer := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);
  al_clear_bitmap (Buffer);
  al_set_palette (pal);

  al_create_rgb_table (rgb_table, pal, NIL);
  al_rgb_table := @rgb_table;

  al_create_light_table (@light_table, pal, 0, 0, 0, NIL);
  al_color_table := @light_table;

{ Pre-calculate values used to calculate distances. }
  ScreenW := AL_SCREEN_W DIV 2;
  ScreenH := AL_SCREEN_H DIV 2;
  SpriteW := ScreenW + Planet^.w;
  SpriteH := ScreenH + Planet^.h;

  al_set_trans_blender (0, 0, 0, 128);
  REPEAT
    al_poll_mouse;

    al_draw_gouraud_sprite (Buffer, Planet,
	ScreenW, ScreenH,
	Distance (ScreenW, ScreenH, al_mouse_x, al_mouse_y),
	Distance (SpriteW, ScreenH, al_mouse_x, al_mouse_y),
	Distance (SpriteW, SpriteH, al_mouse_x, al_mouse_y),
	Distance (ScreenW, SpriteH, al_mouse_x, al_mouse_y));

    al_textout_ex (Buffer, al_font, 'Gouraud Shaded Sprite Demo', 0, 0,
		   al_palette_color^[10], -1);

    al_circle (Buffer, al_mouse_x, al_mouse_y, 4, al_palette_color^[10]);
    al_blit (Buffer, al_screen, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);
    al_circle (Buffer, al_mouse_x, al_mouse_y, 4, al_palette_color^[31]);
  UNTIL al_keypressed;

  al_destroy_bitmap (Planet);
  al_destroy_bitmap (Buffer);
END.
