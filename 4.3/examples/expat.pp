PROGRAM expal;
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
 *	This program demonstrates the use of patterned drawing and sub-bitmaps.
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


  PROCEDURE DrawPattern (bitmap: AL_BITMAPptr; message: ANSISTRING; color: LONGINT);
  VAR
    pattern: AL_BITMAPptr;
  BEGIN
  { Create a pattern bitmap. }
    pattern := al_create_bitmap (al_text_length (al_font, message),
				 al_text_height (al_font));
    al_clear_to_color (pattern, al_bitmap_mask_color (pattern));
    al_textout_ex (pattern, al_font, message, 0, 0,
		   al_makecol (255,255,255), al_bitmap_mask_color (al_screen));

  { Cover the bitmap with the pattern. }
    al_drawing_mode (AL_DRAW_MODE_MASKED_PATTERN, pattern, 0, 0);
    al_rectfill (bitmap, 0, 0, bitmap^.w, bitmap^.h, color);
    al_solid_mode();

  { Destroy the pattern bitmap. }
    al_destroy_bitmap (pattern);
  END;



VAR
  bitmap: AL_BITMAPptr;
BEGIN { The program starts here. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 320, 200, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Shows an error message. }
      al_message (al_error);
    { Shutdowns Allegro. }
      al_exit;
      EXIT;
    END;
  al_set_palette (al_desktop_palette);
  al_clear_to_color (al_screen, al_makecol (255, 255, 255));

{ First cover the whole screen with a pattern. }
  DrawPattern (al_screen, '<screen>', al_makecol (0, 0, 0));

{ Draw the pattern onto a memory bitmap and then blit it to the screen. }
  bitmap := al_create_bitmap (128, 32);
  al_clear_to_color (bitmap, al_makecol (255, 255, 255));
  DrawPattern (bitmap, '<memory>', al_makecol (255, 0, 0));
  al_blit (bitmap, al_screen, 0, 0, 32, 32, 128, 32);
  al_destroy_bitmap (bitmap);

{ Or we could use a sub-bitmap.  These share video memory with their
  parent, so the drawing will be visible without us having to blit it
  across onto the screen. }
  bitmap := al_create_sub_bitmap (al_screen, 224, 64, 64, 128);
  al_rectfill (al_screen, 224, 64, 286, 192, al_makecol (255, 255, 255));
  DrawPattern (bitmap, '<subbmp>', al_makecol (0, 0, 255));
  al_destroy_bitmap (bitmap);

  al_readkey;

{ Shutdown Allegro. }
  al_exit;

{ End of the program. }
END.

