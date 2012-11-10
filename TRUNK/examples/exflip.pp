PROGRAM exflip;
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
 *
 *	This program moves a circle across the screen, first with a
 *	double buffer and then using page flips.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See README file for license and copyright information.
 *)

USES
  allegro;



VAR
   buffer, page1, page2, active_page: AL_BITMAPptr;
   c: INTEGER;
BEGIN { The program starts here. }
   IF NOT al_init THEN
   BEGIN
      WriteLn ('Can''t initialize Allegro!');
      EXIT;
   END;
   al_install_timer;
   al_install_keyboard;

{ Some platforms do page flipping by making one large screen that you
  can then scroll, while others give you several smaller, unique
  surfaces. If you use the al_create_video_bitmap() function, the same
  code can work on either kind of platform, but you have to be careful
  how you set the video mode in the first place. We want two pages of
  320x200 video memory, but if we just ask for that, on DOS Allegro
  might use a VGA driver that won't later be able to give us a second
  page of vram. But if we ask for the full 320x400 virtual screen that
  we want, the call will fail when using DirectX drivers that can't do
  this. So we try two different mode sets, first asking for the 320x400
  size, and if that doesn't work, for 320x200.
}
   IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 320, 200, 0, 400) THEN
      IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 320, 200, 0, 0) THEN
	 IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 400) THEN
	 BEGIN
	    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
	 { Shows an error message. }
	    al_message ('Unable to set any graphic mode'#10+al_error+''#10);
	   EXIT;
	 END;

   al_set_palette (al_desktop_palette);

{ Allocate the memory buffer }
   buffer := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);

{ First with a double buffer... }
   al_clear_keybuf;
   c := al_retrace_count + 32;
   WHILE al_retrace_count - c <= AL_SCREEN_W + 32 DO
   BEGIN
      al_clear_to_color (buffer, al_makecol (255, 255, 255));
      al_circlefill (buffer, al_retrace_count - c,
		     AL_SCREEN_H DIV 2, 32, al_makecol (0, 0, 0));
      al_textout_ex (buffer, al_font, 'Double buffered', 0, 0,
		     al_makecol(0, 0, 0), -1);
      al_blit (buffer, al_screen, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);

      IF al_keypressed THEN
	 BREAK;
   END;

   al_destroy_bitmap (buffer);

{ Now create two video memory bitmaps for the page flipping. }
   page1 := al_create_video_bitmap (AL_SCREEN_W, AL_SCREEN_H);
   page2 := al_create_video_bitmap (AL_SCREEN_W, AL_SCREEN_H);

   IF (page1 = NIL) OR (page2 = NIL) THEN
   BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
   { Shows an error message. }
      al_message ('Unable to create two video memory pages');
      EXIT;
   END;

   active_page := page2;

{ Do the animation using page flips... }
   al_clear_keybuf;
   FOR c := -32 TO  AL_SCREEN_W + 32 DO
   BEGIN
      al_clear_to_color (active_page, al_makecol(255, 255, 255));
      al_circlefill(active_page, c, AL_SCREEN_H DIV 2, 32, al_makecol(0, 0, 0));
      al_textout_ex (active_page, al_font, 'Page flipping', 0, 0,
		    al_makecol(0, 0, 0), -1);
      al_show_video_bitmap (active_page);

      IF active_page = page1 THEN
	 active_page := page2
      ELSE
	 active_page := page1;

      IF al_keypressed THEN
	 BREAK;
   END;

{ Shutdown Allegro. }
  al_destroy_bitmap (page1);
  al_destroy_bitmap (page2);

{ End of the program. }
END.
