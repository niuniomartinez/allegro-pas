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
 *	This example demonstrates how to use datafiles, various sprite drawing
 *	routines and flicker-free animation.
 *
 *	by Ñuño Martínez <>
 *	from an example of Allegro Game Library by Grzegorz Ludorowski.
 *
 *	See readme.txt for license and copyright information.
 *
 *	Why is the animate() routine coded in that way?  As you probably know,
 *	VIDEO RAM is much slower than "normal" RAM, so it's advisable to reduce
 *	VRAM blits to a minimum.  Drawing sprite on the screen (meaning in
 *	VRAM) and then clearing a background for it is not very fast.  This
 *	example uses a different method which is much faster, but require a bit
 *	more memory.
 *
 *	First the buffer is cleared (it's a normal BITMAP), then the sprite is
 *      drawn on it, and when the drawing is finished this buffer is copied
 *	directly to the screen. So the end result is that there is a single
 *	VRAM blit instead of blitting/clearing the background and drawing a
 *	sprite on it.  It's a good method even when you have to restore the
 *	background.  And of course, it completely removes any flickering
 *	effect.
 *
 *	When one uses a big (ie. 800x600 background) and draws something on it,
 *	it's wise to use a copy of background somewhere in memory and restore
 *	background using this "virtual background".  When blitting from VRAM in
 *	SVGA modes, it's probably, that drawing routines have to switch banks
 *	on video card.  I think, I don't have to remind how slow is it.
 *
 *	Note that on modern systems, the above isn't true anymore, and you
 *	usually get the best performance by caching all your animations in
 *	video ram and doing only VRAM->VRAM blits, so there is no more
 *	RAM->VRAM transfer at all anymore.  And usually, such transfers can run
 *	in parallel on the graphics card's processor as well, costing virtually
 *	no main cpu time at all.  See the exaccel example for an example of
 *	this.
 *)

{$IFDEF FPC}
{ Free Pascal. }
  {$LONGSTRINGS ON}
{$ENDIF}

USES
  sysutils,
{ It needs some Allegro.pas units. }
  allegro, alfile, alfixed,
  albltspr, { Image blitting and sprite drawing. }
  aldigi,   { Digital sound. }
  aldraw,   { Drawing primitives. }
  algraph,  { Graphic mode configuration. }
  alsound,  { Sound configuration. }
  altext;   { Text drawing. }



{ The grabber produces a header, which contains defines for the names of all
  the objects in the datafile (FRAME_01, PALETTE_001, etc).  You can convert
  that header into Pascal code using the h2pas utility. }
{$I running.inc}



CONST
  FRAMES_PER_SECOND = 30;



VAR
  running_data: AL_DATAFILEptr; { Pointer to data file. }
  frame, frame_number: INTEGER; { Current sprite frame number. }
  sprite_buffer: AL_BITMAPptr;	{ Pointer to a sprite buffer, where sprite
				  will be drawn. }
  next: BOOLEAN;		{ If true, skip to next part. }
  ticks: INTEGER;		{ Timer for animation. }


(* set up a timer for animation *)
  PROCEDURE ticker; CDECL;
  BEGIN
    INC (ticks);
  END;



  PROCEDURE animate;
  BEGIN
  { Wait for animation timer. }
    WHILE frame > ticks DO
    { Avoid busy wait. }
      al_rest (1);

  { Ideally, instead of using a timer, we would set the monitor refresh rate
    to a multiple of the animation speed, and synchronize with the vertical
    blank interrupt (al_vsync) - to get a completely smooth animation.  But
    this doesn't work on all setups (e.g. in X11 windowed modes), so should
    only be used after performing some tests first or letting the user enable
    it.  Too much for this simple example }

    INC (frame);

  { Blits sprite buffer to screen. }
    al_blit (sprite_buffer, al_screen, 0, 0,
	     (AL_SCREEN_W - sprite_buffer^.w) DIV 2,
	     (AL_SCREEN_H - sprite_buffer^.h) DIV 2,
	     sprite_buffer^.w, sprite_buffer^.h
	    );

  { clears sprite buffer with color 0 }
    al_clear_bitmap (sprite_buffer);

  { If key pressed set a next flag. }
    next := al_keypressed;

    IF frame_number = 0 THEN
      al_play_sample (running_data^[SOUND_01].dat, 128, 128, 1000, 0);

  { Increase frame number, or if it's equal 9 (last frame) set it to 0. }
    IF frame_number = 9 THEN
      frame_number := 0
    ELSE
      INC (frame_number);
  END;



VAR
  datafile_name: ANSISTRING;
  palette: AL_PALETTEptr;
  angle: INTEGER;
  x, y: INTEGER;
  text_y: INTEGER;
  color: INTEGER;

BEGIN { The program starts here. }
  ticks := 0;
  frame := 0;
  frame_number := 0;
  next := FALSE;
  angle := 0;

  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_sound (AL_DIGI_AUTODETECT, AL_MIDI_NONE);
  al_install_timer;
  al_install_int_ex (@ticker, AL_BPS_TO_TIMER (30));

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Shows an error message. }
      al_message (al_error);
    { Shutdowns Allegro. }
      al_exit;
      EXIT;
    END;

{ Loads datafile and sets user palette saved in datafile. }
  datafile_name := ExtractFilePath (ParamStr (0)) + 'running.dat';
  running_data := al_load_datafile (datafile_name);
  IF running_data = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Error loading running.dat');
    al_exit;
    EXIT;
  END;

{ Select the palette which was loaded from the datafile. }
  palette := running_data^[PALETTE_001].dat;
  al_set_palette (palette^);

{ Create and clear a bitmap for sprite buffering, big
  enough to hold the diagonal(sqrt(2)) when rotating. }
  sprite_buffer := al_create_bitmap (TRUNC (82 * SQRT(2) + 2),
				     TRUNC (82 * SQRT(2) + 2));
  al_clear_bitmap (sprite_buffer);

  x := (sprite_buffer^.w - 82) DIV 2;
  y := (sprite_buffer^.h - 82) DIV 2;
  color := al_makecol (0, 80, 0);
  text_y := AL_SCREEN_H - 10 - al_text_height (al_font);

  frame := ticks;

{ Write current sprite drawing method. }
  al_textout_centre_ex (al_screen, al_font, 'Press a key for next part...',
			AL_SCREEN_W DIV 2, 10, 1, -1);
  al_textout_centre_ex (al_screen, al_font, 'Using al_draw_sprite',
			AL_SCREEN_W DIV 2, text_y, 15, -1);

  REPEAT
    al_hline (sprite_buffer, 0, y + 82, sprite_buffer^.w - 1, color);
    al_draw_sprite (sprite_buffer, running_data^[frame_number].dat, x, y);
    animate();
  UNTIL next;

  al_clear_keybuf;
  al_rectfill (al_screen, 0, text_y, AL_SCREEN_W, AL_SCREEN_H, 0);
  al_textout_centre_ex (al_screen, al_font, 'Using al_draw_sprite_h_flip',
			AL_SCREEN_W DIV 2, text_y, 15, -1);

  REPEAT
    al_hline (sprite_buffer, 0, y + 82, sprite_buffer^.w - 1, color);
    al_draw_sprite_h_flip (sprite_buffer, running_data^[frame_number].dat, x, y);
    animate();
  UNTIL next;

  al_clear_keybuf;
  al_rectfill (al_screen, 0, text_y, AL_SCREEN_W, AL_SCREEN_H, 0);
  al_textout_centre_ex (al_screen, al_font, 'Using al_draw_sprite_v_flip',
			AL_SCREEN_W DIV 2, text_y, 15, -1);

  REPEAT
    al_hline (sprite_buffer, 0, y + 82, sprite_buffer^.w - 1, color);
    al_draw_sprite_v_flip (sprite_buffer, running_data^[frame_number].dat, x, y);
    animate();
  UNTIL next;

  al_clear_keybuf;
  al_rectfill (al_screen, 0, text_y, AL_SCREEN_W, AL_SCREEN_H, 0);
  al_textout_centre_ex (al_screen, al_font, 'Using al_draw_sprite_vh_flip',
			AL_SCREEN_W DIV 2, text_y, 15, -1);

  REPEAT
    al_hline (sprite_buffer, 0, y + 82, sprite_buffer^.w - 1, color);
    al_draw_sprite_vh_flip (sprite_buffer, running_data^[frame_number].dat, x, y);
    animate();
  UNTIL next;

  al_clear_keybuf;
  al_rectfill (al_screen, 0, text_y, AL_SCREEN_W, AL_SCREEN_H, 0);
  al_textout_centre_ex (al_screen, al_font, 'Now with rotating - pivot sprite',
			AL_SCREEN_W DIV 2, text_y, 15, -1);

  REPEAT
{ The last argument to al_pivot_sprite() is a fixed point type,
  so I had to use al_itofix routine (integer to fixed). }
    al_circle (sprite_buffer, x + 41, y + 41, 47, color);
    al_pivot_sprite (sprite_buffer, running_data^[frame_number].dat,
		     sprite_buffer^.w DIV 2, sprite_buffer^.h DIV 2,
		     41, 41, al_itofix (angle));
    animate();
    angle := angle - 4;
  UNTIL next;

  al_clear_keybuf;
  al_rectfill (al_screen, 0, text_y, AL_SCREEN_W, AL_SCREEN_H, 0);
  al_textout_centre_ex (al_screen, al_font, 'Now using pivot_sprite_v_flip',
			AL_SCREEN_W DIV 2, text_y, 15, -1);

  REPEAT
{ The last argument to al_pivot_sprite() is a fixed point type,
  so I had to use al_itofix() routine (integer to fixed). }
    al_circle (sprite_buffer, x + 41, y + 41, 47, color);
    al_pivot_sprite_v_flip (sprite_buffer, running_data^[frame_number].dat,
			    sprite_buffer^.w DIV 2, sprite_buffer^.h DIV 2,
			    41, 41, al_itofix (angle));
    animate();
    angle := angle - 4;
  UNTIL next;

{ Shutdown Allegro. }
  al_unload_datafile (running_data);
  al_destroy_bitmap (sprite_buffer);
  al_exit;

{ End of the program. }
END.
