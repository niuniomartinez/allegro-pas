PROGRAM exSwitch;
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
 *    Example program for the Allegro library, by Shawn Hargreaves.
 *
 *    This program shows how to control the console switching mode, and
 *    let your program run in the background. These functions don't apply
 *    to every platform and driver, for example you can't control the
 *    switching mode from a DOS program.
 *
 *    Yes, I know the fractal drawing is very slow: that's the point!
 *    This is so you can easily check whether it goes on working in the
 *    background after you switch away from the app.
 *
 *    Depending on the type of selected switching mode, you will see
 *    whether the contents of the screen are preserved or not.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See README file for license and copyright information.
 *)

  USES
    Allegro, alGUI,
    sysutils;

  VAR
  (* there is no particular reason to use sub-bitmaps here: I just do it as a
   * stress-test, to make sure the switching code will handle them correctly.
   *)
    TextArea, GraphicsArea: AL_BITMAPptr;

    InCallback, OutCallback: INTEGER;



  (* timer callbacks should go on running when we are in background mode *)
    Counter: INTEGER;

  PROCEDURE IncrementCounter; CDECL;
  BEGIN
    INC (Counter)
  END;



(* Displays a text message in the scrolling part of the screen **)
  PROCEDURE ShowMsg (msg: STRING);
  BEGIN
    al_acquire_bitmap (TextArea);

    al_blit (TextArea, TextArea, 0, 8, 0, 0, TextArea^.w, TextArea^.h-8);
    al_rectfill (TextArea, 0, TextArea^.h-8, TextArea^.w, TextArea^.h,
		al_palette_color^[0]);

    IF msg <> '' THEN
      al_textout_centre_ex (TextArea, al_font, msg,
		TextArea^.w DIV 2, TextArea^.h - 8,
		al_palette_color^[255], al_palette_color^[0]);

    al_release_bitmap (TextArea);
  END;



(* Displays the current switch mode setting *)
  PROCEDURE ShowSwitchMode;
  BEGIN
    CASE al_get_display_switch_mode OF
    AL_SWITCH_NONE:
      ShowMsg ('Current mode is AL_SWITCH_NONE');
    AL_SWITCH_PAUSE:
      ShowMsg ('Current mode is AL_SWITCH_PAUSE');
    AL_SWITCH_AMNESIA:
      ShowMsg ('Current mode is AL_SWITCH_AMNESIA');
    AL_SWITCH_BACKGROUND:
      ShowMsg ('Current mode is AL_SWITCH_BACKGROUND');
    AL_SWITCH_BACKAMNESIA:
      ShowMsg ('Current mode is AL_SWITCH_BACKAMNESIA');
    ELSE
      ShowMsg ('Eeek! Unknown switch mode...');
    END
  END;



(* Callback for switching back to our program *)
  PROCEDURE SwitchInCallback; CDECL;
  BEGIN
    INC (InCallback)
  END;



(* Callback for switching away from our program *)
  PROCEDURE SwitchOutCallback; CDECL;
  BEGIN
    INC (OutCallback)
  END;



(* Changes the display switch mode *)
  PROCEDURE SetSwMode (Mode: INTEGER);
  BEGIN
    IF NOT al_set_display_switch_mode (mode) THEN
    BEGIN
      ShowMsg ('Error changing switch mode');
      ShowMsg ('');
      EXIT
    END;
    ShowSwitchMode;
    IF al_set_display_switch_callback (AL_SWITCH_IN, @SwitchInCallback) THEN
      ShowMsg ('AL_SWITCH_IN callback activated')
    ELSE
      ShowMsg ('AL_SWITCH_IN callback not available');
    IF al_set_display_switch_callback (AL_SWITCH_OUT, @SwitchOutCallback) THEN
      ShowMsg ('AL_SWITCH_OUT callback activated')
    ELSE
      ShowMsg ('AL_SWITCH_OUT callback not available');
    ShowMsg ('')
  END;



  VAR
    X, Y: INTEGER;

(* Draws some graphics, for no particular reason at all *)
  PROCEDURE DrawPointlessGraphics;
  VAR
   zr, zi, cr, ci, tr, ti: REAL;
   c: INTEGER;
  BEGIN
    IF (x = 0) AND (y = 0) THEN
      al_clear_to_color (GraphicsArea, al_palette_color^[255]);

    cr := (x / GraphicsArea^.w - 0.75) * 2.0;
    ci := (y / GraphicsArea^.h - 0.5) * 1.8;

    zr := 0;
    zi := 0;

    FOR c := 0 TO 99 DO
    BEGIN
      tr := zr * zr - zi * zi;
      ti := zr * zi * 2;

      zr := tr + cr;
      zi := ti + ci;
      IF (zr < -10) OR (zr > 10) OR (zi < -10) OR (zi > 10) THEN
	BREAK;
    END;

    IF (zi <> zi) OR (zr <> zr) THEN
      c := 0
    ELSE IF (zi <= -1) OR (zi >= 1) OR (zr <= -1) OR (zr >= 1) THEN
      c := 255
    ELSE BEGIN
      c := TRUNC (sqrt (zi * zi + zr * zr) * 256);
      IF c > 255 THEN c := 255;
    END;

    al_putpixel (GraphicsArea, x, y, al_makecol (c, c, c));

    INC (x);
    IF x >= GraphicsArea^.w THEN
    BEGIN
      x := 0;
      INC (y);
      IF y >= GraphicsArea^.h THEN y := 0
    END
  END;



VAR
  Pal: AL_PALETTE;
  Finished: BOOLEAN;
  LastCounter, c, w, h, bpp, i: LONGINT;
BEGIN
  Finished := FALSE;
  LastCounter := 0;

  InCallback := 0; OutCallback := 0;
  Counter := 0;

  IF NOT al_init THEN EXIT;
  al_install_keyboard;
  al_install_mouse;
  al_install_timer;

  IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Unable to set any graphic mode'#10'' + al_error);
    EXIT
  END;
  al_set_palette (al_desktop_palette);

  c := AL_GFX_AUTODETECT;
  w := AL_SCREEN_W;
  h := AL_SCREEN_H;
  bpp := al_bitmap_color_depth (al_screen);
  IF NOT al_gfx_mode_select_ex (c, w, h, bpp) THEN
  BEGIN
    al_exit;
    EXIT
  END;

  al_set_color_depth (bpp);
  IF NOT al_set_gfx_mode (c, w, h, 0, 0) THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Error setting graphics mode'#10'' + al_error);
    EXIT
  END;

  FOR i := 0 TO 255 DO
  BEGIN
    pal[i].r := i DIV 4;
    pal[i].g := i DIV 4;
    pal[i].b := i DIV 4
  END;

  al_set_palette (pal);

  TextArea := al_create_sub_bitmap (al_screen, 0, 0, AL_SCREEN_W, AL_SCREEN_H DIV 2);
  GraphicsArea := al_create_sub_bitmap (al_screen, 0, AL_SCREEN_H DIV 2,
					AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2);
  IF (TextArea = NIL) OR (GraphicsArea = NIL) THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Out of memory!');
    EXIT
  END;

  al_install_int (@IncrementCounter, 10);

  ShowMsg ('Console switching test');
  ShowMsg ('Press 1-5 to change mode');
  ShowMsg ('');

  ShowSwitchMode;
  ShowMsg ('');

  REPEAT
    IF Counter <> LastCounter THEN
    BEGIN
      LastCounter := Counter;

      al_acquire_screen;
      al_textout_ex (al_screen, al_font,
		     Format ('Time: %d', [LastCounter]),
		     (AL_SCREEN_W * 3) DIV 4, (AL_SCREEN_H * 3) DIV 4,
		     al_palette_color^[255], al_palette_color^[0]
		    );
      al_release_screen;

      al_acquire_bitmap (GraphicsArea);
      FOR i := 1 TO 10 DO DrawPointlessGraphics;
      al_release_bitmap (GraphicsArea);
    END;

    IF al_keypressed THEN
      CASE al_readkey AND 255 OF
      ORD ('1'):
	SetSwMode (AL_SWITCH_NONE);
      ORD ('2'):
	SetSwMode (AL_SWITCH_PAUSE);
      ORD ('3'):
	SetSwMode (AL_SWITCH_AMNESIA);
      ORD ('4'):
	SetSwMode (AL_SWITCH_BACKGROUND);
      ORD ('5'):
	SetSwMode (AL_SWITCH_BACKAMNESIA);
      27: { Esc }
	Finished := TRUE;
      END;

      WHILE InCallback > 0 DO
      BEGIN
	DEC (InCallback);
	ShowMsg ('AL_SWITCH_IN callback');
	ShowMsg ('')
      END;

      WHILE OutCallback > 0 DO
      BEGIN
	DEC (OutCallback);
	ShowMsg ('AL_SWITCH_OUT callback');
	ShowMsg ('')
      END;
  UNTIL Finished;

  al_destroy_bitmap (TextArea);
  al_destroy_bitmap (GraphicsArea);
END.
