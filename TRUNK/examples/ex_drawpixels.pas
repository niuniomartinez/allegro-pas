PROGRAM ex_drawpixels;

{$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}

  USES
    Allegro5, Common,
    sysutils;

  CONST
    WIDTH = 640;
    HEIGHT = 480;
    NUM_STARS = 300;
    TARGET_FPS = 9999;

  TYPE
    TPoint = RECORD
      X, Y: SINGLE;
    END;

  VAR
    Display: ALLEGRO_DISPLAYptr;
  { Read line 120.
    KeyState: ALLEGRO_KEYBOARD_STATE;
  }
    Stars: ARRAY [1..3] OF ARRAY [1..(NUM_STARS DIV 3)] OF TPoint;
    Speeds: ARRAY [1..3] OF SINGLE = (0.0001, 0.05, 0.15);
    Colors: ARRAY [1..3] OF ALLEGRO_COLOR;
    Start, Now, Elapsed, FrameCount: LONGINT;
    TotalFrames: INTEGER;
    ProgramStart, Length: DOUBLE;
    Layer, Star, X, Y: INTEGER;

    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    EndExample: BOOLEAN;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  OpenLog;

  al_install_keyboard;

  Display := al_create_display (WIDTH, HEIGHT);
  IF Display = NIL THEN AbortExample ('Could not create display.');

  EventQueue := al_create_event_queue;
  al_register_event_source (EventQueue, al_get_keyboard_event_source);
  al_register_event_source (EventQueue, al_get_display_event_source (Display));

  Colors[1] := al_map_rgba (255, 100, 255, 128);
  Colors[2] := al_map_rgba (255, 100, 100, 255);
  Colors[3] := al_map_rgba (100, 100, 255, 255);

  FOR Layer := LOW (Stars) TO HIGH (Stars) DO
  BEGIN
    FOR Star := LOW (Stars[Layer]) TO HIGH (Stars[Layer]) DO
    BEGIN
      Stars[Layer][Star].X := Random (WIDTH);
      Stars[Layer][Star].Y := Random (HEIGHT)
    END
  END;


  Start := TRUNC (al_get_time * 1000);
  Now := Start;
  Elapsed := 0;
  FrameCount := 0;
  ProgramStart := al_get_time;

  TotalFrames := 0;
  EndExample := FALSE;
  REPEAT
    IF FrameCount < (1000 / TARGET_FPS) THEN
      FrameCount := FrameCount + Elapsed
    ELSE BEGIN
      DEC (FrameCount, TRUNC (1000 / TARGET_FPS));
      al_clear_to_color (al_map_rgb (0, 0, 0));
      FOR Star := LOW (Stars[1]) TO HIGH (Stars[1]) DO
        al_draw_pixel (Stars[1][Star].X, Stars[1][Star].Y, Colors[1]);
      al_lock_bitmap (al_get_backbuffer (Display), ALLEGRO_PIXEL_FORMAT_ANY, 0);

      FOR Layer := 2 TO HIGH (Stars) DO
      BEGIN
        FOR Star := LOW (Stars[Layer]) TO HIGH (Stars[Layer]) DO
          al_draw_pixel (
            Stars[Layer][Star].X, Stars[Layer][Star].Y, Colors[Layer]
          )
      END;

    { Check that dots appear at the window extremes. }
      X := WIDTH - 1;
      Y := HEIGHT - 1;
      al_put_pixel (0, 0, al_map_rgb_f (1, 1, 1));
      al_put_pixel (X, 0, al_map_rgb_f (1, 1, 1));
      al_put_pixel (0, Y, al_map_rgb_f (1, 1, 1));
      al_put_pixel (X, Y, al_map_rgb_f (1, 1, 1));

      al_unlock_bitmap (al_get_backbuffer (Display));
      al_flip_display;
      INC (TotalFrames)
    END;

    Now := TRUNC (al_get_time * 1000);
    Elapsed := Now - Start;
    Start := Now;

    FOR Layer := LOW (Stars) TO HIGH (Stars) DO
    BEGIN
      FOR Star := LOW (Stars[Layer]) TO HIGH (Stars[Layer]) DO
      BEGIN
        Stars[Layer][Star].Y := Stars[Layer][Star].Y - Speeds[Layer] * Elapsed;
        IF Stars[Layer][Star].Y < 0 THEN
        BEGIN
          Stars[Layer][Star].X := Random (WIDTH);
          Stars[Layer][Star].Y := HEIGHT
        END
      END
    END;

    al_rest (0.001);

  { NOTE: Seems to be a problem with "get_keyboard_state" and this doesn't work
	  in all systems (i.e. my Xubuntu 14.04.4 64bit), so I (Ñuño) have
	  changed this by events.
    al_get_keyboard_state (KeyState);
  UNTIL al_key_down (KeyState, ALLEGRO_KEY_ESCAPE);
  }
    IF al_get_next_event (EventQueue, Event) THEN
      CASE Event._type OF
      ALLEGRO_EVENT_DISPLAY_CLOSE:
	EndExample := TRUE;
      ALLEGRO_EVENT_KEY_DOWN:
	IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
	  EndExample := TRUE;
      END
  UNTIL EndExample;

  Length := al_get_time - ProgramStart;

  IF Length <> 0 THEN
    LogWriteLn (Format ('%d FPS', [TRUNC (TotalFrames / Length)]));

  al_destroy_display (Display);

  CloseLog (TRUE)
END.
