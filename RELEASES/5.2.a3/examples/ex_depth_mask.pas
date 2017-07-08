PROGRAM ex_depth_mask;
{$IFDEF FPC}
{ Needed to support classes. }
  {$IF NOT DEFINED(FPC_DELPHI)}
    {$MODE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}

  USES
    Common,
    Allegro5, al5font, al5image, al5ttf,
    sysutils;

  CONST
    FPS = 60;
    COUNT = 80;

  TYPE
    TExample = CLASS (TObject)
    PRIVATE
      fDisplay: ALLEGRO_DISPLAYptr;
      fW, fH: INTEGER;
      fTimer: ALLEGRO_TIMERptr;
      fQueue: ALLEGRO_EVENT_QUEUEptr;
      fMysha, fObp: ALLEGRO_BITMAPptr;
      fFont, fFont2: ALLEGRO_FONTptr;
      fDirectSpeedMeasure: SINGLE;
      fSprites: ARRAY [1..COUNT] OF RECORD
	X, Y, Angle: DOUBLE;
      END;

      PROCEDURE Redraw;
      PROCEDURE Update;
    PUBLIC
      PROCEDURE Initialize;
      PROCEDURE Run;
    END;



  PROCEDURE TExample.Redraw;
  VAR
    T: ALLEGRO_TRANSFORM;
    i, x, y: INTEGER;
  BEGIN
  { We first draw the Obp background and clear the depth buffer to 1. }
    al_set_render_state (ALLEGRO_ALPHA_TEST, 1);
    al_set_render_state (ALLEGRO_ALPHA_FUNCTION, ALLEGRO_RENDER_GREATER);
    al_set_render_state (ALLEGRO_ALPHA_TEST_VALUE, 0);

    al_set_render_state (ALLEGRO_DEPTH_TEST, 0);

    al_set_render_state (ALLEGRO_WRITE_MASK, ALLEGRO_MASK_DEPTH OR ALLEGRO_MASK_RGBA);

    al_clear_depth_buffer (1);
    al_clear_to_color (al_map_rgb_f (0, 0, 0));

    al_draw_scaled_bitmap (fObp, 0, 0, 532, 416, 0, 0, fW, 416 * fW / 532, 0);

  { Next we draw all sprites but only to the depth buffer (with a depth value
    of 0). }
    al_set_render_state (ALLEGRO_DEPTH_TEST, 1);
    al_set_render_state (ALLEGRO_DEPTH_FUNCTION, ALLEGRO_RENDER_ALWAYS);
    al_set_render_state (ALLEGRO_WRITE_MASK, ALLEGRO_MASK_DEPTH);

    FOR i := LOW (fSprites) TO HIGH (fSprites) DO
    BEGIN
      al_hold_bitmap_drawing (TRUE);
      Y := -fH;
      REPEAT
	x := -fW;
	REPEAT
	  al_identity_transform (t);
	  al_rotate_transform (t, fSprites[i].Angle);
	  al_translate_transform (t, fSprites[i].x + x, fSprites[i].y + y);
	  al_use_transform (t);
	  al_draw_text (
	    fFont, al_map_rgb (0, 0, 0), 0, 0,
	    ALLEGRO_ALIGN_CENTER, 'Allegro 5'
	  );
	  INC (x, fW)
	UNTIL x > 0;
	INC (y, fH)
      UNTIL y > 0;
      al_hold_bitmap_drawing (FALSE);
    END;
    al_identity_transform (t);
    al_use_transform (t);

  { Finally we draw Mysha, with depth testing so she only appears where
    sprites have been drawn before. }
    al_set_render_state (ALLEGRO_DEPTH_FUNCTION, ALLEGRO_RENDER_EQUAL);
    al_set_render_state (ALLEGRO_WRITE_MASK, ALLEGRO_MASK_RGBA);
    al_draw_scaled_bitmap (fMysha, 0, 0, 320, 200, 0, 0, 320 * fH / 200, fH, 0);

  { Finally we draw an FPS counter. }
    al_set_render_state (ALLEGRO_DEPTH_TEST, 0);

    al_draw_text (
      fFont2, al_map_rgb_f (1, 1, 1), fW, 0, ALLEGRO_ALIGN_RIGHT,
      Format ('%.1f FPS', [1.0 / fDirectSpeedMeasure])
    )
  END;


  PROCEDURE TExample.Update;
  VAR
    i: INTEGER;
  BEGIN
    FOR i := LOW (fSprites) TO HIGH (fSprites) DO
    BEGIN
      fSprites[i].x := fSprites[i].x - 4;
      IF fSprites[i].x < 80 THEN
	fSprites[i].x := fSprites[i].x + fW;
      fSprites[i].angle := fSprites[i].angle + i * ALLEGRO_PI / 180 / COUNT
    END
  END;



  PROCEDURE TExample.Initialize;
  VAR
    i: INTEGER;
    Info: ALLEGRO_MONITOR_INFO;
  BEGIN
    IF NOT al_init THEN AbortExample ('Could not init Allegro.');
    IF NOT al_init_image_addon THEN AbortExample ('Failed to init IIO addon.');
    al_init_font_addon;
    IF NOT al_init_ttf_addon THEN AbortExample ('Failed to init TTF addon.');
    InitPlatformSpecific;

    al_get_num_video_adapters;

    al_get_monitor_info (0, Info);
{ TODO: #ifdef ALLEGRO_IPHONE
   al_set_new_display_flags(ALLEGRO_FULLSCREEN_WINDOW);
   #endif
}
    al_set_new_display_option (
      ALLEGRO_SUPPORTED_ORIENTATIONS, ALLEGRO_DISPLAY_ORIENTATION_ALL,
      ALLEGRO_SUGGEST);

    al_set_new_display_option (ALLEGRO_DEPTH_SIZE, 8,
      ALLEGRO_SUGGEST);

    al_set_new_bitmap_flags (ALLEGRO_MIN_LINEAR OR ALLEGRO_MAG_LINEAR);

    fW := 640; fH := 480;
    fDisplay := al_create_display (fW, fH);
    IF fDisplay = NIL THEN AbortExample ('Error creating display.');

    IF NOT al_install_keyboard THEN AbortExample ('Error installing keyboard.');

    fFont := al_load_font ('data/DejaVuSans.ttf', 40, 0);
    IF fFont = NIL THEN AbortExample ('Error loading data/DejaVuSans.ttf');
    fFont2 := al_load_font ('data/DejaVuSans.ttf', 12, 0);
    IF fFont2 = NIL THEN AbortExample ('Error loading data/DejaVuSans.ttf');

    fMysha := al_load_bitmap ('data/mysha.pcx');
    IF fMysha = NIL THEN AbortExample ('Error loading data/mysha.pcx');

    fObp := al_load_bitmap ('data/obp.jpg');
    IF fObp = NIL THEN AbortExample ('Error loading data/obp.jpg');

    FOR i := LOW (fSprites) TO HIGH (fSprites) DO
    BEGIN
      fSprites[i].x := (i MOD 4) * 160;
      fSprites[i].y := (i / 4) * 24;
    END;
    fTimer := al_create_timer (1.0 / FPS);

    fQueue := al_create_event_queue;
    al_register_event_source (fQueue, al_get_keyboard_event_source);
    al_register_event_source (fQueue, al_get_timer_event_source (fTimer));
    al_register_event_source (fQueue, al_get_display_event_source (fDisplay));

    fDirectSpeedMeasure := al_get_time
  END;



  PROCEDURE TExample.Run;
  VAR
    Done, NeedRedraw, Background: BOOLEAN;
    Event: ALLEGRO_EVENT;
    t: DOUBLE;
  BEGIN
    Done := FALSE;
    NeedRedraw := TRUE;
    Background := FALSE;
    al_start_timer (fTimer);

    WHILE NOT Done DO
    BEGIN
      IF NOT Background AND NeedRedraw
      AND al_is_event_queue_empty (fQueue) THEN
      BEGIN
	t := -al_get_time;

	SELF.Redraw;

	t := t + al_get_time;
	fDirectSpeedMeasure := t;
	al_flip_display;
	NeedRedraw := FALSE;
      END;

      al_wait_for_event (fQueue, Event);
      CASE Event._type OF
      ALLEGRO_EVENT_KEY_CHAR:
	IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN Done := TRUE;
      ALLEGRO_EVENT_DISPLAY_CLOSE:
	Done := TRUE;
      ALLEGRO_EVENT_DISPLAY_HALT_DRAWING:
	BEGIN
	  Background := TRUE;
	  al_acknowledge_drawing_halt (Event.display.source)
	END;
      ALLEGRO_EVENT_DISPLAY_RESUME_DRAWING:
	Background := FALSE;
      ALLEGRO_EVENT_DISPLAY_RESIZE:
	al_acknowledge_resize (Event.display.source);
      ALLEGRO_EVENT_TIMER:
	BEGIN
	  Update;
	  NeedRedraw := TRUE;
	END;
      END
    END
  END;

VAR
  Example: TExample;
BEGIN
  Example := TExample.Create;
  Example.Initialize;
  Example.Run;
  Example.Free
END.
