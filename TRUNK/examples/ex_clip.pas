PROGRAM ex_clip;
(* Test performance of al_draw_bitmap_region, al_create_sub_bitmap and
 * al_set_clipping_rectangle when clipping a bitmap.
 *)

{$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}

  USES
    Common,
    Allegro5, al5font, al5color,
    math, sysutils;

  TYPE
    TExample = RECORD
      Pattern: ALLEGRO_BITMAPptr;
      Font: ALLEGRO_FONTptr;
      Queue: ALLEGRO_EVENT_QUEUEptr;
      Background, Text, White: ALLEGRO_COLOR;

      Timer, Counter: ARRAY [1..4] OF DOUBLE;
      FPS: INTEGER;
      TextX, TextY: SINGLE;
    END;

  VAR
    Ex: TExample;

  FUNCTION ExampleBitmap (CONST w, h: INTEGER): ALLEGRO_BITMAPptr;
  VAR
    Bmp: ALLEGRO_BITMAPptr;
    i, j: INTEGER;
    mx, my, a, d, l, hue, sat: SINGLE;
    State: ALLEGRO_STATE;
  BEGIN
    mx := w * 0.5;
    my := h * 0.5;
    Bmp := al_create_bitmap (w, h);
    al_store_state (state, ALLEGRO_STATE_TARGET_BITMAP);
    al_set_target_bitmap (Bmp);
    al_lock_bitmap (Bmp, ALLEGRO_PIXEL_FORMAT_ANY, ALLEGRO_LOCK_WRITEONLY);
    FOR i := 0 TO w - 1 DO
    BEGIN
      FOR j := 0 TO h - 1 DO
      BEGIN
        a := arctan2 (i - mx, j - my);
	d := Sqrt (Sqr (i - mx) + Sqr (j - my));
	l := 1 - power (1.0 - 1 / (1 + d * 0.1), 5);
	hue := a * 180 / ALLEGRO_PI;
	sat := 1;
	IF (i = 0) OR (j = 0) OR (i = w - 1) OR (j = h - 1) THEN
	  hue := hue + 180
	ELSE IF (i = 1) OR (j = 1) OR (i = w - 2) OR (j = h - 2) THEN
	BEGIN
	  hue := hue + 180;
	  sat := 0.5;
	END;
	al_put_pixel (i, j, al_color_hsl (hue, sat, l))
      END
    END;
    al_unlock_bitmap (Bmp);
    al_restore_state (State);
    ExampleBitmap := Bmp
  END;



  PROCEDURE SetXY (CONST x, y: SINGLE);
  BEGIN
    Ex.TextX := x;
    Ex.TextY := y
  END;



  PROCEDURE GetXy (VAR x, y: SINGLE);
  BEGIN
    x := Ex.TextX;
    y := Ex.TextY
  END;



  PROCEDURE Print (CONST Message: STRING);
  VAR
    th: INTEGER;
  BEGIN
    th := al_get_font_line_height (Ex.Font);

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    al_draw_text (Ex.Font, Ex.Text, Ex.TextX, Ex.TextY, 0, Message);

    Ex.TextY := Ex.TextY + th
  END;



  PROCEDURE StartTimer (CONST i: INTEGER);
  BEGIN
    Ex.Timer[i] := Ex.Timer[i] - al_get_time;
    Ex.Counter[i] := Ex.Counter[i] + 1
  END;



  PROCEDURE StopTimer (CONST i: INTEGER);
  BEGIN
    Ex.Timer[i] := Ex.Timer[i] + al_get_time
  END;



  FUNCTION GetFps (CONST i: INTEGER): DOUBLE;
  BEGIN
    IF Ex.Timer[i] = 0 THEN EXIT (0);
    GetFps := Ex.Counter[i] / Ex.Timer[i]
  END;


  PROCEDURE Draw;
  VAR
    x, y: SINGLE;
    iw, ih, cx, cy, cw, ch, gap: LONGINT;
    Temp: ALLEGRO_BITMAPptr;
  BEGIN
    x := 0; y := 0;
    iw := al_get_bitmap_width (Ex.Pattern);
    ih := al_get_bitmap_height (Ex.Pattern);
    gap := 8;

    al_get_clipping_rectangle (cx, cy, cw, ch);

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);

    al_clear_to_color (Ex.Background);

  { Test 1. }
    SetXY (8, 8);
    Print (Format ('al_draw_bitmap_region (%1f fps)', [GetFps (1)]));
    GetXy (x, y);
    al_draw_bitmap (Ex.Pattern, x, y, 0);

    StartTimer (1);
    al_draw_bitmap_region (
      Ex.Pattern,
      1, 1, iw - 2, ih - 2,
      x + 8 + iw + 1, y + 1,
      0
    );
    StopTimer (1);
    SetXY (x, y + ih + gap);

  { Test 2. }
    Print (Format ('al_create_sub_bitmap (%.1f fps)', [GetFps (2)]));
    GetXy (x, y);
    al_draw_bitmap (Ex.Pattern, x, y, 0);

    StartTimer (2);
    Temp := al_create_sub_bitmap (Ex.Pattern, 1, 1, iw - 2, ih - 2);
    al_draw_bitmap (Temp, x + 8 + iw + 1, y + 1, 0);
    al_destroy_bitmap (Temp);
    StopTimer (2);
    SetXY (x, y + ih + gap);

  { Test 3. }
    Print (Format ('al_set_clipping_rectangle (%.1f fps)', [GetFps (3)]));
    GetXy (x, y);
    al_draw_bitmap (Ex.Pattern, x, y, 0);

    StartTimer (3);
    al_set_clipping_rectangle
      (TRUNC (x + 8 + iw + 1), TRUNC (y + 1), iw - 2, ih - 2);
    al_draw_bitmap (Ex.Pattern, x + 8 + iw, y, 0);
    al_set_clipping_rectangle (cx, cy, cw, ch);
    StopTimer (3);
    SetXY (x, y + ih + gap);
  END;



  PROCEDURE Tick;
  BEGIN
    Draw;
    al_flip_display
  END;



  PROCEDURE Run;
  VAR
    Event: ALLEGRO_EVENT;
    NeedDraw: BOOLEAN;
  BEGIN
    NeedDraw := TRUE;

    WHILE TRUE DO
    BEGIN
      IF NeedDraw AND al_is_event_queue_empty (Ex.Queue) THEN
      BEGIN
	tick;
	NeedDraw := FALSE
      END;

      al_wait_for_event (Ex.Queue, Event);

      CASE Event._type OF 
      ALLEGRO_EVENT_DISPLAY_CLOSE:
	EXIT;
      ALLEGRO_EVENT_KEY_DOWN:
	IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN EXIT;
      ALLEGRO_EVENT_TIMER:
	NeedDraw := TRUE;
      END
    END
  END;



  PROCEDURE Init;
  BEGIN
    Ex.FPS := 60;

    Ex.font := al_create_builtin_font;
    IF Ex.Font = NIL THEN AbortExample ('Error creating builtin font.');
    Ex.Background := al_color_name ('beige');
    Ex.Text := al_color_name ('blue');
    Ex.White := al_color_name ('white');
    Ex.Pattern := ExampleBitmap (100, 100);
  END;



  VAR
    Display: ALLEGRO_DISPLAYptr;
    Timer: ALLEGRO_TIMERptr;

BEGIN

  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  al_install_keyboard;
  al_install_mouse;
  al_init_font_addon;
  InitPlatformSpecific;

  Display := al_create_display (640, 480);
  IF Display = NIL THEN AbortExample ('Error creating display');

  Init;

  Timer := al_create_timer (1.0 / Ex.FPS);

  Ex.Queue := al_create_event_queue;
  al_register_event_source (Ex.Queue, al_get_keyboard_event_source);
  al_register_event_source (Ex.Queue, al_get_mouse_event_source);
  al_register_event_source (Ex.Queue, al_get_display_event_source (Display));
  al_register_event_source (Ex.Queue, al_get_timer_event_source (Timer));

  al_start_timer (Timer);
  run;

  al_destroy_event_queue (Ex.Queue)
END.
