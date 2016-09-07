PROGRAM ex_filter;

  USES
    Allegro5, al5font, al5image, Common;

  CONST
    FPS = 60;
    FilterFlags: ARRAY [0..5] OF INTEGER = (
      0,
      ALLEGRO_MIN_LINEAR,
      ALLEGRO_MIPMAP,
      ALLEGRO_MIN_LINEAR OR ALLEGRO_MIPMAP,
      0,
      ALLEGRO_MAG_LINEAR
    );

    FilterText: ARRAY [0..3] OF STRING = (
      'nearest', 'linear',
      'nearest mipmap', 'linear mipmap'
    );

  TYPE
    TExample = RECORD
      Display: ALLEGRO_DISPLAYptr;
      Font: ALLEGRO_FONTptr;
      Bitmaps: ARRAY [0..1] OF ARRAY [0..8] OF ALLEGRO_BITMAPptr;
      bg, fg, Info: ALLEGRO_COLOR;
      Bitmap, Ticks: INTEGER;
    END;

  VAR
    Example: TExample;



  PROCEDURE Update;
  BEGIN
    INC (Example.Ticks)
  END;



  PROCEDURE Redraw;
  VAR
    w, h, i: INTEGER;
    x, y, bw, bh, t, Scale, Angle: REAL;
    Bmp: ALLEGRO_BITMAPptr;
  BEGIN
    w := al_get_display_width (Example.Display);
    h := al_get_display_height (Example.Display);

    al_clear_to_color (Example.bg);

    FOR i := 0 TO 5 DO
    BEGIN
      x := (i DIV 2) * w / 3 + w / 6;
      y := (i MOD 2) * h / 2 + h / 4;
      Bmp := Example.Bitmaps[Example.Bitmap][i];
      bw := al_get_bitmap_width (Bmp);
      bh := al_get_bitmap_height (Bmp);
      t := 1 - 2 * ABS ((Example.Ticks MOD (FPS * 16)) / 16 / FPS - 0.5);
      Angle := Example.Ticks * ALLEGRO_PI * 2 / FPS / 8;

      IF i < 4 THEN
         Scale := 1 - t * 0.9
      ELSE
         scale := 1 + t * 9;

      al_draw_text (
	Example.Font, Example.fg, x, y - 64 - 14,
	ALLEGRO_ALIGN_CENTRE, FilterText[i MOD 4]
      );

      al_set_clipping_rectangle (TRUNC (x - 64), TRUNC (y - 64), 128, 128);
      al_draw_scaled_rotated_bitmap (
	Bmp, bw / 2, bh / 2,
	x, y, scale, scale, angle, 0
      );
      al_set_clipping_rectangle (0, 0, w, h);
    END;
    al_draw_text (
      Example.Font, Example.Info, w / 2, h - 14,
      ALLEGRO_ALIGN_CENTRE, 'press space to change')
  END;


  CONST
    w = 640; h = 480;
  VAR
    Timer: ALLEGRO_TIMERptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Done, NeedRedraw: BOOLEAN;
    i, x, y, c: INTEGER;
    Mysha: ALLEGRO_BITMAPptr;
    Lock: ALLEGRO_LOCKED_REGIONptr;
    Row, Ptr: PBYTE;

BEGIN
  done := FALSE;
  NeedRedraw := TRUE;

  IF NOT al_init THEN
    AbortExample ('Failed to init Allegro.');

  IF NOT al_init_image_addon THEN
    AbortExample ('Failed to init IIO addon.');

  al_init_font_addon;

  InitPlatformSpecific;

  Example.Display := al_create_display (w, h);
  IF Example.Display = NIL THEN
    AbortExample ('Error creating display.');

  IF NOT al_install_keyboard THEN
    AbortExample ('Error installing keyboard.');

  IF NOT al_install_mouse THEN
    AbortExample ('Error installing mouse.');

  Example.Font := al_load_font ('data/fixed_font.tga', 0, 0);
  IF Example.Font = NIL THEN
    AbortExample ('Error loading data/fixed_font.tga');

  Mysha := al_load_bitmap ('data/mysha256x256.png');
  IF Mysha = NIL THEN
    AbortExample ('Error loading data/mysha256x256.png');

  FOR i := LOW (FilterFlags) TO HIGH (FilterFlags) DO
  BEGIN
  { Only power-of-two bitmaps can have mipmaps. }
    al_set_new_bitmap_flags (FilterFlags[i]);
    Example.Bitmaps[0][i] := al_create_bitmap (1024, 1024);
    Example.Bitmaps[1][i] := al_clone_bitmap (Mysha);
    Lock := al_lock_bitmap (
      Example.Bitmaps[0][i],
      ALLEGRO_PIXEL_FORMAT_ABGR_8888_LE, ALLEGRO_LOCK_WRITEONLY
    );
    FOR y := 0 TO 1023 DO
    BEGIN
      Row := PBYTE (Lock^.data + Lock^.pitch * y);
      Ptr := Row;
      FOR X := 0 TO 1023 DO
      BEGIN
        c := 0;
	IF (((x SHR 2) AND 1) XOR ((y SHR 2) AND 1)) <> 0 THEN c := 255;
	Ptr^ := c; INC (ptr);
	Ptr^ := c; INC (ptr);
	Ptr^ := c; INC (ptr);
	Ptr^ := 255; INC (ptr)
      END
    END;
    al_unlock_bitmap (Example.Bitmaps[0][i])
  END;

  Example.bg := al_map_rgb_f (0, 0, 0);
  Example.fg := al_map_rgb_f (1, 1, 1);
  Example.info := al_map_rgb_f (0.5, 0.5, 1);

  Timer := al_create_timer (1 / FPS);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_mouse_event_source);
  al_register_event_source (Queue, al_get_timer_event_source (Timer));
  al_register_event_source (Queue, al_get_display_event_source (Example.Display));

  al_start_timer (Timer);

  REPEAT
    IF NeedRedraw AND al_is_event_queue_empty (Queue) THEN
    BEGIN
      Redraw;
      al_flip_display;
      NeedRedraw := FALSE;
    END;

    al_wait_for_event (Queue, Event);
    CASE Event._type OF
    ALLEGRO_EVENT_KEY_DOWN:
      BEGIN
	IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN Done := TRUE;
	IF Event.keyboard.keycode = ALLEGRO_KEY_SPACE THEN
	  Example.Bitmap := (Example.Bitmap + 1) MOD 2
      END;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Done := TRUE;
    ALLEGRO_EVENT_TIMER:
      BEGIN
	Update;
	NeedRedraw := TRUE
      END;
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
      Example.Bitmap := (Example.Bitmap + 1) MOD 2
    END
  UNTIL Done;

  FOR i := 0 TO 5 DO
  BEGIN
    al_destroy_bitmap (Example.Bitmaps[0][i]);
    al_destroy_bitmap (Example.Bitmaps[1][i])
  END
END.
