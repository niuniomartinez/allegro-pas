PROGRAM ex_icon;

{$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}

  USES
    Common,
    Allegro5, al5image;

  VAR
    Display: ALLEGRO_DISPLAYptr;
    Icon1, Icon2: ALLEGRO_BITMAPptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Timer: ALLEGRO_TIMERptr;
    i, u, v: INTEGER;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  al_install_keyboard;
  al_init_image_addon;
  InitPlatformSpecific;

  Display := al_create_display (320, 200);
  IF Display = NIL THEN AbortExample ('Could not create display');
  al_clear_to_color (al_map_rgb_f (0, 0, 0));
  al_flip_display;

{ First icon: Read from file. }
  Icon1 := al_load_bitmap ('data/icon.tga');
  IF Icon1 = NIL THEN AbortExample ('icon.tga not found');

{ Second icon: Create it. }
  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
  Icon2 := al_create_bitmap (16, 16);
  al_set_target_bitmap (Icon2);
  FOR i := 0 TO 255 DO
  BEGIN
    u := i MOD 16;
    v := i DIV 16;
    al_put_pixel (u, v, al_map_rgb_f (u / 15.0, v / 15.0, 1))
  END;
  al_set_target_backbuffer (Display);

  al_set_window_title (Display, 'Changing icon example');

  Timer := al_create_timer (0.5);
  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_display_event_source (Display));
  al_register_event_source (Queue, al_get_timer_event_source (Timer));
  al_start_timer (Timer);

  WHILE TRUE DO
  BEGIN
    al_wait_for_event (Queue, Event);

    IF (Event._type = ALLEGRO_EVENT_KEY_DOWN)
    AND (Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE) THEN
      EXIT;
    IF Event._type = ALLEGRO_EVENT_DISPLAY_CLOSE THEN
      EXIT;
    IF Event._type = ALLEGRO_EVENT_TIMER THEN
    BEGIN
      IF (Event.timer.count AND 1) <> 0 THEN
	al_set_display_icon (Display, Icon2)
      ELSE
	al_set_display_icon (Display, Icon1)
    END
  END
END.
