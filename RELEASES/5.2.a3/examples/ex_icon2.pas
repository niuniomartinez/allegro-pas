PROGRAM ex_icon2;
(*
 *      Example program for the Allegro library.
 *
 *      Set multiple window icons, a big one and a small one.
 *      The small would would be used for the task bar,
 *      and the big one for the alt-tab popup, for example.
 *)

{$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}

  USES
    Common,
    Allegro5, al5image;

  CONST
    NUM_ICONS = 2;

  VAR
    Display: ALLEGRO_DISPLAYptr;
    Icons: ARRAY [0..NUM_ICONS-1] OF ALLEGRO_BITMAPptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    u, v: INTEGER;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  al_install_keyboard;
  al_init_image_addon;
  InitPlatformSpecific;

  Display := al_create_display (320, 200);
  IF Display = NIL THEN AbortExample ('Could not create display');
  al_clear_to_color (al_map_rgb_f (0, 0, 0));
  al_flip_display;

{ First icon 16x16: Read from file. }
  Icons[0] := al_load_bitmap ('data/cursor.tga');
  IF Icons[0] = NIL THEN AbortExample ('cursor.tga not found');

{ Second icon 32x32: Create it. }
  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
  Icons[1] := al_create_bitmap (32, 32);
  al_set_target_bitmap (Icons[1]);
  FOR v := 0 TO 31 DO
    FOR u := 0 TO 31 DO
      al_put_pixel (u, v, al_map_rgb_f (u / 31.0, v / 31.0, 1));
  al_set_target_backbuffer (Display);

  al_set_display_icons (Display, NUM_ICONS, Icons);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_display_event_source (Display));

  WHILE TRUE DO
  BEGIN
    al_wait_for_event (Queue, Event);

    IF (Event._type = ALLEGRO_EVENT_KEY_DOWN)
    AND (Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE) THEN
      EXIT;
    IF Event._type = ALLEGRO_EVENT_DISPLAY_CLOSE THEN
      EXIT;
  END
END.
