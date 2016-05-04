PROGRAM ex_windows;

USES
  Common,
  Allegro5, al5font, al5image;

CONST
  W = 100;
  H = 100;
  Margin = 20;

VAR
  Displays: ARRAY [0..1] OF ALLEGRO_DISPLAYptr;
  Info: ARRAY OF  ALLEGRO_MONITOR_INFO;
  x, y, a, dw, dh: INTEGER;
  MyFont: ALLEGRO_FONTptr;
  Events: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  i: INTEGER;
  EndExample: BOOLEAN;

BEGIN
  Randomize;

  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  al_install_mouse;
  al_init_font_addon;
  al_init_image_addon;

  SetLength (Info, al_get_num_video_adapters);

  FOR i := LOW (Info) TO HIGH (Info) DO al_get_monitor_info (i, Info[i]);

  x := TRUNC (((info[0].x2 - info[0].x1) / 3) - (W / 2));
  y := TRUNC (((info[0].y2 - info[0].y1) / 2) - (H / 2));

  al_set_new_window_position (x, y);

  Displays[0] := al_create_display (W, H);

  x := x * 2;
  al_set_new_window_position (x, y);

  Displays[1] := al_create_display (W, H);

  if (Displays[0] = NIL) OR (Displays[1] = NIL) THEN 
    AbortExample ('Could not create displays.');

  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
  MyFont := al_load_font ('data/fixed_font.tga', 0, 0);
  if MyFont = NIL THEN  AbortExample ('Could not load font.');

  events := al_create_event_queue;
  al_register_event_source (Events, al_get_mouse_event_source);
  al_register_event_source (Events, al_get_display_event_source (Displays[0]));
  al_register_event_source (Events, al_get_display_event_source (Displays[1]));

  EndExample := FALSE;
  REPEAT
    FOR i := LOW (Displays) TO HIGH (Displays) DO
    BEGIN
      al_set_target_backbuffer (Displays[i]);
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      IF i = 0 THEN
         al_clear_to_color (al_map_rgb (255, 0, 255))
      ELSE
         al_clear_to_color (al_map_rgb (155, 255, 0));
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_draw_text (myfont, al_map_rgb (0, 0, 0), 50, 50, ALLEGRO_ALIGN_CENTRE, 'Click me...');
      al_flip_display
    END;

    IF al_wait_for_event_timed (events, event, 1) THEN
    BEGIN
      IF Event._type = ALLEGRO_EVENT_DISPLAY_CLOSE THEN
	EndExample := TRUE
      ELSE IF event._type = ALLEGRO_EVENT_MOUSE_BUTTON_DOWN THEN
      BEGIN
	a := Random (Length (Info));
	dw := Info[a].x2 - Info[a].x1;
	dh := Info[a].y2 - Info[a].y1;
	x := Margin + Info[a].x1 + (Random (dw - W - Margin));
	y := Margin + Info[a].y1 + (Random (dh - H - Margin));
	al_set_window_position (Event.mouse.display, x, y);
      END
    END
  UNTIL EndExample;

  al_destroy_event_queue (Events);

  al_destroy_display (Displays[0]);
  al_destroy_display (Displays[1]);
END.
