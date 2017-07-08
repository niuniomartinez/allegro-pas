PROGRAM ex_cpu;
(* An example showing the use of al_get_cpu_count and al_get_ram_size. *)

{$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}

  USES
    Allegro5, al5font, Common,
    sysutils;

  CONST
    INTERVAL = 0.1;

  VAR
    Display: ALLEGRO_DISPLAYptr;
    Timer: ALLEGRO_TIMERptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Font: ALLEGRO_FONTptr;
    Done, Redraw: BOOLEAN;

BEGIN
  Done := FALSE;
  Redraw := TRUE;

  IF NOT al_init THEN
    AbortExample ('Failed to init Allegro.');

  al_init_font_addon;
  InitPlatformSpecific;

  Display := al_create_display (640, 480);
  IF Display = NIL THEN
    AbortExample ('Error creating display.');

  IF NOT al_install_keyboard THEN
    AbortExample ('Error installing keyboard.');

  Font := al_create_builtin_font;

  Timer := al_create_timer (INTERVAL);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_timer_event_source (Timer));
  al_register_event_source (Queue, al_get_display_event_source (Display));

  al_start_timer (Timer);

  al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);

  REPEAT
    IF Redraw AND al_is_event_queue_empty (Queue) THEN
    BEGIN
      al_clear_to_color (al_map_rgba_f (0, 0, 0, 1));
      al_draw_text (
        Font, al_map_rgba_f (1, 1, 0, 1), 16, 16, 0,
	Format ('Amount of CPU cores detected: %d.', [al_get_cpu_count])
      );
      al_draw_text (
        Font, al_map_rgba_f (0, 1, 1, 1), 16, 32, 0,
	Format ('Size of random access memory: %d MiB.', [al_get_ram_size])
      );
      al_flip_display;
      Redraw := FALSE;
    END;

    al_wait_for_event (Queue, Event);
    CASE event._type OF
    ALLEGRO_EVENT_KEY_DOWN:
      IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN Done := TRUE;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Done := TRUE;
    ALLEGRO_EVENT_TIMER:
      Redraw := TRUE;
    END
  UNTIL Done;

  al_destroy_font (Font)
END.
