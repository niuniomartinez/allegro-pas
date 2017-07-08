PROGRAM ex_display_events;

{$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}

  USES
    Common,
    Allegro5, al5font, al5primitives,
    sysutils;

  CONST
    MAX_EVENTS = 23;

  VAR
    Events: ARRAY [1..MAX_EVENTS] OF ANSISTRING;



  PROCEDURE AddEvent (CONST Message: STRING);
  VAR
    Ndx: INTEGER;
  BEGIN
    FOR Ndx := HIGH (Events) - 1 DOWNTO LOW (Events) DO
      Events[Ndx + 1] := Events[Ndx];
    Events[LOW (Events)] := Message
  END;



  VAR
    Display: ALLEGRO_DISPLAYptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Font: ALLEGRO_FONTptr;
    Color, Black, Red, Blue: ALLEGRO_COLOR;
    i: INTEGER;
    EndExample: BOOLEAN;
    x, y: SINGLE;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  al_init_primitives_addon;
  al_install_mouse;
  al_install_keyboard;
  al_init_font_addon;

  al_set_new_display_flags (ALLEGRO_RESIZABLE);
  Display := al_create_display (640, 480);
  IF Display = NIL THEN AbortExample ('Error creating display');

  Font := al_create_builtin_font;
  IF Font = NIL THEN AbortExample ('Error creating builtin font');

  Black := al_map_rgb_f (0, 0, 0);
  Red := al_map_rgb_f (1, 0, 0);
  Blue := al_map_rgb_f (0, 0, 1);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_mouse_event_source);
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_display_event_source (Display));

  EndExample := FALSE;
  REPEAT
    IF al_is_event_queue_empty (Queue) THEN
    BEGIN
      x := 8; y := 28;
      al_clear_to_color (al_map_rgb ($FF, $FF, $C0));

      al_draw_text (Font, Blue, 8, 8, 0, 'Display events (newest on top)');

      Color := Red;
      FOR i := LOW (Events) TO HIGH (Events) DO
      BEGIN
        IF Events[i] <> '' THEN
        BEGIN
          al_draw_text (Font, Color, x, y, 0, Events[i]);
          Color := Black;
          y := y + 20
        END
      END;
      al_flip_display
    END;

    al_wait_for_event (Queue, Event);
    CASE Event._type OF
    ALLEGRO_EVENT_MOUSE_ENTER_DISPLAY:
      AddEvent ('ALLEGRO_EVENT_MOUSE_ENTER_DISPLAY');
    ALLEGRO_EVENT_MOUSE_LEAVE_DISPLAY:
      AddEvent ('ALLEGRO_EVENT_MOUSE_LEAVE_DISPLAY');
    ALLEGRO_EVENT_KEY_DOWN:
      IF Event.Keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN EndExample := TRUE;
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      BEGIN
        AddEvent (Format (
          'ALLEGRO_EVENT_DISPLAY_RESIZE x=%d, y=%d, width=%d, height=%d',
          [
            Event.display.x, Event.display.y, Event.display.width,
            Event.display.height
          ]
        ));
        al_acknowledge_resize (Event.display.source)
      END;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      AddEvent ('ALLEGRO_EVENT_DISPLAY_CLOSE');
    ALLEGRO_EVENT_DISPLAY_LOST:
      AddEvent ('ALLEGRO_EVENT_DISPLAY_LOST');
    ALLEGRO_EVENT_DISPLAY_FOUND:
      AddEvent ('ALLEGRO_EVENT_DISPLAY_FOUND');
    ALLEGRO_EVENT_DISPLAY_SWITCH_OUT:
      AddEvent ('ALLEGRO_EVENT_DISPLAY_SWITCH_OUT');
    ALLEGRO_EVENT_DISPLAY_SWITCH_IN:
      AddEvent ('ALLEGRO_EVENT_DISPLAY_SWITCH_IN');
    END
  UNTIL EndExample;

  al_destroy_event_queue (Queue)
END.
