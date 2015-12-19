PROGRAM ex_bitmap;
(*
 * Example with bitmaps.
 *)

USES
  Common,
  allegro5, al5image,
  sysutils;
VAR
  FileName: STRING;
  MemBitmap, Bitmap: ALLEGRO_BITMAPptr;
  Timer: ALLEGRO_TIMERptr;
  Display: ALLEGRO_DISPLAYptr;
  EventQueue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Redraw, EndLoop: BOOLEAN;
  Zoom, T0, T1: DOUBLE;
BEGIN
  IF ParamCount > 0 THEN
    FileName := ParamStr (1)
  ELSE
    FileName := 'data/mysha.pcx';

  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  OpenLog;

  IF ParamCount > 2 THEN al_set_new_display_adapter (StrToInt (ParamStr(2)));

  al_install_keyboard;
  al_init_image_addon;

  Display := al_create_display (640, 480);
  IF Display = NIL THEN AbortExample ('Could not create display');

  al_set_window_title (Display, FileName);

  Redraw := TRUE;
  Zoom := 1;

{ We load the bitmap into a memory bitmap, because creating a
  display bitmap could fail if the bitmap is too big to fit into a
  single texture.
  FIXME: Or should A5 automatically created multiple display bitmaps?
}
  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
  T0 := al_get_time;
  MemBitmap := al_load_bitmap (FileName);
  t1 := al_get_time;
  IF MemBitmap = NIL THEN
    AbortExample (Format ('"%s" not found or failed to load.', [filename]));
  al_set_new_bitmap_flags (ALLEGRO_VIDEO_BITMAP);

  LogWriteLn (Format ('Loading took %.4f seconds', [t1 - t0]));
    
{ FIXME:
  Now try to split the memory bitmap into display bitmaps? }
  Bitmap := al_clone_bitmap (MemBitmap);
  IF Bitmap = NIL THEN Bitmap := MemBitmap;

  Timer := al_create_timer (1.0 / 30);
  EventQueue := al_create_event_queue;
  al_register_event_source (EventQueue, al_get_keyboard_event_source);
  al_register_event_source (EventQueue, al_get_display_event_source (Display));
  al_register_event_source (EventQueue, al_get_timer_event_source (Timer));
  al_start_timer (Timer);

  EndLoop := FALSE;
  REPEAT
    al_wait_for_event (EventQueue, Event);
    CASE Event._type OF
    ALLEGRO_EVENT_DISPLAY_ORIENTATION:
      CASE Event.display.orientation OF
      ALLEGRO_DISPLAY_ORIENTATION_0_DEGREES:
        LogWriteLn ('0 degrees');
      ALLEGRO_DISPLAY_ORIENTATION_90_DEGREES:
        LogWriteLn ('90 degrees');
      ALLEGRO_DISPLAY_ORIENTATION_180_DEGREES:
        LogWriteLn ('180 degrees');
      ALLEGRO_DISPLAY_ORIENTATION_270_DEGREES:
        LogWriteLn ('270 degrees');
      ALLEGRO_DISPLAY_ORIENTATION_FACE_UP:
        LogWriteLn ('Face up');
      ALLEGRO_DISPLAY_ORIENTATION_FACE_DOWN:
        LogWriteLn ('Face down');
      END;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EndLoop := TRUE;
    ALLEGRO_EVENT_KEY_CHAR:
      BEGIN
        IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
          EndLoop := TRUE;
        IF event.keyboard.unichar = ORD ('1') THEN
          Zoom := 1;
        IF event.keyboard.unichar = ORD ('+') THEN
          Zoom := Zoom * 1.1;
        IF event.keyboard.unichar = ORD ('-') THEN
          Zoom := Zoom / 1.1;
        IF event.keyboard.unichar = ORD ('f') THEN
          Zoom := al_get_display_width (Display) / al_get_bitmap_width (Bitmap);
      END;
    ALLEGRO_EVENT_TIMER:
      Redraw := TRUE;
    END;
            
    IF Redraw AND  al_is_event_queue_empty (EventQueue) THEN
    BEGIN
      Redraw := FALSE;
      al_clear_to_color (al_map_rgb_f (0, 0, 0));
      IF Zoom = 1 THEN
        al_draw_bitmap (Bitmap, 0, 0, 0)
      ELSE
        al_draw_scaled_rotated_bitmap (
          Bitmap, 0, 0, 0, 0, Zoom, Zoom, 0, 0);
      al_flip_display;
    END;
  UNTIL EndLoop;

  al_destroy_bitmap (Bitmap);

  CloseLog (FALSE);
END.

/* vim: set sts=4 sw=4 et: */
