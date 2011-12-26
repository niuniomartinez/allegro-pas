PROGRAM ex_font;
(*<An example. *)
(* TODO: License. *)

  USES
    Common,
    Allegro5, al5font, al5image;

  CONST
    EURO = #226#82#172;

  PROCEDURE WaitForEsc (Display: ALLEGRO_DISPLAYptr);
  VAR
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    ScreenClone: ALLEGRO_BITMAPptr;
    EndLoop: BOOLEAN;
    x, y, w, h: INTEGER;
  BEGIN
    al_install_keyboard;
    Queue := al_create_event_queue;
    al_register_event_source (Queue, al_get_keyboard_event_source);
    al_register_event_source (Queue, al_get_display_event_source (Display));
    ScreenClone := al_clone_bitmap (al_get_target_bitmap);
    EndLoop := FALSE;
    REPEAT
      al_wait_for_event (Queue, @Event);
      IF Event._type = ALLEGRO_EVENT_DISPLAY_CLOSE THEN
	EndLoop := TRUE
      ELSE IF Event._type = ALLEGRO_EVENT_KEY_DOWN THEN
	BEGIN
	  IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
	    EndLoop := TRUE;
	END
	
      ELSE IF Event._type = ALLEGRO_EVENT_DISPLAY_EXPOSE THEN
	BEGIN
	  al_draw_bitmap_region (
	    ScreenClone,
	    Event.display.x, Event.display.y,
	    Event.display.width, Event.display.height,
	    Event.display.x, Event.display.y,
	    0
	  );
	  al_update_display_region(x, y, w, h);
	END;
    UNTIL EndLoop;
    al_destroy_bitmap (ScreenClone);
    al_destroy_event_queue (Queue);
  END;



VAR
  Display: ALLEGRO_DISPLAYptr;
  Bitmap, FontBitmap: ALLEGRO_BITMAPptr;
  Font, A4Font: ALLEGRO_FONTptr;
  Ranges: ARRAY [0..7] OF LONGINT = (
    $0020, $007F, { ASCII }
    $00A1, $00FF, { Latin 1 }
    $0100, $017F, { Extended-A }
    $20AC, $20AC  { Euro }
  );
BEGIN
  IF NOT al_init THEN
    AbortExample ('Could not init Allegro.');
  al_init_image_addon;
  al_init_font_addon;

  al_set_new_display_option (ALLEGRO_SINGLE_BUFFER, 0, ALLEGRO_SUGGEST);
  al_set_new_display_flags (ALLEGRO_GENERATE_EXPOSE_EVENTS);
  Display := al_create_display (320, 200);
  IF Display = NIL THEN
    AbortExample ('Failed to create display.');

  Bitmap := al_load_bitmap ('data/mysha.pcx');
  IF Bitmap = NIL THEN
    AbortExample ('Failed to load misha.pcx.');

  Font := al_load_font ('data/bmpfont.tga', 0, 0);
  IF Font = NIL THEN
    AbortExample ('Failed to load bmpfont.tga.');

  FontBitmap := al_load_bitmap ('data/a4_font.tga');
  IF FontBitmap = NIL THEN
    AbortExample ('Failed to load a4_font.tga.');

  A4Font := al_grab_font_from_bitmap (FontBitmap, 4, Ranges);

{ Draw background }
  al_draw_bitmap (Bitmap, 0, 0, 0);

{ Draw red text }
  al_draw_text (Font, al_map_rgb (255, 0, 0), 10, 10, 0, 'red');

{ Draw green text }
  al_draw_text (Font, al_map_rgb (0, 255, 0), 10, 50, 0, 'green');

{ Draw a unicode symbol }
  al_draw_text (A4Font, al_map_rgb (0, 0, 255), 10, 90, 0, 'Mysha''s 0.02' + EURO);

  al_flip_display;

  WaitForEsc (Display);

  al_destroy_bitmap (Bitmap);
  al_destroy_font (Font);
  al_destroy_bitmap (FontBitmap);
  al_destroy_font (A4Font);
END.
