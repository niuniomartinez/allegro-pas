PROGRAM ex_membmp;

{$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}

  USES
    Common,
    Allegro5, al5font, al5Image,
    sysutils;

  PROCEDURE Print (aFont: ALLEGRO_FONTptr; Message: STRING; x, y: INTEGER);
  BEGIN
    al_draw_text (aFont, al_map_rgb (0, 0, 0), x+2, y+2, 0, Message);
    al_draw_text (aFont, al_map_rgb (255, 255, 255), x, y, 0, Message)
  END;



  FUNCTION Test
    (Bitmap: ALLEGRO_BITMAPptr; Font: ALLEGRO_FONTptr; Message: STRING)
    : BOOLEAN;
  VAR
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    StartTime, FPS: DOUBLE;
    Frames: LONGINT;
  BEGIN
    Frames := 0;
    FPS := 0;

    Queue := al_create_event_queue;
    al_register_event_source (Queue, al_get_keyboard_event_source);

    StartTime := al_get_time;

    WHILE TRUE DO
    BEGIN
      IF al_get_next_event (Queue, Event) THEN
      BEGIN
        IF Event._type = ALLEGRO_EVENT_KEY_DOWN THEN
	BEGIN
	  IF Event.keyboard.keycode = ALLEGRO_KEY_SPACE THEN
	  BEGIN
	    al_destroy_event_queue (Queue);
	    EXIT (FALSE)
	  END;
	  IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
	  BEGIN
	    al_destroy_event_queue (Queue);
	    EXIT (TRUE)
	  END
	END
      END;

      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);

    { Clear the backbuffer with red so we can tell if the bitmap does not
      cover the entire backbuffer. }
      al_clear_to_color (al_map_rgb (255, 0, 0));

      al_draw_scaled_bitmap (
	Bitmap, 0, 0,
	al_get_bitmap_width (Bitmap),
	al_get_bitmap_height (Bitmap),
	0, 0,
	al_get_bitmap_width (al_get_target_bitmap),
	al_get_bitmap_height (al_get_target_bitmap),
	0
      );

      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);

    { Note this makes the memory buffer case much slower due to repeated
      locking of the backbuffer.  Officially you can't use al_lock_bitmap
      to solve the problem either. }
      Print (Font, Message, 0, 0);
      Print (
	Font, Format ('%.1f FPS', [FPS]), 0,
	al_get_font_line_height (Font) + 5
      );

      al_flip_display;

      INC (Frames);
      FPS := Frames / (al_get_time - StartTime)
    END
  END;



  VAR
    Display: ALLEGRO_DISPLAYptr;
    AccelFont, MemFont: ALLEGRO_FONTptr;
    AccelBmp, MemBmp: ALLEGRO_BITMAPptr;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  al_install_keyboard;
  al_init_image_addon;
  al_init_font_addon;
  InitPlatformSpecific;

  Display := al_create_display (640, 400);
  IF Display = NIL THEN AbortExample ('Error creating display.');

  AccelFont := al_load_font ('data/font.tga', 0, 0);
  IF AccelFont = NIL THEN AbortExample ('font.tga not found');
  AccelBmp := al_load_bitmap ('data/mysha.pcx');
  IF AccelBmp = NIL THEN AbortExample ('mysha.pcx not found');

  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);

  MemFont := al_load_font ('data/font.tga', 0, 0);
  MemBmp := al_load_bitmap ('data/mysha.pcx');

  REPEAT
    IF Test (MemBmp, MemFont, 'Memory bitmap (press SPACE key)') THEN EXIT;
    IF Test (AccelBmp, AccelFont, 'Accelerated bitmap (press SPACE key)') THEN EXIT
  UNTIL FALSE
END.
