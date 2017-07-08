PROGRAM ex_draw;
(* Tests some drawing primitives. *)

{$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}

  USES
    Common,
    Allegro5, al5image, al5font, al5color, al5primitives,
    sysutils;

  TYPE
    RExample = RECORD
      Font: ALLEGRO_FONTptr;
      Queue: ALLEGRO_EVENT_QUEUEptr;
      Background, Text, White, Foreground: ALLEGRO_COLOR;
      Outline: ALLEGRO_COLOR;
      Pattern: ALLEGRO_BITMAPptr;
      Zoom: ALLEGRO_BITMAPptr;

      Timer, Counter: ARRAY [0..3] OF DOUBLE;
      FPS: INTEGER;
      TextX, TextY: REAL;

      Software: BOOLEAN;
      Samples: INTEGER;
      What: INTEGER;
      Thickness: INTEGER;
    END;

  VAR
   Ex: RExample;

  CONST
    Names: ARRAY [0..4] OF STRING = (
      'filled rectangles',
      'rectangles',
      'filled circles',
      'circles',
      'lines'
    );

  PROCEDURE DrawPattern (Bmp: ALLEGRO_BITMAPptr);
  VAR
    w, h, x, y: INTEGER;
    Format: ALLEGRO_PIXEL_FORMAT;
    Light, Dark, c: ALLEGRO_COLOR;
    Lock: ALLEGRO_LOCKED_REGIONptr;
    r, g, b: BYTE;
    Data: PBYTE;
  BEGIN
    w := al_get_bitmap_width (Bmp);
    h := al_get_bitmap_height (Bmp);
    Format := ALLEGRO_PIXEL_FORMAT_BGR_888;
    Light := al_map_rgb_f (1, 1, 1);
    Dark := al_map_rgb_f (1, 0.9, 0.8);
    Lock := al_lock_bitmap (Bmp, format, ALLEGRO_LOCK_WRITEONLY);
    FOR Y := 0 TO (h - 1) DO
    BEGIN
      FOR X := 0 TO (w - 1) DO
      BEGIN
        IF ((x + y) AND 1) <> 0 THEN c := Light ELSE c := Dark;
        Data := Lock^.data;
        al_unmap_rgb (c, r, g, b);
        Data := Data + (y * Lock^.pitch);
        Data := Data + (x * 3);
        Data[0] := r;
        Data[1] := g;
        Data[2] := b
      END
    END;
    al_unlock_bitmap (Bmp)
  END;



  PROCEDURE SetXY (x, y: REAL);
  BEGIN
    Ex.TextX := x;
    Ex.TextY := y
  END;



  PROCEDURE Print (CONST aText: STRING);
  VAR
    State: ALLEGRO_STATE;
    th: INTEGER;
  BEGIN
    th := al_get_font_line_height (Ex.Font);
    al_store_state(&state, ALLEGRO_STATE_BLENDER);
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    al_draw_text (Ex.Font, Ex.Text, Ex.TextX, Ex.TextY, 0, aText);
    al_restore_state (state);
    Ex.TextY := Ex.TextY + th
  END;



  PROCEDURE Primitive (l, t, r, b: REAL; Clr: ALLEGRO_COLOR; NoFill: BOOLEAN);
  VAR
    cx, cy, rx, ry: REAL;
    tk, w: INTEGER;
  BEGIN
    cx := (l + r) / 2;
    cy := (t + b) / 2;
    rx := (r - l) / 2;
    ry := (b - t) / 2;
    IF NoFill THEN tk := 0 ELSE tk := Ex.Thickness;
    w := Ex.What;
    IF NoFill THEN
    BEGIN
      IF w = 0 THEN w := 1 ELSE IF w = 2 THEN w := 3
    END;
    CASE w OF
      0: al_draw_filled_rectangle (l, t, r, b, Clr);
      1: al_draw_rectangle (l, t, r, b, Clr, tk);
      2: al_draw_filled_ellipse (cx, cy, rx, ry, Clr);
      3: al_draw_ellipse (cx, cy, rx, ry, Clr, tk);
      4: al_draw_line (l, t, r, b, Clr, tk);
    END
  END;



  PROCEDURE Draw;
  VAR
    x, y: REAL;
    cx, cy, cw, ch, w, h, RectsNum, i, j: LONGINT;
    Screen, mem: ALLEGRO_BITMAPptr;
    Rects: ARRAY [0..(16 * 4) - 1] OF REAL;
    rgba: ALLEGRO_COLOR;
    sw: STRING;
  BEGIN
    w := al_get_bitmap_width (Ex.Zoom);
    h := al_get_bitmap_height (Ex.Zoom);
    Screen := al_get_target_bitmap;
    RectsNum := 16;
    FOR j := 0 TO 3 DO
      FOR i := 0 TO 3 DO
      BEGIN
        Rects[(j * 4 + i) * 4 + 0] := 2 + i * 0.25 + i * 7;
        Rects[(j * 4 + i) * 4 + 1] := 2 + j * 0.25 + j * 7;
        Rects[(j * 4 + i) * 4 + 2] := 2 + i * 0.25 + i * 7 + 5;
        Rects[(j * 4 + i) * 4 + 3] := 2 + j * 0.25 + j * 7 + 5;
      END;
    al_get_clipping_rectangle (cx, cy, cw, ch);
    al_clear_to_color (Ex.Background);

    SetXY (8, 0);
    Print (Format ('Drawing %s (press SPACE to change)', [Names[Ex.What]]));

    SetXY(8, 16);
    Print ('Original');

    SetXY (80, 16);
    Print ('Enlarged x 16');

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);

    IF Ex.Software THEN
    BEGIN
      al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
      al_set_new_bitmap_format (al_get_bitmap_format (al_get_target_bitmap));
      mem := al_create_bitmap (w, h);
      al_set_target_bitmap (mem);
      x := 0;
      y := 0;
      sw := 'software'
    END
    ELSE BEGIN
      mem := NIL;
      x := 8;
      y := 40;
      sw := 'hardware'
    END;
    al_draw_bitmap (Ex.Pattern, x, y, 0);

  { Draw the test scene. }

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    FOR i := 0 TO (RectsNum - 1) DO
    BEGIN
      rgba := Ex.Foreground;
      rgba.a := rgba.a * 0.5; { May be "/ 2" is better? }
      Primitive (
         x + Rects[i * 4 + 0],
         y + Rects[i * 4 + 1],
         x + Rects[i * 4 + 2],
         y + Rects[i * 4 + 3],
         rgba, FALSE)
    END;

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);

    IF Ex.Software THEN
    BEGIN
      al_set_target_bitmap(screen);
      x := 8;
      y := 40;
      al_draw_bitmap(mem, x, y, 0);
      al_destroy_bitmap(mem);
    END;

  { Grab screen contents into our bitmap. }
    al_set_target_bitmap (Ex.Zoom);
    al_draw_bitmap_region (screen, x, y, w, h, 0, 0, 0);
    al_set_target_bitmap (screen);

  { Draw it enlarged. }
    x := 80;
    y := 40;
    al_draw_scaled_bitmap (Ex.Zoom, 0, 0, w, h, x, y, w * 16, h * 16, 0);

  { Draw outlines. }
    FOR i := 0 TO (RectsNum - 1) DO
      Primitive (
         x + Rects[i * 4 + 0] * 16,
         y + Rects[i * 4 + 1] * 16,
         x + Rects[i * 4 + 2] * 16,
         y + Rects[i * 4 + 3] * 16,
         Ex.Outline, TRUE);

    SetXY (8, 640 - 48);
    Print (Format ('Thickness: %d (press T to change)', [Ex.Thickness]));
    Print (Format ('Drawing with: %s (press S to change)', [sw]));
    Print (Format ('Supersampling: %dx (edit ex_draw.ini to change)', [Ex.Samples]));

{ FIXME: doesn't work
      al_get_display_option (ALLEGRO_SAMPLE_BUFFERS));
  }
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
        Tick;
        NeedDraw := FALSE
      END;

      al_wait_for_event (Ex.Queue, Event);

      CASE Event._type OF
      ALLEGRO_EVENT_DISPLAY_CLOSE:
        EXIT;
      ALLEGRO_EVENT_KEY_DOWN:
        CASE Event.keyboard.keycode OF
        ALLEGRO_KEY_ESCAPE:
          EXIT;
        ALLEGRO_KEY_SPACE:
          BEGIN
            INC (Ex.What);
            IF Ex.What >= 5 THEN Ex.What := 0
          END;
        ALLEGRO_KEY_S:
          Ex.Software := NOT Ex.Software;
        ALLEGRO_KEY_T:
          BEGIN
            INC (Ex.Thickness);
            IF Ex.Thickness >= 2 THEN Ex.Thickness := 0
          END;
        END;
      ALLEGRO_EVENT_TIMER:
        NeedDraw := TRUE;
      END
    END
  END;



  PROCEDURE Init;
  BEGIN
    Ex.FPS := 60;

    Ex.Font := al_load_font ('data/fixed_font.tga', 0, 0);
    IF Ex.Font = NIL THEN AbortExample ('data/fixed_font.tga not found.');
    Ex.Background := al_color_name ('beige');
    Ex.Foreground := al_color_name ('black');
    Ex.Outline := al_color_name ('red');
    Ex.Text := al_color_name ('blue');
    Ex.White := al_color_name ('white');
    Ex.Pattern := al_create_bitmap (32, 32);
    Ex.Zoom := al_create_bitmap (32, 32);
    DrawPattern (Ex.Pattern);
  END;



VAR
  Display: ALLEGRO_DISPLAYptr;
  Timer: ALLEGRO_TIMERptr;
{ TODO Not yet implemented.
  Config: ALLEGRO_CONFIGptr;
  Value, Str: STRING;
}
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  al_init_primitives_addon;
  al_install_keyboard;
  al_install_mouse;
  al_init_image_addon;
  al_init_font_addon;
  InitPlatformSpecific;

{ Read supersampling info from ex_draw.ini. }
  Ex.Samples := 0;
{   NOTE: Currently disabled as isn't implemented. Use TIni objects from FCL?

   config = al_load_config_file("ex_draw.ini");
   if (!config)
      config = al_create_config();
   value = al_get_config_value(config, "settings", "samples");
   if (value)
      ex.samples = strtol(value, NULL, 0);
   sprintf(str, "%d", ex.samples);
   al_set_config_value(config, "settings", "samples", str);
   al_save_config_file("ex_draw.ini", config);
   al_destroy_config(config);
}
  IF Ex.samples > 0 THEN
  BEGIN
    al_set_new_display_option (ALLEGRO_SAMPLE_BUFFERS, 1, ALLEGRO_REQUIRE);
    al_set_new_display_option (ALLEGRO_SAMPLES, Ex.Samples, ALLEGRO_SUGGEST)
  END;
  Display := al_create_display (640, 640);
  IF Display = NIL THEN AbortExample ('Unable to create display.');

  Init;

  Timer := al_create_timer (1.0 / Ex.FPS);

  Ex.Queue := al_create_event_queue;
  al_register_event_source (Ex.Queue, al_get_keyboard_event_source);
  al_register_event_source (Ex.Queue, al_get_mouse_event_source);
  al_register_event_source (Ex.Queue, al_get_display_event_source (Display));
  al_register_event_source (Ex.Queue, al_get_timer_event_source (Timer));

  al_start_timer (Timer);
  Run;

  al_destroy_event_queue (Ex.Queue)
END.
