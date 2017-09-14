PROGRAM ex_blend;
(* An example demonstrating different blending modes. *)

USES
  Common,
  Allegro5, al5Font, al5Image, al5Primitives,
  sysutils;


VAR
(* A structure holding all variables of our example program. *)
  Ex: RECORD
   Example: ALLEGRO_BITMAPptr; (* Our example bitmap. *)
   OffScreen: ALLEGRO_BITMAPptr; (* An offscreen buffer, for testing. *)
   Memory: ALLEGRO_BITMAPptr; (* A memory buffer, for testing. *)
   MyFont: ALLEGRO_FONTptr; (* Our font. *)
   Queue: ALLEGRO_EVENT_QUEUEptr; (* Our events queue. *)
   Image: INTEGER; (* Which test image to use. *)
   Mode: INTEGER; (* How to draw it. *)
   BUTTONS_X: INTEGER; (* Where to draw buttons. *)

   FPS: INTEGER;
   LastSecond: DOUBLE;
   FramesAccum: INTEGER;
   fFPS: DOUBLE;
  END;



(* Print some text with a shadow. *)
  PROCEDURE Print
    (CONST aX, aY: INTEGER; CONST aVertical: BOOLEAN; CONST Text: STRING);
  VAR
    Color: ALLEGRO_COLOR;
    h, i, j: INTEGER;
    ui, Letter: ALLEGRO_USTR_INFO;
    us: ALLEGRO_USTRptr;
  BEGIN
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    h := al_get_font_line_height (Ex.MyFont);

    FOR j := 0 TO 1 DO
    BEGIN
      IF j = 0 THEN
        Color := al_map_rgb (0, 0, 0)
      ELSE
        Color := al_map_rgb (255, 255, 255);

      IF aVertical THEN
      BEGIN
        us := al_ref_cstr (ui, Text);
        FOR i := 0 TO al_ustr_length (us) - 1 DO
        BEGIN
          al_draw_ustr (
            Ex.MyFont, Color, aX + 1 - j, aY + 1 - j + h * i, 0,
            al_ref_ustr (
              Letter, us, al_ustr_offset (us, i),
              al_ustr_offset (us, i + 1)
            )
          )
        END
      END
      ELSE
        al_draw_text (Ex.MyFont, Color, aX + 1 - j, aY + 1 - j, 0, Text)
    END
  END;



(* Create an example bitmap. *)
  FUNCTION CreateExampleBitmap: ALLEGRO_BITMAPptr;
  VAR
    i, j, x, y, r: INTEGER;
    rc: DOUBLE;
    Locked: ALLEGRO_LOCKED_REGIONptr;
    Data: PBYTE;
  BEGIN
    CreateExampleBitmap := al_create_bitmap (100, 100);
    Locked := al_lock_bitmap (
      CreateExampleBitmap,
      ALLEGRO_PIXEL_FORMAT_ABGR_8888, ALLEGRO_LOCK_WRITEONLY
    );
    Data := Locked^.data;
 
    FOR j := 0 TO 99 DO
    BEGIN
      FOR i := 0 TO 99 DO
      BEGIN
        x := i - 50; y := j - 50;
        r := TRUNC (sqrt (x * x + y * y));
        rc := 1 - r / 50.0;
        IF rc < 0 THEN rc := 0;
        Data[i * 4 + 0] := TRUNC (i * 255 / 100);
        Data[i * 4 + 1] := TRUNC (j * 255 / 100);
        Data[i * 4 + 2] := TRUNC (rc * 255);
        Data[i * 4 + 3] := TRUNC (rc * 255);
      END;
      Data := Data + Locked^.pitch;
    END;
    al_unlock_bitmap (CreateExampleBitmap);
  END;



(* Draw our example scene. *)
  PROCEDURE Draw;

    FUNCTION mIs (a, b: INTEGER): CHAR; INLINE;
    BEGIN
      IF a = b THEN mIs := '*' ELSE mIs := ' '
    END;

  CONST
    BlendNames: ARRAY [0..3] OF STRING = ('ZERO', 'ONE', 'ALPHA', 'INVERSE');
    BlendVNnames: ARRAY [0..3] OF STRING = ('ZERO', 'ONE', 'ALPHA', 'INVER');
    BlendModes: ARRAY [0..3] OF ALLEGRO_BLEND_MODE = (
      ALLEGRO_ZERO, ALLEGRO_ONE, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA
    );
   VAR
    Test: ARRAY [0..4] OF ALLEGRO_COLOR;
    Target: ALLEGRO_BITMAPptr;
    x, y: SINGLE;
    i, j: INTEGER;
   BEGIN
    Target := al_get_target_bitmap;
    x := 40; y := 40;

    al_clear_to_color (al_map_rgb_f (0.5, 0.5, 0.5));

    Test[0] := al_map_rgba_f (1, 1, 1, 1);
    Test[1] := al_map_rgba_f (1, 1, 1, 0.5);
    Test[2] := al_map_rgba_f (1, 1, 1, 0.25);
    Test[3] := al_map_rgba_f (1, 0, 0, 0.75);
    Test[4] := al_map_rgba_f (0, 0, 0, 0);

    print (TRUNC (x), 0, FALSE, Format (
      'D  E  S  T  I  N  A  T  I  O  N  (%0.2f fps)', [Ex.fFPS])
    );
    print (0, TRUNC (y), TRUE, 'S O U R C E');
    FOR i := LOW (BlendNames) TO HIGH (BlendNames) DO
    BEGIN
      print (TRUNC (x + i * 110), 20, FALSE, BlendNames[i]);
      print (20, TRUNC (y + i * 110), TRUE, BlendVNnames[i])
    END;

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
    IF (Ex.Mode >= 1) AND (Ex.Mode <= 5) THEN
    BEGIN
      al_set_target_bitmap (Ex.OffScreen);
      al_clear_to_color(test[Ex.Mode - 1])
    END;
    IF (Ex.Mode >= 6) AND (Ex.Mode <= 10) THEN
    BEGIN
      al_set_target_bitmap (Ex.Memory);
      al_clear_to_color (Test[Ex.Mode - 6])
    END;

    FOR j := LOW (BlendModes) TO HIGH (BlendModes) DO
    BEGIN
      FOR i := LOW (BlendModes) TO HIGH (BlendModes) DO
      BEGIN
        al_set_blender (ALLEGRO_ADD, BlendModes[j], BlendModes[i]);
        IF Ex.Image = 0 THEN
          al_draw_bitmap (Ex.Example, x + i * 110, y + j * 110, 0)
        ELSE IF (Ex.Image >= 1) AND (Ex.Image <= 6) THEN
          al_draw_filled_rectangle (
            x + i * 110,       y + j * 110,
            x + i * 110 + 100, y + j * 110 + 100,
            Test[Ex.Image - 1]
          )
      END
    END;

    IF (Ex.Mode >= 1) AND (Ex.Mode <= 5) THEN
    BEGIN
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_set_target_bitmap (Target);
      al_draw_bitmap_region (Ex.OffScreen, x, y, 430, 430, x, y, 0)
    END;
    IF (Ex.Mode >= 6) AND (Ex.Mode <= 10) THEN
    BEGIN
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_set_target_bitmap (Target);
      al_draw_bitmap_region (Ex.Memory, x, y, 430, 430, x, y, 0)
    END;

    print (Ex.BUTTONS_X, 20 * 1, FALSE, 'What to draw');
    print (Ex.BUTTONS_X, 20 * 2, FALSE, mIs (Ex.Image, 0)+' Picture');
    print (Ex.BUTTONS_X, 20 * 3, FALSE, mIs (Ex.Image, 1)+' Rec1 (1/1/1/1)');
    print (Ex.BUTTONS_X, 20 * 4, FALSE, mIs (Ex.Image, 2)+' Rec2 (1/1/1/.5)');
    print (Ex.BUTTONS_X, 20 * 5, FALSE, mIs (Ex.Image, 3)+' Rec3 (1/1/1/.25)');
    print (Ex.BUTTONS_X, 20 * 6, FALSE, mIs (Ex.Image, 4)+' Rec4 (1/0/0/.75)');
    print (Ex.BUTTONS_X, 20 * 7, FALSE, mIs (Ex.Image, 5)+' Rec5 (0/0/0/0)');

    print (Ex.BUTTONS_X, 20 * 9, FALSE, 'Where to draw');
    print (Ex.BUTTONS_X, 20 * 10, FALSE, mIs (Ex.Mode, 0)+' screen');

    print (Ex.BUTTONS_X, 20 * 11, FALSE, mIs (Ex.Mode, 1)+' offscreen1');
    print (Ex.BUTTONS_X, 20 * 12, FALSE, mIs (Ex.Mode, 2)+' offscreen2');
    print (Ex.BUTTONS_X, 20 * 13, FALSE, mIs (Ex.Mode, 3)+' offscreen3');
    print (Ex.BUTTONS_X, 20 * 14, FALSE, mIs (Ex.Mode, 4)+' offscreen4');
    print (Ex.BUTTONS_X, 20 * 15, FALSE, mIs (Ex.Mode, 5)+' offscreen5');

    print (Ex.BUTTONS_X, 20 * 16, FALSE, mIs (Ex.Mode, 6)+' memory1');
    print (Ex.BUTTONS_X, 20 * 17, FALSE, mIs (Ex.Mode, 7)+' memory2');
    print (Ex.BUTTONS_X, 20 * 18, FALSE, mIs (Ex.Mode, 8)+' memory3');
    print (Ex.BUTTONS_X, 20 * 19, FALSE, mIs (Ex.Mode, 9)+' memory4');
    print (Ex.BUTTONS_X, 20 * 20, FALSE, mIs (Ex.Mode, 10)+' memory5')
  END;



(* Called a fixed amount of times per second. *)
  PROCEDURE Tick;
  VAR
    t: DOUBLE;
  BEGIN
  { Count frames during the last second or so. }
    t := al_get_time ();
    IF t >= Ex.LastSecond + 1 THEN
    BEGIN
      Ex.fFPS := Ex.FramesAccum / (t - Ex.LastSecond);
      Ex.FramesAccum := 0;
      Ex.LastSecond := t
    END;

    draw;
    al_flip_display;
    Ex.FramesAccum := Ex.FramesAccum + 1
  END;



(* Run our test. *)
  PROCEDURE run;
  VAR
    Event: ALLEGRO_EVENT;
    x, y: SINGLE;
    NeedDraw: BOOLEAN;
  BEGIN
    NeedDraw := TRUE;

    REPEAT
    { Perform frame skipping so we don't fall behind the timer events. }
      IF NeedDraw AND al_is_event_queue_empty (Ex.Queue) THEN
      BEGIN
        Tick;
        NeedDraw := FALSE
      END;

      al_wait_for_event (ex.Queue, Event);

      CASE Event._type OF
      { Was the X button on the window pressed? }
      ALLEGRO_EVENT_DISPLAY_CLOSE:
        EXIT;
      { Was a key pressed? }
      ALLEGRO_EVENT_KEY_DOWN:
        IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN EXIT;
      { Is it time for the next timer tick? }
      ALLEGRO_EVENT_TIMER:
        NeedDraw := true;
      { Mouse click? }
      ALLEGRO_EVENT_MOUSE_BUTTON_UP:
        BEGIN
          x := Event.mouse.x;
          y := Event.mouse.y;
          IF x >= Ex.BUTTONS_X THEN
          BEGIN
            CASE TRUNC (y / 20) OF
              2: ex.image := 0;
              3: ex.image := 1;
              4: ex.image := 2;
              5: ex.image := 3;
              6: ex.image := 4;
              7: ex.image := 5;

              10: ex.mode := 0;

              11: ex.mode := 1;
              12: ex.mode := 2;
              13: ex.mode := 3;
              14: ex.mode := 4;
              15: ex.mode := 5;
               
              16: ex.mode := 6;
              17: ex.mode := 7;
              18: ex.mode := 8;
              19: ex.mode := 9;
              20: ex.mode := 10;
            END
          END
        END;
      END
    UNTIL FALSE
  END;



(* Initialize the example. *)
  PROCEDURE Init;
  BEGIN
   Ex.BUTTONS_X := 40 + 110 * 4;
   Ex.FPS := 60;

   Ex.MyFont := al_load_font ('data/font.tga', 0, 0);
   IF Ex.MyFont = NIL THEN
     AbortExample ('data/font.tga not found');
   Ex.Example := CreateExampleBitmap;

   Ex.OffScreen := al_create_bitmap (640, 480);
   al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
   Ex.Memory := al_create_bitmap (640, 480)
  END;



VAR
  Display: ALLEGRO_DISPLAYptr;
  Timer: ALLEGRO_TIMERptr;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

   al_init_primitives_addon;
   al_install_keyboard;
   al_install_mouse;
 {  al_install_touch_input; }
   al_init_image_addon;
   al_init_font_addon;
   InitPlatformSpecific;

   Display := al_create_display (640, 480);
   IF display = NIL THEN  AbortExample ('Error creating display.');

   Init;

   Timer := al_create_timer (1.0 / Ex.FPS);

   Ex.Queue := al_create_event_queue;
   al_register_event_source (Ex.Queue, al_get_keyboard_event_source);
   al_register_event_source (Ex.Queue, al_get_mouse_event_source);
   al_register_event_source (Ex.Queue, al_get_display_event_source (Display));
   al_register_event_source (Ex.Queue, al_get_timer_event_source (Timer));
{
   IF al_is_touch_input_installed THEN
     al_register_event_source (
       Ex.Queue, al_get_touch_input_mouse_emulation_event_source
     );
}

   al_start_timer (Timer);
   Run;

   al_destroy_event_queue (Ex.Queue);
END.
