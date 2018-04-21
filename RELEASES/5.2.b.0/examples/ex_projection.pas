PROGRAM ex_projection;
(*
  Copyright (c) 2012-2018 Guillermo Martínez J.

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
 *)

{$IFDEF FPC}
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

  USES
    Common,
    Allegro5, al5font, al5image, al5ttf;

  VAR
  (* How far has the text been scrolled. *)
    ScrollY: SINGLE;
  (* Total length of the scrolling text in pixels. *)
    TextLength: INTEGER;
  (* The Alex logo. *)
    Logo: ALLEGRO_BITMAPptr;
  (* The star particle. *)
    Particle: ALLEGRO_BITMAPptr;
  (* The font we use for everything. *)
    Font: ALLEGRO_FONTptr;



  (* Local pseudo-random number generator. *)
    FUNCTION MyRnd (VAR Seed: LONGINT): INTEGER;
    BEGIN
      Seed := (Seed + 1) * 1103515245 + 12345;
      EXIT ((Seed SHR 16) AND $ffff)
    END;



  (* Load a bitmap and exit with a message if it's missing. *)
    FUNCTION LoadBmp (CONST Path: STRING): ALLEGRO_BITMAPptr;
    VAR
      lBmp: ALLEGRO_BITMAPptr;
    BEGIN
      lBmp := al_load_bitmap (Path);
      IF lBmp = NIL THEN AbortExample ('Could not load '+Path) ELSE EXIT (lBmp)
    END;



  (* Load a font and exit with a message if it's missing. *)
    FUNCTION LoadFont (CONST Path: STRING; Size, Flags: INTEGER): ALLEGRO_FONTptr;
    VAR
      lFont: ALLEGRO_FONTptr;
    BEGIN
      lFont := al_load_font (Path, Size, Flags);
      IF lFont = NIL THEN
	AbortExample ('Could not load '+Path)
      ELSE
	EXIT (lFont)
    END;



  (* Print fading text. *)
    FUNCTION Print
      (Font: ALLEGRO_FONTptr; x, y, r, g, b, Fade: SINGLE; CONST Text: STRING): INTEGER;
    VAR
      c: SINGLE;
    BEGIN
      c := 1 + (y - Fade) / 360 / 2.0;
      IF c > 1 THEN c := 1 ELSE IF c < 0 THEN c := 0;
      al_draw_text (
        Font, al_map_rgba_f (c * r, c * g, c * b, c),
        x, y, ALLEGRO_ALIGN_CENTER, Text
      );
      EXIT (TRUNC (y + al_get_font_line_height (Font)))
    END;



(* Set up a perspective transform. We make the screen span
   180 vertical units with square pixel aspect and 90° vertical
   FoV. *)
  PROCEDURE Setup3DProjection (VAR Projection: ALLEGRO_TRANSFORM);
  VAR
    Display: ALLEGRO_DISPLAYptr;
    dw, dh: INTEGER;
  BEGIN
    Display := al_get_current_display;
    dw := al_get_display_width (Display);
    dh := al_get_display_height (Display);
    al_perspective_transform (
      Projection, -180 * dw / dh, -180, 180,
      180 * dw / dh, 180, 3000
    );
    al_use_projection_transform (Projection)
  END;


(* 3D transformations make it very easy to draw a starfield. *)
  PROCEDURE DrawStars;
  VAR
    Projection: ALLEGRO_TRANSFORM;
    Seed, Cnt, X, Y, Z: LONGINT;
  BEGIN
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ONE);

    Seed := 0;

    FOR Cnt := 1 TO 100 DO
    BEGIN
      X := MyRnd (Seed);
      Y := MyRnd (Seed);
      Z := MyRnd (Seed);

      al_identity_transform (Projection);
      al_translate_transform_3d (
        Projection, 0, 0,
        -2000 + TRUNC (ScrollY * 1000 / TextLength + z) MOD 2000 - 180);
      Setup3DProjection (Projection);
      al_draw_bitmap (Particle, X MOD 4000 - 2000, y MOD 2000 - 1000, 0)
    END
  END;



(* The main part of this example. *)
  PROCEDURE DrawScrollingText;
  VAR
    Projection: ALLEGRO_TRANSFORM;
    bw, bh: INTEGER;
    x, y, c: SINGLE;

    PROCEDURE T (str: STRING);
    BEGIN
      y := Print (Font, x, y, 1, 0.9, 0.3, ScrollY, str)
    END;

  BEGIN
    bw := al_get_bitmap_width  (Logo);
    bh := al_get_bitmap_height (Logo);

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);

    al_identity_transform (Projection);

  { First, we scroll the text in in the y direction (inside the x/z
     plane) and move it away from the camera (in the z direction).
     We move it as far as half the display height to get a vertical
     FOV of 90 degrees. }
    al_translate_transform_3d (projection, 0, -ScrollY, -180);

  { Then we tilt it backwards 30 degrees. }
    al_rotate_transform_3d (Projection, 1, 0, 0, 30 * ALLEGRO_PI / 180.0);

  { And finally move it down so the 0 position ends up at the bottom of the
    screen. }
    al_translate_transform_3d (Projection, 0, 180, 0);

    Setup3DProjection (Projection);

    x := 0;
    y := 0;
    c := 1 + (y - ScrollY) / 360 / 2.0;
    IF c < 0 THEN c := 0;
    al_draw_tinted_bitmap (Logo, al_map_rgba_f (c, c, c, c), x - bw / 2, y, 0);
    y := y + bh;


    T ('Allegro 5');
    T ('');
    T ('It is a period of game programming.');
    T ('Game coders have won their first');
    T ('victory against the evil');
    T ('General Protection Fault.');
    T ('');
    T ('During the battle, hackers managed');
    T ('to steal the secret source to the');
    T ('General''s ultimate weapon,');
    T ('the ACCESS VIOLATION, a kernel');
    T ('exception with enough power to');
    T ('destroy an entire program.');
    T ('');
    T ('Pursued by sinister bugs the');
    T ('Allegro developers race home');
    T ('aboard their library to save');
    T ('all game programmers and restore');
    T ('freedom to the open source world...')
  END;



  PROCEDURE DrawIntroText;
  VAR
    Projection: ALLEGRO_TRANSFORM;
    Fade, fh: INTEGER;
  BEGIN
    fh := al_get_font_line_height (Font);

    IF ScrollY < 50 THEN
      Fade := TRUNC ((50 - ScrollY) * 12)
    ELSE
      Fade := TRUNC ((ScrollY - 50) * 4);
   
    al_identity_transform (Projection);
    al_translate_transform_3d (Projection, 0, -ScrollY / 3, -181);
    Setup3DProjection (Projection);

    Print (Font, 0, 0, 0, 0.9, 1, Fade, 'A long time ago, in a galaxy');
    Print (Font, 0, 0 + fh, 0, 0.9, 1, Fade, 'not too far away...')
  END;



VAR
  Display: ALLEGRO_DISPLAYptr;
  Black: ALLEGRO_COLOR;
  Timer: ALLEGRO_TIMERptr;
  Queue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Redraw, EndExample: BOOLEAN;
BEGIN

  IF NOT al_init THEN AbortExample ('Could not init Allegro.');
  al_init_image_addon;
  al_init_font_addon;
  al_init_ttf_addon;
  InitPlatformSpecific;
  al_install_keyboard;

  al_set_new_display_flags (ALLEGRO_RESIZABLE);
  Display := al_create_display (640, 360);
  IF Display = NIL THEN AbortExample ('Error creating display.');

  al_set_new_bitmap_flags
    (ALLEGRO_MIN_LINEAR OR ALLEGRO_MAG_LINEAR OR ALLEGRO_MIPMAP);

  Font := LoadFont ('data/DejaVuSans.ttf', 40, 0);
  Logo := LoadBmp ('data/alexlogo.png');
  Particle := LoadBmp ('data/haiku/air_effect.png');

  al_convert_mask_to_alpha (Logo, al_map_rgb (255, 0, 255));

  TextLength :=
    al_get_bitmap_height (Logo) + 19 * al_get_font_line_height (Font);

  Timer := al_create_timer (1.0 / 60);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_display_event_source (Display));
  al_register_event_source (Queue, al_get_timer_event_source (Timer));

  al_start_timer (Timer);
  EndExample := FALSE;
  Redraw := FALSE;
  ScrollY := 0;
  Black := al_map_rgba_f (0, 0, 0, 1);
  REPEAT
    al_wait_for_event (Queue, Event);
    CASE Event._type OF
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EndExample := TRUE;
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      al_acknowledge_resize (Display);
    ALLEGRO_EVENT_KEY_DOWN:
      IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN EndExample := TRUE;
    ALLEGRO_EVENT_TIMER:
      BEGIN
        ScrollY := ScrollY + 1;
        IF ScrollY > TextLength * 2 THEN ScrollY := ScrollY - TextLength * 2;
        Redraw := TRUE
      END;
    END;

    IF Redraw AND al_is_event_queue_empty (Queue) THEN
    BEGIN
      al_clear_to_color (Black);
      DrawStars;
      DrawScrollingText;
      DrawIntroText;
      al_flip_display;
      Redraw := FALSE
    END
  UNTIL EndExample
END.
