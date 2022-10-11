program ex_projection;
(*
  Copyright (c) 2012-2020 Guillermo Martínez J.

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
  {$RANGECHECKS OFF} { If set, example doesn't run. }
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

  uses
    Common,
    Allegro5, al5base, al5font, al5image, al5ttf;

  var
  (* How far has the text been scrolled. *)
    ScrollY: Single;
  (* Total length of the scrolling text in pixels. *)
    TextLength: Integer;
  (* The Alex logo. *)
    Logo: ALLEGRO_BITMAPptr;
  (* The star particle. *)
    Particle: ALLEGRO_BITMAPptr;
  (* The star list. *)
    Stars: array [1..100] of record x, y, z: LongInt end;
  (* The font we use for everything. *)
    Font: ALLEGRO_FONTptr;



  (* Set stars positions. *)
    procedure InitStars;
    var
      Cnt: Integer;
    begin
      for Cnt := Low (Stars) to High (Stars) do
      begin
        Stars[Cnt].x := Random ($FFFF);
        Stars[Cnt].y := Random ($FFFF);
        Stars[Cnt].z := Random ($FFFF)
      end
    end;



  (* Load a bitmap and exit with a message if it's missing. *)
    function LoadBmp (const Path: AL_STR): ALLEGRO_BITMAPptr;
    var
      lBmp: ALLEGRO_BITMAPptr;
    begin
      lBmp := al_load_bitmap (Path);
      if lBmp = Nil then AbortExample ('Could not load '+Path);
      Exit (lBmp)
    end;



  (* Load a font and exit with a message if it's missing. *)
    function LoadFont (const Path: AL_STR; Size, Flags: Integer): ALLEGRO_FONTptr;
    var
      lFont: ALLEGRO_FONTptr;
    begin
      lFont := al_load_font (Path, Size, Flags);
      if lFont = Nil then AbortExample ('Could not load '+Path);
      Exit (lFont)
    end;



  (* Print fading text. *)
    function Print
      (Font: ALLEGRO_FONTptr; x, y, r, g, b, Fade: Single; const Text: AL_STR): Integer;
    var
      c: Single;
    begin
      c := 1 + (y - Fade) / 360 / 2.0;
      if c > 1 then c := 1 else if c < 0 then c := 0;
      al_draw_text (
        Font, al_map_rgba_f (c * r, c * g, c * b, c),
        x, y, ALLEGRO_ALIGN_CENTER, Text
      );
      Exit (Trunc (y + al_get_font_line_height (Font)))
    end;



(* Set up a perspective transform. We make the screen span
   180 vertical units with square pixel aspect and 90° vertical
   FoV. *)
  procedure Setup3DProjection (var Projection: ALLEGRO_TRANSFORM);
  var
    Display: ALLEGRO_DISPLAYptr;
    dw, dh: Integer;
  begin
    Display := al_get_current_display;
    dw := al_get_display_width (Display);
    dh := al_get_display_height (Display);
    al_perspective_transform (
      Projection, -180 * dw / dh, -180, 180,
      180 * dw / dh, 180, 3000
    );
    al_use_projection_transform (Projection)
  end;


(* 3D transformations make it very easy to draw a starfield. *)
  procedure DrawStars;
  var
    Projection: ALLEGRO_TRANSFORM;
    Cnt: Integer;
  begin
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ONE);
    for Cnt := 1 to 100 do
    begin
      al_identity_transform (Projection);
      al_translate_transform_3d (
        Projection, 0, 0,
        -2000 + Trunc (ScrollY * 1000 / TextLength + Stars[Cnt].z) mod 2000 - 180);
      Setup3DProjection (Projection);
      al_draw_bitmap (Particle, Stars[Cnt].x mod 4000 - 2000, Stars[Cnt].y mod 2000 - 1000, 0)
    end
  end;



(* The main part of this example. *)
  procedure DrawScrollingText;
  var
    Projection: ALLEGRO_TRANSFORM;
    bw, bh: Integer;
    x, y, c: Single;

    procedure T (str: AL_STR);
    begin
      y := Print (Font, x, y, 1, 0.9, 0.3, ScrollY, str)
    end;

  begin
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
    if c < 0 then c := 0;
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
  end;



  procedure DrawIntroText;
  var
    Projection: ALLEGRO_TRANSFORM;
    Fade, fh: Integer;
  begin
    fh := al_get_font_line_height (Font);

    if ScrollY < 50 then
      Fade := Trunc ((50 - ScrollY) * 12)
    else
      Fade := Trunc ((ScrollY - 50) * 4);

    al_identity_transform (Projection);
    al_translate_transform_3d (Projection, 0, -ScrollY / 3, -181);
    Setup3DProjection (Projection);

    Print (Font, 0, 0, 0, 0.9, 1, Fade, 'A long time ago, in a galaxy');
    Print (Font, 0, 0 + fh, 0, 0.9, 1, Fade, 'not too far away...')
  end;



var
  Display: ALLEGRO_DISPLAYptr;
  Black: ALLEGRO_COLOR;
  Timer: ALLEGRO_TIMERptr;
  Queue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Redraw, EndExample: Boolean;
begin

  if not al_init then AbortExample ('Could not init Allegro.');
  al_init_image_addon;
  al_init_font_addon;
  al_init_ttf_addon;
  InitPlatformSpecific;
  al_install_keyboard;

  al_set_new_display_flags (ALLEGRO_RESIZABLE);
  Display := al_create_display (640, 360);
  if Display = Nil then AbortExample ('Error creating display.');

  al_set_new_bitmap_flags
    (ALLEGRO_MIN_LINEAR or ALLEGRO_MAG_LINEAR or ALLEGRO_MIPMAP);

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

  InitStars;

  al_start_timer (Timer);
  EndExample := False;
  Redraw := False;
  ScrollY := 0;
  Black := al_map_rgba_f (0, 0, 0, 1);
  repeat
    al_wait_for_event (Queue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EndExample := True;
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      al_acknowledge_resize (Display);
    ALLEGRO_EVENT_KEY_DOWN:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then EndExample := True;
    ALLEGRO_EVENT_TIMER:
      begin
        ScrollY := ScrollY + 1;
        if ScrollY > TextLength * 2 then ScrollY := ScrollY - TextLength * 2;
        Redraw := True
      end;
    end;

    if Redraw and al_is_event_queue_empty (Queue) then
    begin
      al_clear_to_color (Black);
      DrawStars;
      DrawScrollingText;
      DrawIntroText;
      al_flip_display;
      Redraw := False
    end
  until EndExample
end.
