PROGRAM ex_clip;
(* Test performance of al_draw_bitmap_region, al_create_sub_bitmap and
 * al_set_clipping_rectangle when clipping a bitmap.
 *)
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
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

  uses
    Common,
    Allegro5, al5base, al5font, al5color,
    math;

  type
    TExample = record
      Pattern: ALLEGRO_BITMAPptr;
      Font: ALLEGRO_FONTptr;
      Queue: ALLEGRO_EVENT_QUEUEptr;
      Background, Text, White: ALLEGRO_COLOR;

      Timer, Counter: array [1..4] of Double;
      FPS: Integer;
      TextX, TextY: SINGLE;
    end;

  var
    Ex: TExample;

  function ExampleBitmap (const w, h: Integer): ALLEGRO_BITMAPptr;
  var
    Bmp: ALLEGRO_BITMAPptr;
    i, j: Integer;
    mx, my, a, d, l, hue, sat: SINGLE;
    State: ALLEGRO_STATE;
  begin
    mx := w * 0.5;
    my := h * 0.5;
    Bmp := al_create_bitmap (w, h);
    al_store_state (state, ALLEGRO_STATE_TARGET_BITMAP);
    al_set_target_bitmap (Bmp);
    al_lock_bitmap (Bmp, ALLEGRO_PIXEL_FORMAT_ANY, ALLEGRO_LOCK_WRITEONLY);
    for i := 0 to w - 1 do
    begin
      for j := 0 to h - 1 do
      begin
        a := arctan2 (i - mx, j - my);
        d := Sqrt (Sqr (i - mx) + Sqr (j - my));
        l := 1 - power (1.0 - 1 / (1 + d * 0.1), 5);
        hue := a * 180 / ALLEGRO_PI;
        sat := 1;
        if (i = 0) or (j = 0) or (i = w - 1) or (j = h - 1) then
          hue := hue + 180
        else if (i = 1) or (j = 1) or (i = w - 2) or (j = h - 2) then
        begin
          hue := hue + 180;
          sat := 0.5;
        end;
        al_put_pixel (i, j, al_color_hsl (hue, sat, l))
      end
    end;
    al_unlock_bitmap (Bmp);
    al_restore_state (State);
    ExampleBitmap := Bmp
  end;



  procedure SetXY (const x, y: SINGLE);
  begin
    Ex.TextX := x;
    Ex.TextY := y
  end;



  procedure GetXy (var x, y: SINGLE);
  begin
    x := Ex.TextX;
    y := Ex.TextY
  end;



  procedure Print (const Fmt: AL_STR; const Args: array of const);
  var
    th: Integer;
  begin
    th := al_get_font_line_height (Ex.Font);

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    al_draw_textf (
      Ex.Font, Ex.Text, Ex.TextX, Ex.TextY, 0,
      Fmt, Args
    );

    Ex.TextY := Ex.TextY + th
  end;



  procedure StartTimer (const i: Integer);
  begin
    Ex.Timer[i] := Ex.Timer[i] - al_get_time;
    Ex.Counter[i] := Ex.Counter[i] + 1
  end;



  procedure StopTimer (const i: Integer);
  begin
    Ex.Timer[i] := Ex.Timer[i] + al_get_time
  end;



  function GetFps (const i: Integer): Double;
  begin
    if Ex.Timer[i] = 0 then Exit (0);
    GetFps := Ex.Counter[i] / Ex.Timer[i]
  end;


  procedure Draw;
  var
    x, y: SINGLE;
    iw, ih, cx, cy, cw, ch, gap: LongInt;
    Temp: ALLEGRO_BITMAPptr;
  begin
    x := 0; y := 0;
    iw := al_get_bitmap_width (Ex.Pattern);
    ih := al_get_bitmap_height (Ex.Pattern);
    gap := 8;

    al_get_clipping_rectangle (cx, cy, cw, ch);

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);

    al_clear_to_color (Ex.Background);

  { Test 1. }
    SetXY (8, 8);
    Print ('al_draw_bitmap_region (%1f fps)', [GetFps (1)]);
    GetXy (x, y);
    al_draw_bitmap (Ex.Pattern, x, y, 0);

    StartTimer (1);
    al_draw_bitmap_region (
      Ex.Pattern,
      1, 1, iw - 2, ih - 2,
      x + 8 + iw + 1, y + 1,
      0
    );
    StopTimer (1);
    SetXY (x, y + ih + gap);

  { Test 2. }
    Print ('al_create_sub_bitmap (%.1f fps)', [GetFps (2)]);
    GetXy (x, y);
    al_draw_bitmap (Ex.Pattern, x, y, 0);

    StartTimer (2);
    Temp := al_create_sub_bitmap (Ex.Pattern, 1, 1, iw - 2, ih - 2);
    al_draw_bitmap (Temp, x + 8 + iw + 1, y + 1, 0);
    al_destroy_bitmap (Temp);
    StopTimer (2);
    SetXY (x, y + ih + gap);

  { Test 3. }
    Print ('al_set_clipping_rectangle (%.1f fps)', [GetFps (3)]);
    GetXy (x, y);
    al_draw_bitmap (Ex.Pattern, x, y, 0);

    StartTimer (3);
    al_set_clipping_rectangle
      (Trunc (x + 8 + iw + 1), Trunc (y + 1), iw - 2, ih - 2);
    al_draw_bitmap (Ex.Pattern, x + 8 + iw, y, 0);
    al_set_clipping_rectangle (cx, cy, cw, ch);
    StopTimer (3);
    SetXY (x, y + ih + gap);
  end;



  procedure Tick;
  begin
    Draw;
    al_flip_display
  end;



  procedure Run;
  var
    Event: ALLEGRO_EVENT;
    NeedDraw: Boolean;
  begin
    NeedDraw := True;

    while True do
    begin
      if NeedDraw and al_is_event_queue_empty (Ex.Queue) then
      begin
        tick;
        NeedDraw := False
      end;

      al_wait_for_event (Ex.Queue, @Event);

      case Event.ftype of 
      ALLEGRO_EVENT_DISPLAY_CLOSE:
        Exit;
      ALLEGRO_EVENT_KEY_DOWN:
        if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then Exit;
      ALLEGRO_EVENT_TIMER:
        NeedDraw := True;
      end
    end
  end;



  procedure Init;
  begin
    Ex.FPS := 60;

    Ex.font := al_create_builtin_font;
    if Ex.Font = Nil then AbortExample ('Error creating builtin font.');
    Ex.Background := al_color_name ('beige');
    Ex.Text := al_color_name ('blue');
    Ex.White := al_color_name ('white');
    Ex.Pattern := ExampleBitmap (100, 100);
  end;



  var
    Display: ALLEGRO_DISPLAYptr;
    Timer: ALLEGRO_TIMERptr;

begin

  if not al_init then AbortExample ('Could not init Allegro.');

  al_install_keyboard;
  al_install_mouse;
  al_init_font_addon;
  InitPlatformSpecific;

  Display := al_create_display (640, 480);
  if Display = Nil then AbortExample ('Error creating display');

  Init;
{ Ignore message:
ex_clip.pas(276,35) Warning: Variable "Ex" does not seem to be initialized
  It is initialized at "Init". }
  Timer := al_create_timer (1.0 / Ex.FPS);

  Ex.Queue := al_create_event_queue;
  al_register_event_source (Ex.Queue, al_get_keyboard_event_source);
  al_register_event_source (Ex.Queue, al_get_mouse_event_source);
  al_register_event_source (Ex.Queue, al_get_display_event_source (Display));
  al_register_event_source (Ex.Queue, al_get_timer_event_source (Timer));

  al_start_timer (Timer);
  run;

  al_destroy_event_queue (Ex.Queue)
end.
