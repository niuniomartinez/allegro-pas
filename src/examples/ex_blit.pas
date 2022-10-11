program ex_blit;
(*
 * An example demonstrating different blending modes.
 *
 * Lazarus forum user x2nie proposed the fix for Text 4.
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
  {$POINTERMATH ON}
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

uses
  Common,
  Allegro5, al5base, al5color, al5font, al5image, al5strings,
  math;

const
  FPS = 60;

var
  Pattern: ALLEGRO_BITMAPptr;
  Font: ALLEGRO_FONTptr;
  EventQueue: ALLEGRO_EVENT_QUEUEptr;
  Background, TextClr, Black, Red: ALLEGRO_COLOR;
  Timer, Counter: array [1..4] of Double;
  TextX, TextY: Single;



  function ExampleBitmap (const w, h: Integer): ALLEGRO_BITMAPptr;
  var
    i, j: Integer;
    mx, my, a, d, sat, hue: Single;
    State: ALLEGRO_STATE;
    Lock: ALLEGRO_LOCKED_REGIONptr;
    Pattern: ALLEGRO_BITMAPptr;
  begin
    mx := w * 0.5;
    my := h * 0.5;
    Pattern := al_create_bitmap (w, h);
    al_store_state (State, ALLEGRO_STATE_TARGET_BITMAP);
    al_set_target_bitmap (Pattern);
{ Ignore message:
ex_blit.pas(67,5) Note: Local variable "Lock" is assigned but never used
  It is initialized at "Init". }
    Lock := al_lock_bitmap (Pattern, ALLEGRO_PIXEL_FORMAT_ANY, ALLEGRO_LOCK_WRITEONLY);
    for i := 0 to w - 1 do
    begin
      for j := 0 to h - 1 do
      begin
	a := arctan2 (i - mx, j - my);
	d := sqrt (power (i - mx, 2) + power (j - my, 2));
	sat := power (1 - 1 / (1 + d * 0.1), 5);
	hue := 3 * a * 180 / ALLEGRO_PI;
	hue := (hue / 360 - floor (hue / 360)) * 360;
	al_put_pixel (i, j, al_color_hsv (hue, sat, 1))
      end
    end;
    al_put_pixel (0, 0, Black);
    al_unlock_bitmap (Pattern);
    al_restore_state (State);
    ExampleBitmap := Pattern
  end;



  procedure SetXY (const x, y: Single);
  begin
    TextX := x; TextY := y
  end;



  procedure GetXY (out x, y: Single);
  begin
    x := TextX; y := TextY
  end;



  procedure Print (const Fmt: AL_STR; const Args: array of const);
  var
    th: Integer;
  begin
    th := al_get_font_line_height (Font);
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    al_draw_text (Font, TextClr, TextX, TextY, 0, al_str_format (Fmt, Args));
    TextY := TextY + th
  end;



  procedure StartTimer (Ndx: Integer);
  begin
    Timer[Ndx] := Timer[Ndx] - al_get_time;
    Counter[Ndx] := Counter[Ndx] + 1
  end;



  procedure StopTimer (Ndx: Integer);
  begin
    Timer[Ndx] := Timer[Ndx] + al_get_time
  end;



  function GetFPS  (Ndx: Integer): Single;
  begin
    if Timer[Ndx] = 0 then Exit (0.0);
    GetFPS := Counter[Ndx] / Timer[Ndx]
  end;



  procedure Draw;
  type
    PByteArray = ^TByteArray;
    TByteArray = array [WORD] of BYTE;
  var
    x, y: Single;
    i, ix, iy, iw, ih, FormatSize: Integer;
    Screen, Temp: ALLEGRO_BITMAPptr;
    FormatLock: ALLEGRO_PIXEL_FORMAT;
    Lock: ALLEGRO_LOCKED_REGIONptr;
    BitmapData, ScreenData: PByteArray;
  begin
    iw := al_get_bitmap_width (Pattern);
    ih := al_get_bitmap_height (Pattern);
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
    al_clear_to_color (Background);
    Screen := al_get_target_bitmap;

    SetXY (8, 8);

  { Test 1. }
  { /* Disabled: drawing to same bitmap is not supported. }
  (*
    Print ('Screen -> Screen (%.1f fps)', get_fps(0));
    get_xy(&x, &y);
    al_draw_bitmap(ex.Pattern, x, y, 0);

    start_timer(0);
    al_draw_bitmap_region(screen, x, y, iw, ih, x + 8 + iw, y, 0);
    stop_timer(0);
    SetXY (x, y + ih);
   *)

  { Test 2. }
    Print ('Screen -> Bitmap -> Screen (%.1f fps)', [GetFPS (1)]);
    GetXY (x, y);
    al_draw_bitmap (Pattern, x, y, 0);

    Temp := al_create_bitmap (iw, ih);
    al_set_target_bitmap (Temp);
    al_clear_to_color (Red);
    StartTimer (1);
    al_draw_bitmap_region (Screen, x, y, iw, ih, 0, 0, 0);

    al_set_target_bitmap (Screen);
    al_draw_bitmap (Temp, x + 8 + iw, y, 0);
    StopTimer (1);
    SetXY (x, y + ih);

    al_destroy_bitmap (Temp);

  { Test 3. }
    Print ('Screen -> Memory -> Screen (%.1f fps)', [GetFPS (2)]);
    GetXY (x, y);
    al_draw_bitmap (Pattern, x, y, 0);

    al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
    Temp := al_create_bitmap (iw, ih);
    al_set_target_bitmap (Temp);
    al_clear_to_color (Red);
    StartTimer (2);
    al_draw_bitmap_region (Screen, x, y, iw, ih, 0, 0, 0);

    al_set_target_bitmap (Screen);
    al_draw_bitmap (Temp, x + 8 + iw, y, 0);
    StopTimer (2);
    SetXY (x, y + ih);

    al_destroy_bitmap (Temp);
    al_set_new_bitmap_flags (ALLEGRO_VIDEO_BITMAP);


  { Test 4. }
    Print ('Screen -> Locked -> Screen (%.1f fps)', [GetFPS(3)]);
    GetXY (x, y);
    al_draw_bitmap (Pattern, x, y, 0);

    StartTimer (3);

    ix := Round (x); iy := Round (y);
    Lock := al_lock_bitmap_region (
      Screen,
      ix, iy, iw, ih,
      ALLEGRO_PIXEL_FORMAT_ANY, ALLEGRO_LOCK_READONLY
    );
    FormatLock := ALLEGRO_PIXEL_FORMAT (Lock.format);
    FormatSize := Lock.pixel_size;
    BitmapData := AllocMem (FormatSize * iw * ih);
    for i := 0 to ih - 1 do
    begin
    { Lock.pitch may be negative (i.e. graphics card stores textures upside down)
      so indexes can't be used here as only positive values can be used.
      So use pointers.
    }
      ScreenData := Lock.Data + (i * Lock.pitch);
      move (
	ScreenData[0],
	BitmapData[i * FormatSize * iw],
	FormatSize * iw
      )
    end;
    al_unlock_bitmap (Screen);

    Lock := al_lock_bitmap_region (
      Screen,
      ix + 8 + iw, iy, iw, ih,
      FormatLock, ALLEGRO_LOCK_WRITEONLY
    );
    for i := 0 to ih - 1 do
    begin
      ScreenData := Lock.Data + (i * Lock.pitch);
      move (
	BitmapData[i * FormatSize * iw],
	ScreenData[0],
	FormatSize * iw
      )
    end;
    al_unlock_bitmap (Screen);
    FreeMem (BitmapData);

    StopTimer (3);
    SetXY (x, y + ih)
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

    repeat
      if NeedDraw and al_is_event_queue_empty (EventQueue) then
      begin
        Tick;
        NeedDraw := False
      end;
      al_wait_for_event (EventQueue, @Event);
      case Event.ftype of
      ALLEGRO_EVENT_DISPLAY_CLOSE:
        Exit;
      ALLEGRO_EVENT_KEY_DOWN:
        if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then Exit;
      ALLEGRO_EVENT_TIMER:
        NeedDraw := True;
      end
    until False
  end;



  procedure Init;
  begin
    Font := al_load_font ('data/fixed_font.tga', 0, 0);
    if Font = Nil then
      AbortExample ('data/fixed_font.tga not found');
    Background := al_color_name ('beige');
    TextClr := al_color_name ('black');
    Black := al_color_name ('black');
    Red := al_map_rgba_f (1, 0, 0, 1);
    Pattern := ExampleBitmap (100, 100)
  end;



var
  Display: ALLEGRO_DISPLAYptr;
  TheTimer: ALLEGRO_TIMERptr;
begin
  if not al_init then AbortExample ('Could not init Allegro.');

  al_install_keyboard;
  al_init_image_addon;
  al_init_font_addon;

  Display := al_create_display (640, 480);
  if Display = Nil then AbortExample ('Could not create display');

  Init;

  TheTimer := al_create_timer (1 / FPS);

  EventQueue := al_create_event_queue;
  al_register_event_source (EventQueue, al_get_keyboard_event_source);
  al_register_event_source (EventQueue, al_get_display_event_source (Display));
  al_register_event_source (EventQueue, al_get_timer_event_source (TheTimer));

  al_start_timer (TheTimer);
  Run;

  al_destroy_event_queue (EventQueue)
end.
