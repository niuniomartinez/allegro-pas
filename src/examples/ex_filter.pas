program ex_filter;
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
    Allegro5, al5base, al5font, al5image, Common;

  const
    FPS = 60;
    FilterFlags: array [0..5] of Integer = (
      0,
      ALLEGRO_MIN_LINEAR,
      ALLEGRO_MIPMAP,
      ALLEGRO_MIN_LINEAR or ALLEGRO_MIPMAP,
      0,
      ALLEGRO_MAG_LINEAR
    );

    FilterText: array [0..3] of AL_STR = (
      'nearest', 'linear',
      'nearest mipmap', 'linear mipmap'
    );

  type
    TExample = record
      Display: ALLEGRO_DISPLAYptr;
      Font: ALLEGRO_FONTptr;
      Bitmaps: array [0..1] of array [0..8] of ALLEGRO_BITMAPptr;
      bg, fg, Info: ALLEGRO_COLOR;
      Bitmap, Ticks: Integer;
    end;

  var
    Example: TExample;



  procedure Update;
  begin
    Inc (Example.Ticks)
  end;



  procedure Redraw;
  var
    w, h, i: Integer;
    x, y, bw, bh, t, Scale, Angle: Real;
    Bmp: ALLEGRO_BITMAPptr;
  begin
    w := al_get_display_width (Example.Display);
    h := al_get_display_height (Example.Display);

    al_clear_to_color (Example.bg);

    for i := 0 to 5 do
    begin
      x := (i div 2) * w / 3 + w / 6;
      y := (i mod 2) * h / 2 + h / 4;
      Bmp := Example.Bitmaps[Example.Bitmap][i];
      bw := al_get_bitmap_width (Bmp);
      bh := al_get_bitmap_height (Bmp);
      t := 1 - 2 * Abs ((Example.Ticks mod (FPS * 16)) / 16 / FPS - 0.5);
      Angle := Example.Ticks * ALLEGRO_PI * 2 / FPS / 8;

      if i < 4 then
         Scale := 1 - t * 0.9
      else
         scale := 1 + t * 9;

      al_draw_text (
        Example.Font, Example.fg, x, y - 64 - 14,
        ALLEGRO_ALIGN_CENTRE, FilterText[i mod 4]
      );

      al_set_clipping_rectangle (Trunc (x - 64), Trunc (y - 64), 128, 128);
      al_draw_scaled_rotated_bitmap (
        Bmp, bw / 2, bh / 2,
        x, y, scale, scale, angle, 0
      );
      al_set_clipping_rectangle (0, 0, w, h);
    end;
    al_draw_text (
      Example.Font, Example.Info, w / 2, h - 14,
      ALLEGRO_ALIGN_CENTRE, 'press space to change')
  end;


  const
    w = 640; h = 480;
  var
    Timer: ALLEGRO_TIMERptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Done, NeedRedraw: Boolean;
    i, x, y, c: Integer;
    Mysha: ALLEGRO_BITMAPptr;
    Lock: ALLEGRO_LOCKED_REGIONptr;
    Row, Ptr: PBYTE;

begin
  done := False;
  NeedRedraw := True;

  if not al_init then
    AbortExample ('Failed to init Allegro.');

  if not al_init_image_addon then
    AbortExample ('Failed to init IIO addon.');

  al_init_font_addon;

  InitPlatformSpecific;

  Example.Display := al_create_display (w, h);
  if Example.Display = Nil then
    AbortExample ('Error creating display.');

  if not al_install_keyboard then
    AbortExample ('Error installing keyboard.');

  if not al_install_mouse then
    AbortExample ('Error installing mouse.');

  Example.Font := al_load_font ('data/fixed_font.tga', 0, 0);
  if Example.Font = Nil then
    AbortExample ('Error loading data/fixed_font.tga');

  Mysha := al_load_bitmap ('data/mysha256x256.png');
  if Mysha = Nil then
    AbortExample ('Error loading data/mysha256x256.png');

  for i := Low (FilterFlags) to High (FilterFlags) do
  begin
  { Only power-of-two bitmaps can have mipmaps. }
    al_set_new_bitmap_flags (FilterFlags[i]);
    Example.Bitmaps[0][i] := al_create_bitmap (1024, 1024);
    Example.Bitmaps[1][i] := al_clone_bitmap (Mysha);
    Lock := al_lock_bitmap (
      Example.Bitmaps[0][i],
      ALLEGRO_PIXEL_FORMAT_ABGR_8888_LE, ALLEGRO_LOCK_WRITEONLY
    );
    for y := 0 to 1023 do
    begin
      Row := PBYTE (Lock^.data) + Lock^.pitch * y;
      Ptr := Row;
      for X := 0 to 1023 do
      begin
        c := 0;
        if (((x shr 2) and 1) XOR ((y shr 2) and 1)) <> 0 then c := 255;
        Ptr^ := c; Inc (ptr);
        Ptr^ := c; Inc (ptr);
        Ptr^ := c; Inc (ptr);
        Ptr^ := 255; Inc (ptr)
      end
    end;
    al_unlock_bitmap (Example.Bitmaps[0][i])
  end;

  Example.bg := al_map_rgb_f (0, 0, 0);
  Example.fg := al_map_rgb_f (1, 1, 1);
  Example.info := al_map_rgb_f (0.5, 0.5, 1);

  Timer := al_create_timer (1 / FPS);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_mouse_event_source);
  al_register_event_source (Queue, al_get_timer_event_source (Timer));
  al_register_event_source (Queue, al_get_display_event_source (Example.Display));

  al_start_timer (Timer);

  repeat
    if NeedRedraw and al_is_event_queue_empty (Queue) then
    begin
      Redraw;
      al_flip_display;
      NeedRedraw := False;
    end;

    al_wait_for_event (Queue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_KEY_DOWN:
      begin
        if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then Done := True;
        if Event.keyboard.keycode = ALLEGRO_KEY_SPACE then
          Example.Bitmap := (Example.Bitmap + 1) mod 2
      end;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Done := True;
    ALLEGRO_EVENT_TIMER:
      begin
        Update;
        NeedRedraw := True
      end;
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
      Example.Bitmap := (Example.Bitmap + 1) mod 2
    end
  until Done;

  for i := 0 to 5 do
  begin
    al_destroy_bitmap (Example.Bitmaps[0][i]);
    al_destroy_bitmap (Example.Bitmaps[1][i])
  end
end.
