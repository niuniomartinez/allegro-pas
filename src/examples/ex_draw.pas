program ex_draw;
(* Tests some drawing primitives. *)
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
    Allegro5, al5base, al5image, al5font, al5color, al5primitives;

  type
    RExample = record
      Font: ALLEGRO_FONTptr;
      Queue: ALLEGRO_EVENT_QUEUEptr;
      Background, Text, White, Foreground: ALLEGRO_COLOR;
      Outline: ALLEGRO_COLOR;
      Pattern: ALLEGRO_BITMAPptr;
      Zoom: ALLEGRO_BITMAPptr;

      Timer, Counter: array [0..3] of Double;
      FPS: Integer;
      TextX, TextY: Real;

      Software: Boolean;
      Samples: Integer;
      What: Integer;
      Thickness: Integer;
    end;

  var
   Ex: RExample;

  const
    Names: array [0..4] of AL_STR = (
      'filled rectangles',
      'rectangles',
      'filled circles',
      'circles',
      'lines'
    );

  procedure DrawPattern (Bmp: ALLEGRO_BITMAPptr);
  var
    w, h, x, y: Integer;
    Format: ALLEGRO_PIXEL_FORMAT;
    Light, Dark, c: ALLEGRO_COLOR;
    Lock: ALLEGRO_LOCKED_REGIONptr;
    r, g, b: BYTE;
    Data: PBYTE;
  begin
    w := al_get_bitmap_width (Bmp);
    h := al_get_bitmap_height (Bmp);
    Format := ALLEGRO_PIXEL_FORMAT_BGR_888;
    Light := al_map_rgb_f (1, 1, 1);
    Dark := al_map_rgb_f (1, 0.9, 0.8);
    Lock := al_lock_bitmap (Bmp, format, ALLEGRO_LOCK_WRITEONLY);
    for Y := 0 to (h - 1) do
    begin
      for X := 0 to (w - 1) do
      begin
        if ((x + y) and 1) <> 0 then c := Light else c := Dark;
        Data := Lock^.data;
        al_unmap_rgb (c, r, g, b);
        Data := Data + (y * Lock^.pitch);
        Data := Data + (x * 3);
        Data[0] := r;
        Data[1] := g;
        Data[2] := b
      end
    end;
    al_unlock_bitmap (Bmp)
  end;



  procedure SetXY (x, y: Real);
  begin
    Ex.TextX := x;
    Ex.TextY := y
  end;



  procedure Printf (const aFmt: AL_STR; const aVals: array of const);
  var
    State: ALLEGRO_STATE;
    th: Integer;
  begin
    th := al_get_font_line_height (Ex.Font);
    al_store_state(&state, ALLEGRO_STATE_BLENDER);
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    al_draw_textf (Ex.Font, Ex.Text, Ex.TextX, Ex.TextY, 0, aFmt, aVals);
    al_restore_state (state);
    Ex.TextY := Ex.TextY + th
  end;

  procedure Print (const aText: AL_STR); inline;
  begin
    Printf (aText, [])
  end;



  procedure Primitive (l, t, r, b: Real; Clr: ALLEGRO_COLOR; NoFill: Boolean);
  var
    cx, cy, rx, ry: Real;
    tk, w: Integer;
  begin
    cx := (l + r) / 2;
    cy := (t + b) / 2;
    rx := (r - l) / 2;
    ry := (b - t) / 2;
    if NoFill then tk := 0 else tk := Ex.Thickness;
    w := Ex.What;
    if NoFill then
    begin
      if w = 0 then w := 1 else if w = 2 then w := 3
    end;
    case w of
      0: al_draw_filled_rectangle (l, t, r, b, Clr);
      1: al_draw_rectangle (l, t, r, b, Clr, tk);
      2: al_draw_filled_ellipse (cx, cy, rx, ry, Clr);
      3: al_draw_ellipse (cx, cy, rx, ry, Clr, tk);
      4: al_draw_line (l, t, r, b, Clr, tk);
    end
  end;



  procedure Draw;
  var
    x, y: Real;
    cx, cy, cw, ch, w, h, RectsNum, i, j: LongInt;
    Screen, mem: ALLEGRO_BITMAPptr;
    Rects: array [0..(16 * 4) - 1] of Real;
    rgba: ALLEGRO_COLOR;
    sw: AL_STR;
  begin
    w := al_get_bitmap_width (Ex.Zoom);
    h := al_get_bitmap_height (Ex.Zoom);
    Screen := al_get_target_bitmap;
    RectsNum := 16;
    for j := 0 to 3 do
      for i := 0 to 3 do
      begin
        Rects[(j * 4 + i) * 4 + 0] := 2 + i * 0.25 + i * 7;
        Rects[(j * 4 + i) * 4 + 1] := 2 + j * 0.25 + j * 7;
        Rects[(j * 4 + i) * 4 + 2] := 2 + i * 0.25 + i * 7 + 5;
        Rects[(j * 4 + i) * 4 + 3] := 2 + j * 0.25 + j * 7 + 5;
      end;
    al_get_clipping_rectangle (cx, cy, cw, ch);
    al_clear_to_color (Ex.Background);

    SetXY (8, 0);
    Printf ('Drawing %s (press SPACE to change)', [Names[Ex.What]]);

    SetXY(8, 16);
    Print ('Original');

    SetXY (80, 16);
    Print ('Enlarged x 16');

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);

    if Ex.Software then
    begin
      al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
      al_set_new_bitmap_format (al_get_bitmap_format (al_get_target_bitmap));
      mem := al_create_bitmap (w, h);
      al_set_target_bitmap (mem);
      x := 0;
      y := 0;
      sw := 'software'
    end
    else begin
      mem := Nil;
      x := 8;
      y := 40;
      sw := 'hardware'
    end;
    al_draw_bitmap (Ex.Pattern, x, y, 0);

  { Draw the test scene. }

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    for i := 0 to (RectsNum - 1) do
    begin
      rgba := Ex.Foreground;
      rgba.a := rgba.a * 0.5; { May be "/ 2" is better? }
      Primitive (
         x + Rects[i * 4 + 0],
         y + Rects[i * 4 + 1],
         x + Rects[i * 4 + 2],
         y + Rects[i * 4 + 3],
         rgba, False)
    end;

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);

    if Ex.Software then
    begin
      al_set_target_bitmap(screen);
      x := 8;
      y := 40;
      al_draw_bitmap(mem, x, y, 0);
      al_destroy_bitmap(mem);
    end;

  { Grab screen contents into our bitmap. }
    al_set_target_bitmap (Ex.Zoom);
    al_draw_bitmap_region (screen, x, y, w, h, 0, 0, 0);
    al_set_target_bitmap (screen);

  { Draw it enlarged. }
    x := 80;
    y := 40;
    al_draw_scaled_bitmap (Ex.Zoom, 0, 0, w, h, x, y, w * 16, h * 16, 0);

  { Draw outlines. }
    for i := 0 to (RectsNum - 1) do
      Primitive (
         x + Rects[i * 4 + 0] * 16,
         y + Rects[i * 4 + 1] * 16,
         x + Rects[i * 4 + 2] * 16,
         y + Rects[i * 4 + 3] * 16,
         Ex.Outline, True);

    SetXY (8, 640 - 48);
    Printf ('Thickness: %d (press T to change)', [Ex.Thickness]);
    Printf ('Drawing with: %s (press S to change)', [sw]);
    Printf ('Supersampling: %dx (edit ex_draw.ini to change)', [Ex.Samples]);

{ FIXME: doesn't work
      al_get_display_option (ALLEGRO_SAMPLE_BUFFERS));
}
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
        Tick;
        NeedDraw := False
      end;

      al_wait_for_event (Ex.Queue, @Event);

      case Event.ftype of
      ALLEGRO_EVENT_DISPLAY_CLOSE:
        Exit;
      ALLEGRO_EVENT_KEY_DOWN:
        case Event.keyboard.keycode of
        ALLEGRO_KEY_ESCAPE:
          Exit;
        ALLEGRO_KEY_SPACE:
          begin
            Inc (Ex.What);
            if Ex.What >= 5 then Ex.What := 0
          end;
        ALLEGRO_KEY_S:
          Ex.Software := not Ex.Software;
        ALLEGRO_KEY_T:
          begin
            Inc (Ex.Thickness);
            if Ex.Thickness >= 2 then Ex.Thickness := 0
          end;
        end;
      ALLEGRO_EVENT_TIMER:
        NeedDraw := True;
      end
    end
  end;



  procedure Init;
  begin
    Ex.FPS := 60;

    Ex.Font := al_load_font ('data/fixed_font.tga', 0, 0);
    if Ex.Font = Nil then AbortExample ('data/fixed_font.tga not found.');
    Ex.Background := al_color_name ('beige');
    Ex.Foreground := al_color_name ('black');
    Ex.Outline := al_color_name ('red');
    Ex.Text := al_color_name ('blue');
    Ex.White := al_color_name ('white');
    Ex.Pattern := al_create_bitmap (32, 32);
    Ex.Zoom := al_create_bitmap (32, 32);
    DrawPattern (Ex.Pattern);
  end;



var
  Display: ALLEGRO_DISPLAYptr;
  Timer: ALLEGRO_TIMERptr;
{ TODO Not yet implemented.
  Config: ALLEGRO_CONFIGptr;
  Value, Str: String;
}
begin
  if not al_init then AbortExample ('Could not init Allegro.');

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
  if Ex.samples > 0 then
  begin
    al_set_new_display_option (ALLEGRO_SAMPLE_BUFFERS, 1, ALLEGRO_REQUIRE);
    al_set_new_display_option (ALLEGRO_SAMPLES, Ex.Samples, ALLEGRO_SUGGEST)
  end;
  Display := al_create_display (640, 640);
  if Display = Nil then AbortExample ('Unable to create display.');

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
end.
