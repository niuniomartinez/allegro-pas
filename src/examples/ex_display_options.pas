PROGRAM ex_display_options;
(* Test retrieving and settings possible modes.

   Implementation note:  Compiler will return several warning messages about
   some variables that aren't initialized.  You can ignore them as they're
   initialized properly but the compiler can't find it. *)
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
    Allegro5, al5base, al5font, al5primitives;

  type
    TOptions = record
      Name: AL_STR;
      Option: ALLEGRO_DISPLAY_OPTIONS;
      Value, MaxValue: LongInt;
      Required: LongInt;
    end;

  var
    Font: ALLEGRO_FONTptr;
    FontH, ModesCount, OptionsCount: Integer;
    Status: AL_STR;
    Flags, OldFlags: Integer;
    VisibleRows, FirstVisibleRow: Integer;
    SelectedColumn, SelectedMode, SelectedOption: Integer;
    Options: array [0..23] of TOptions;
    FlagNames: array [0..31] of AL_STR;

  procedure SetOptions;

    procedure SetOption (Ndx: Integer; aName: AL_STR; aOption: ALLEGRO_DISPLAY_OPTIONS; aMax: LongInt);
    inline;
    begin
      Options[Ndx].Name := aName;
      Options[Ndx].Option := aOption;
      Options[Ndx].Value := 0;
      Options[Ndx].MaxValue := aMax;
      Options[Ndx].Required := 0
    end;

  begin
    SetOption ( 0, 'COLOR_SIZE', ALLEGRO_COLOR_SIZE, 32);
    SetOption ( 1, 'RED_SIZE', ALLEGRO_RED_SIZE, 8);
    SetOption ( 2, 'GREEN_SIZE', ALLEGRO_GREEN_SIZE, 8);
    SetOption ( 3, 'BLUE_SIZE', ALLEGRO_BLUE_SIZE, 8);
    SetOption ( 4, 'ALPHA_SIZE', ALLEGRO_ALPHA_SIZE, 8);
    SetOption ( 5, 'RED_SHIFT', ALLEGRO_RED_SHIFT, 32);
    SetOption ( 6, 'GREEN_SHIFT', ALLEGRO_GREEN_SHIFT, 32);
    SetOption ( 7, 'BLUE_SHIFT', ALLEGRO_BLUE_SHIFT, 32);
    SetOption ( 8, 'ALPHA_SHIFT', ALLEGRO_ALPHA_SHIFT, 32);
    SetOption ( 9, 'DEPTH_SIZE', ALLEGRO_DEPTH_SIZE, 32);
    SetOption (10, 'FLOAT_COLOR', ALLEGRO_FLOAT_COLOR, 1);
    SetOption (11, 'FLOAT_DEPTH', ALLEGRO_FLOAT_DEPTH, 1);
    SetOption (12, 'STENCIL_SIZE', ALLEGRO_STENCIL_SIZE, 32);
    SetOption (13, 'SAMPLE_BUFFERS', ALLEGRO_SAMPLE_BUFFERS, 1);
    SetOption (14, 'SAMPLES', ALLEGRO_SAMPLES, 8);
    SetOption (15, 'RENDER_METHOD', ALLEGRO_RENDER_METHOD, 2);
    SetOption (16, 'SINGLE_BUFFER', ALLEGRO_SINGLE_BUFFER, 1);
    SetOption (17, 'SWAP_METHOD', ALLEGRO_SWAP_METHOD, 1);
    SetOption (18, 'VSYNC', ALLEGRO_VSYNC, 2);
    SetOption (19, 'COMPATIBLE_DISPLAY', ALLEGRO_COMPATIBLE_DISPLAY, 1);
    SetOption (20, 'MAX_BITMAP_SIZE', ALLEGRO_MAX_BITMAP_SIZE, 65536);
    SetOption (21, 'SUPPORT_NPOT_BITMAP', ALLEGRO_SUPPORT_NPOT_BITMAP, 1);
    SetOption (22, 'CAN_DRAW_INTO_BITMAP', ALLEGRO_CAN_DRAW_INTO_BITMAP, 1);
    SetOption (23, 'SUPPORT_SEPARATE_ALPHA', ALLEGRO_SUPPORT_SEPARATE_ALPHA, 1)
  end;



  procedure InitFlags;
  var
    i, v: Integer;
  begin
    for i := Low (FlagNames) to High (FlagNames) do
    begin
      v := i shl i;
      if v = Ord (ALLEGRO_WINDOWED) then FlagNames[i] := 'WINDOWED';
      if v = Ord (ALLEGRO_FULLSCREEN) then FlagNames[i] := 'FULLSCREEN';
      if v = Ord (ALLEGRO_OPENGL) then FlagNames[i] := 'OPENGL';
      if v = Ord (ALLEGRO_RESIZABLE) then FlagNames[i] := 'RESIZABLE';
      if v = Ord (ALLEGRO_FRAMELESS) then FlagNames[i] := 'FRAMELESS';
      if v = Ord (ALLEGRO_GENERATE_EXPOSE_EVENTS) then
        FlagNames[i] := 'GENERATE_EXPOSE_EVENTS';
      if v = Ord (ALLEGRO_FULLSCREEN_WINDOW) then
        FlagNames[i] := 'FULLSCREEN_WINDOW';
      if v = Ord (ALLEGRO_MINIMIZED) then FlagNames[i] := 'MINIMIZED'
    end
  end;



  procedure LoadFont;
  begin
    Font := al_create_builtin_font;
    if Font = Nil then AbortExample ('Error creating builtin font');
    FontH := al_get_font_line_height (Font)
  end;



  procedure DisplayOptions (Display: ALLEGRO_DISPLAYptr);
  var
    i, y, x, n, dw, dh: Integer;
    c: ALLEGRO_COLOR;
    Mode: ALLEGRO_DISPLAY_MODE;
    Tmp: AL_STR;
  begin
    y := 10;
    x := 10;
    n := OptionsCount;
    dw := al_get_display_width (Display);
    dh := al_get_display_height (Display);

    ModesCount := al_get_num_display_modes;

    c := al_map_rgb_f (0.8, 0.8, 1);
    al_draw_text (Font, c, x, y, 0, 'Create new display');
    Inc (y, FontH);
    i := FirstVisibleRow;
    while (i < ModesCount + 2)
    and (i < FirstVisibleRow + VisibleRows)
    do begin
      if i > 1 then
        al_get_display_mode (i - 2, mode)
      else if i = 1  then
      begin
        Mode.width := 800;
        Mode.height := 600;
        Mode.format := 0;
        Mode.refresh_rate := 0
      end
      else begin
        Mode.width := 800;
        Mode.height := 600;
        Mode.format := 0;
        Mode.refresh_rate := 0
      end;
      if (SelectedColumn = 0) and (SelectedMode = i) then
      begin
        c := al_map_rgb_f (1, 1, 0);
        al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
        al_draw_filled_rectangle (x, y, x + 300, y + FontH, c)
      end;
      c := al_map_rgb_f (0, 0, 0);
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      if (i = FirstVisibleRow) and (i > 0)
      or (i = FirstVisibleRow + VisibleRows - 1) and (i < ModesCount + 1)
      then
        al_draw_text (Font, c, x, y, 0, '...')
      else begin
        if i > 1 then
          Tmp := 'Fullscreen'
        else if i = 0 then
          Tmp := 'Windowed'
        else
          Tmp := 'FS Window';
        al_draw_textf (
          Font, c, x, y, 0,
          '%s %d x %d (fmt: %x, %d Hz)',
          [Tmp, Mode.width, Mode.height, Mode.format, Mode.refresh_rate]
        )
      end;
      y := y + FontH;
      Inc (i)
    end;

    x := Trunc (dw / 2 + 10);
    y := 10;
    c := al_map_rgb_f (0.8, 0.8, 1);
    al_draw_text (Font, c, x, y, 0, 'Options for new display');
    al_draw_text (Font, c, dw - 10, y, ALLEGRO_ALIGN_RIGHT, '(current display)');
    y := y + FontH;
    for i := 0 to n - 1 do
    begin
      if (SelectedColumn = 1) and (SelectedOption = i) then
      begin
        c := al_map_rgb_f (1, 1, 0);
        al_draw_filled_rectangle (x, y, x + 300, y + FontH, c)
      end;

      case Options[i].Required of
        ALLEGRO_REQUIRE: c := al_map_rgb_f (0.5, 0, 0);
        ALLEGRO_SUGGEST: c := al_map_rgb_f (0, 0, 0);
        ALLEGRO_DONTCARE: c := al_map_rgb_f (0.5, 0.5, 0.5);
      end;
      case Options[i].Required of
        ALLEGRO_REQUIRE: Tmp := 'required';
        ALLEGRO_SUGGEST: Tmp := 'suggested';
        else             Tmp := 'ignored';
      end;
      al_draw_textf (
        Font, c, x, y, 0,
        '%s: %d (%s)',
        [Options[i].Name, Options[i].Value, Tmp]
      );
      c := al_map_rgb_f (0.9, 0.5, 0.3);
      al_draw_textf (
        Font, c, dw - 10, y, ALLEGRO_ALIGN_RIGHT,
        '%d',
        [al_get_display_option (Display, Options[i].Option)]
      );
      y := y + FontH
    end;

    c := al_map_rgb_f (0, 0, 0.8);
    x := 10;
    y := dh - FontH - 10;
    y := y - FontH;
    al_draw_text (Font, c, x, y, 0, 'PageUp/Down: modify values');
    y := y - FontH;
    al_draw_text (Font, c, x, y, 0, 'Return: set mode or require option');
    y := y - FontH;
    al_draw_text (Font, c, x, y, 0, 'Cursor keys: change selection');

    y := y - (FontH * 2);
    for i := Low (FlagNames) to High (FlagNames) do
    begin
      if FlagNames[i] <> '' then
      begin
        Tmp := 'r';
        if (Flags and (1 shl i)) <> 0 then
          c := al_map_rgb_f (0.5, 0, 0)
        else if (OldFlags and (1 shl i)) <> 0 then
          c := al_map_rgb_f (0.5, 0.4, 0.4)
        else
          Tmp := '';
        if Tmp <> '' then
        begin
          al_draw_text (Font, c, x, y, 0, FlagNames[i]);
          x := x + al_get_text_width (Font, FlagNames[i]) + 10
        end
      end
    end;

    c := al_map_rgb_f (1, 0, 0);
    al_draw_text (Font, c, dw / 2, dh - FontH, ALLEGRO_ALIGN_CENTRE, Status)
  end;



  procedure UpdateUI;
  var
    h: Integer;
  begin
    h := al_get_display_height (al_get_current_display);
    VisibleRows := h div FontH - 10
  end;



  var
    Display, NewDisplay: ALLEGRO_DISPLAYptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Timer: ALLEGRO_TIMERptr;
    Redraw, EndExample: Boolean;
    dw, y, Row, Column, Tmp: Integer;
    Mode: ALLEGRO_DISPLAY_MODE;
begin;
  Redraw := False;
  Flags := 0; OldFlags := 0;
  FirstVisibleRow := 0;

  if not al_init then AbortExample ('Could not init Allegro.');
  SetOptions;
  InitFlags;
  al_init_primitives_addon;

  al_install_keyboard;
  al_install_mouse;
  al_init_font_addon;

  Display := al_create_display (800, 600);
  if Display = Nil then AbortExample ('Error creating display');

  LoadFont;

  Timer := al_create_timer (1.0 / 60);

  ModesCount := al_get_num_display_modes;
  OptionsCount := Length (Options);

  UpdateUI;

  al_clear_to_color (al_map_rgb_f (1, 1, 1));
  DisplayOptions (Display);
  al_flip_display;

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_mouse_event_source);
  al_register_event_source (Queue, al_get_display_event_source (Display));
  al_register_event_source (Queue, al_get_timer_event_source (Timer));

  al_start_timer (Timer);

  EndExample := False;
  repeat
    al_wait_for_event (Queue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EndExample := True;
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
      if Event.mouse.button = 1 then
      begin
        dw := al_get_display_width (Display);
        y := 10;
        Row := (Event.mouse.y - y) div FontH - 1;
        Column := Event.mouse.x div (dw div 2);
        if Column = 0 then
        begin
          if (Row >= 0) and (Row <= ModesCount) then
          begin
            SelectedColumn := Column;
            SelectedMode := Row;
            Redraw := True
          end
        end;
        if Column = 1 then
        begin
          if (Row >= 0) and (Row < OptionsCount) then
          begin
            SelectedColumn := Column;
            SelectedOption := Row;
            Redraw := True
          end
        end
      end;
    ALLEGRO_EVENT_TIMER:
      begin
        Tmp := al_get_display_flags (Display);
        if Tmp <> Flags then
        begin
          Redraw := True;
          Flags := Tmp;
          OldFlags := OldFlags or Tmp
        end
      end;
    ALLEGRO_EVENT_KEY_CHAR:
      begin
        case event.keyboard.keycode of
        ALLEGRO_KEY_ESCAPE:
          EndExample := True;
        ALLEGRO_KEY_LEFT:
          begin
            SelectedColumn := 0;
            Redraw := True
          end;
        ALLEGRO_KEY_RIGHT:
          begin
            SelectedColumn := 1;
            Redraw := True
          end;
        ALLEGRO_KEY_UP:
          begin
            if SelectedColumn = 0 then Dec (SelectedMode);
            if SelectedColumn = 1 then Dec (SelectedOption);
            Redraw := True
          end;
         ALLEGRO_KEY_DOWN:
          begin
            if SelectedColumn = 0 then Inc (SelectedMode);
            if SelectedColumn = 1 then Inc (SelectedOption);
            Redraw := True
          end;
        ALLEGRO_KEY_ENTER:
          begin
            if SelectedColumn = 0 then
            begin
              if SelectedMode > 1 then
              begin
                al_get_display_mode (SelectedMode - 2, Mode);
                al_set_new_display_flags (ALLEGRO_FULLSCREEN)
              end
              else if SelectedMode = 1 then
              begin
                Mode.width := 800;
                Mode.height := 600;
                al_set_new_display_flags (ALLEGRO_FULLSCREEN_WINDOW)
              end
              else begin
                Mode.width := 800;
                Mode.height := 600;
                al_set_new_display_flags (ALLEGRO_WINDOWED)
              end;

              al_destroy_font (Font);
              Font := Nil;

              NewDisplay := al_create_display (Mode.width, Mode.height);
              if NewDisplay <> Nil then
              begin
                al_destroy_display (Display);
                Display := NewDisplay;
                al_set_target_backbuffer (Display);
                al_register_event_source (Queue, al_get_display_event_source (Display));
                UpdateUI;
                Status := 'Display creation succeeded.'
              end
              else
                Status := 'Display creation failed.';
              LoadFont
            end;
            if SelectedColumn = 1 then
            begin
              Inc (Options[SelectedOption].Required);
              Options[SelectedOption].Required :=
                Options[SelectedOption].Required mod 3;
              al_set_new_display_option (
                Options[SelectedOption].Option,
                Options[SelectedOption].Value,
                Options[SelectedOption].Required
              )
            end;
            Redraw := True
          end;
        end;
        Tmp := 0;
        if Event.keyboard.keycode = ALLEGRO_KEY_PGUP then Tmp := 1;
        if Event.keyboard.keycode = ALLEGRO_KEY_PGDN then Tmp := -1;
        if (Tmp <> 0) and (SelectedColumn = 1) then
        begin
          Inc (Options[SelectedOption].Value, Tmp);
          if Options[SelectedOption].Value < 0 then
            Options[SelectedOption].Value := 0;
          if Options[SelectedOption].Value >
             Options[SelectedOption].MaxValue
          then
            Options[SelectedOption].Value :=
              Options[SelectedOption].MaxValue;
          al_set_new_display_option (
            Options[SelectedOption].Option,
            Options[SelectedOption].Value,
            Options[SelectedOption].Required
          );
          Redraw := True
        end
      end;
    end;

    if SelectedMode < 0 then SelectedMode := 0;
    if SelectedMode > ModesCount + 1 then SelectedMode := ModesCount + 1;
    if SelectedOption < 0 then SelectedOption := 0;
    if SelectedOption >= OptionsCount then SelectedOption := OptionsCount - 1;
    if SelectedMode < FirstVisibleRow then FirstVisibleRow := SelectedMode;
    if SelectedMode > FirstVisibleRow + VisibleRows - 1 then
      FirstVisibleRow := SelectedMode - VisibleRows + 1;

    if Redraw and al_is_event_queue_empty (Queue) then
    begin
      Redraw := false;
      al_clear_to_color (al_map_rgb_f (1, 1, 1));
      DisplayOptions (Display);
      al_flip_display
    end
  until EndExample;

  al_destroy_font (Font)
end.
