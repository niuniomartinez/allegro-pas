PROGRAM ex_display_options;
(* Test retrieving and settings possible modes. *)
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
    Allegro5, al5font, al5primitives,
    sysutils;

  VAR
    Font: ALLEGRO_FONTptr;
    FontH, ModesCount, OptionsCount: INTEGER;
    Status: STRING;
    Flags, OldFlags: INTEGER;

    VisibleRows, FirstVisibleRow: INTEGER;

    SelectedColumn, SelectedMode, SelectedOption: INTEGER;

  TYPE
    TOptions = RECORD
      Name: STRING;
      Option: ALLEGRO_DISPLAY_OPTIONS;
      Value, MaxValue: LONGINT;
      Required: LONGINT;
    END;

  VAR
    Options: ARRAY [0..23] OF TOptions;
    FlagNames: ARRAY [0..31] OF STRING;

  PROCEDURE SetOptions;

    PROCEDURE SetOption (Ndx: INTEGER; aName: STRING; aOption: ALLEGRO_DISPLAY_OPTIONS; aMax: LONGINT);
    INLINE;
    BEGIN
      Options[Ndx].Name := aName;
      Options[Ndx].Option := aOption;
      Options[Ndx].Value := 0;
      Options[Ndx].MaxValue := aMax;
      Options[Ndx].Required := 0
    END;

  BEGIN
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
  END;



  PROCEDURE InitFlags;
  VAR
    i, v: INTEGER;
  BEGIN
    FOR i := LOW (FlagNames) TO HIGH (FlagNames) DO
    BEGIN
      v := i SHL i;
      IF v = ORD (ALLEGRO_WINDOWED) THEN FlagNames[i] := 'WINDOWED';
      IF v = ORD (ALLEGRO_FULLSCREEN) THEN FlagNames[i] := 'FULLSCREEN';
      IF v = ORD (ALLEGRO_OPENGL) THEN FlagNames[i] := 'OPENGL';
      IF v = ORD (ALLEGRO_RESIZABLE) THEN FlagNames[i] := 'RESIZABLE';
      IF v = ORD (ALLEGRO_FRAMELESS) THEN FlagNames[i] := 'FRAMELESS';
      IF v = ORD (ALLEGRO_GENERATE_EXPOSE_EVENTS) THEN
	FlagNames[i] := 'GENERATE_EXPOSE_EVENTS';
      IF v = ORD (ALLEGRO_FULLSCREEN_WINDOW) THEN
	FlagNames[i] := 'FULLSCREEN_WINDOW';
      IF v = ORD (ALLEGRO_MINIMIZED) THEN FlagNames[i] := 'MINIMIZED'
    END
  END;



  PROCEDURE LoadFont;
  BEGIN
    Font := al_create_builtin_font;
    IF Font = NIL THEN AbortExample ('Error creating builtin font');
    FontH := al_get_font_line_height (Font)
  END;



  PROCEDURE DisplayOptions (Display: ALLEGRO_DISPLAYptr);
  VAR
    i, y, x, n, dw, dh: INTEGER;
    c: ALLEGRO_COLOR;
    Mode: ALLEGRO_DISPLAY_MODE;
    Tmp: STRING;
  BEGIN
    y := 10;
    x := 10;
    n := OptionsCount;
    dw := al_get_display_width (Display);
    dh := al_get_display_height (Display);

    ModesCount := al_get_num_display_modes;

    c := al_map_rgb_f (0.8, 0.8, 1);
    al_draw_text (Font, c, x, y, 0, 'Create new display');
    INC (y, FontH);
    i := FirstVisibleRow;
    WHILE (i < ModesCount + 2)
    AND (i < FirstVisibleRow + VisibleRows)
    DO BEGIN
      IF i > 1 THEN
	al_get_display_mode (i - 2, mode)
      ELSE IF i = 1  THEN
      BEGIN
        Mode.width := 800;
        Mode.height := 600;
        Mode.format := 0;
        Mode.refresh_rate := 0
      END
      ELSE BEGIN
        Mode.width := 800;
        Mode.height := 600;
        Mode.format := 0;
        Mode.refresh_rate := 0
      END;
      IF (SelectedColumn = 0) AND (SelectedMode = i) THEN
      BEGIN
	c := al_map_rgb_f (1, 1, 0);
	al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
	al_draw_filled_rectangle (x, y, x + 300, y + FontH, c)
      END;
      c := al_map_rgb_f (0, 0, 0);
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      IF (i = FirstVisibleRow) AND (i > 0)
      OR (i = FirstVisibleRow + VisibleRows - 1) AND (i < ModesCount + 1)
      THEN
	al_draw_text (Font, c, x, y, 0, '...')
      ELSE BEGIN
	IF i > 1 THEN
	  Tmp := 'Fullscreen'
	ELSE IF i = 0 THEN
	  Tmp := 'Windowed'
	ELSE
	  Tmp := 'FS Window';
	al_draw_text (
	  Font, c, x, y, 0,
	  Format (
	    '%s %d x %d (fmt: %x, %d Hz)', [
	      Tmp, Mode.width, Mode.height, Mode.format, Mode.refresh_rate
	    ]
	  )
	)
      END;
      y := y + FontH;
      INC (i)
    END;

    x := TRUNC (dw / 2 + 10);
    y := 10;
    c := al_map_rgb_f (0.8, 0.8, 1);
    al_draw_text (Font, c, x, y, 0, 'Options for new display');
    al_draw_text (Font, c, dw - 10, y, ALLEGRO_ALIGN_RIGHT, '(current display)');
    y := y + FontH;
    FOR i := 0 TO n - 1 DO
    BEGIN
      IF (SelectedColumn = 1) AND (SelectedOption = i) THEN
      BEGIN
	c := al_map_rgb_f (1, 1, 0);
	al_draw_filled_rectangle (x, y, x + 300, y + FontH, c)
      END;

      CASE Options[i].Required OF
	ALLEGRO_REQUIRE: c := al_map_rgb_f (0.5, 0, 0);
	ALLEGRO_SUGGEST: c := al_map_rgb_f (0, 0, 0);
	ALLEGRO_DONTCARE: c := al_map_rgb_f (0.5, 0.5, 0.5);
      END;
      CASE Options[i].Required OF
	ALLEGRO_REQUIRE: Tmp := 'required';
	ALLEGRO_SUGGEST: Tmp := 'suggested';
	ELSE             Tmp := 'ignored';
      END;
      al_draw_text (
	Font, c, x, y, 0,
	Format ('%s: %d (%s)', [Options[i].Name, Options[i].Value, Tmp])
      );
      c := al_map_rgb_f (0.9, 0.5, 0.3);
      al_draw_text (
	Font, c, dw - 10, y, ALLEGRO_ALIGN_RIGHT,
	Format ('%d', [al_get_display_option (Display, Options[i].Option)])
      );
      y := y + FontH
    END;

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
    FOR i := LOW (FlagNames) TO HIGH (FlagNames) DO
    BEGIN
      IF FlagNames[i] <> '' THEN
      BEGIN
	Tmp := 'r';
	IF (Flags AND (1 SHL i)) <> 0 THEN
	  c := al_map_rgb_f (0.5, 0, 0)
	ELSE IF (OldFlags AND (1 SHL i)) <> 0 THEN
	  c := al_map_rgb_f (0.5, 0.4, 0.4)
	ELSE
	  Tmp := '';
	IF Tmp <> '' THEN
	BEGIN
	  al_draw_text (Font, c, x, y, 0, FlagNames[i]);
	  x := x + al_get_text_width (Font, FlagNames[i]) + 10
	END
      END
    END;

    c := al_map_rgb_f (1, 0, 0);
    al_draw_text (Font, c, dw / 2, dh - FontH, ALLEGRO_ALIGN_CENTRE, Status)
  END;



  PROCEDURE UpdateUI;
  VAR
    h: INTEGER;
  BEGIN
    h := al_get_display_height (al_get_current_display);
    VisibleRows := h DIV FontH - 10
  END;



  VAR
    Display, NewDisplay: ALLEGRO_DISPLAYptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Timer: ALLEGRO_TIMERptr;
    Redraw, EndExample: BOOLEAN;
    dw, y, Row, Column, Tmp: INTEGER;
    Mode: ALLEGRO_DISPLAY_MODE;
BEGIN;
  Redraw := FALSE;
  Flags := 0; OldFlags := 0;
  FirstVisibleRow := 0;

  IF NOT al_init THEN AbortExample ('Could not init Allegro.');
  SetOptions;
  InitFlags;
  al_init_primitives_addon;

  al_install_keyboard;
  al_install_mouse;
  al_init_font_addon;

  Display := al_create_display (800, 600);
  IF Display = NIL THEN AbortExample ('Error creating display');

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

  EndExample := FALSE;
  REPEAT
    al_wait_for_event (Queue, Event);
    CASE Event._type OF
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EndExample := TRUE;
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
      IF Event.mouse.button = 1 THEN
      BEGIN
	dw := al_get_display_width (Display);
	y := 10;
	Row := (Event.mouse.y - y) DIV FontH - 1;
	Column := Event.mouse.x DIV (dw DIV 2);
	IF Column = 0 THEN
	BEGIN
	  IF (Row >= 0) AND (Row <= ModesCount) THEN
	  BEGIN
	    SelectedColumn := Column;
	    SelectedMode := Row;
	    Redraw := TRUE
	  END
	END;
	IF Column = 1 THEN
	BEGIN
	  IF (Row >= 0) AND (Row < OptionsCount) THEN
	  BEGIN
	    SelectedColumn := Column;
	    SelectedOption := Row;
	    Redraw := TRUE
	  END
	END
      END;
    ALLEGRO_EVENT_TIMER:
      BEGIN
	Tmp := al_get_display_flags (Display);
	IF Tmp <> Flags THEN
	BEGIN
	  Redraw := TRUE;
	  Flags := Tmp;
	  OldFlags := OldFlags OR Tmp
	END
      END;
    ALLEGRO_EVENT_KEY_CHAR:
      BEGIN
	CASE event.keyboard.keycode OF
	ALLEGRO_KEY_ESCAPE:
	  EndExample := TRUE;
	ALLEGRO_KEY_LEFT:
	  BEGIN
	    SelectedColumn := 0;
	    Redraw := TRUE
	  END;
	ALLEGRO_KEY_RIGHT:
	  BEGIN
	    SelectedColumn := 1;
	    Redraw := TRUE
	  END;
	ALLEGRO_KEY_UP:
	  BEGIN
	    IF SelectedColumn = 0 THEN DEC (SelectedMode);
	    IF SelectedColumn = 1 THEN DEC (SelectedOption);
	    Redraw := TRUE
	  END;
	 ALLEGRO_KEY_DOWN:
	  BEGIN
	    IF SelectedColumn = 0 THEN INC (SelectedMode);
	    IF SelectedColumn = 1 THEN INC (SelectedOption);
	    Redraw := TRUE
	  END;
	ALLEGRO_KEY_ENTER:
	  BEGIN
	    IF SelectedColumn = 0 THEN
	    BEGIN
	      IF SelectedMode > 1 THEN
	      BEGIN
		al_get_display_mode (SelectedMode - 2, Mode);
		al_set_new_display_flags (ALLEGRO_FULLSCREEN)
	      END
	      ELSE IF SelectedMode = 1 THEN
	      BEGIN
		Mode.width := 800;
		Mode.height := 600;
		al_set_new_display_flags (ALLEGRO_FULLSCREEN_WINDOW)
	      END
	      ELSE BEGIN
		Mode.width := 800;
		Mode.height := 600;
		al_set_new_display_flags (ALLEGRO_WINDOWED)
	      END;

	      al_destroy_font (Font);
	      Font := NIL;

	      NewDisplay := al_create_display (Mode.width, Mode.height);
	      IF NewDisplay <> NIL THEN
	      BEGIN
		al_destroy_display (Display);
		Display := NewDisplay;
		al_set_target_backbuffer (Display);
		al_register_event_source (Queue, al_get_display_event_source (Display));
		UpdateUI;
		Status := 'Display creation succeeded.'
	      END
	      ELSE
		Status := 'Display creation failed.';
	      LoadFont
	    END;
	    IF SelectedColumn = 1 THEN
	    BEGIN
	      INC (Options[SelectedOption].Required);
	      Options[SelectedOption].Required :=
		Options[SelectedOption].Required MOD 3;
	      al_set_new_display_option (
		Options[SelectedOption].Option,
		Options[SelectedOption].Value,
		Options[SelectedOption].Required
	      )
	    END;
	    Redraw := TRUE
	  END;
	END;
	Tmp := 0;
	IF Event.keyboard.keycode = ALLEGRO_KEY_PGUP THEN Tmp := 1;
	IF Event.keyboard.keycode = ALLEGRO_KEY_PGDN THEN Tmp := -1;
        IF (Tmp <> 0) AND (SelectedColumn = 1) THEN
	BEGIN
	  INC (Options[SelectedOption].Value, Tmp);
	  IF Options[SelectedOption].Value < 0 THEN
	    Options[SelectedOption].Value := 0;
	  IF Options[SelectedOption].Value >
	     Options[SelectedOption].MaxValue
	  THEN
	    Options[SelectedOption].Value :=
	      Options[SelectedOption].MaxValue;
	  al_set_new_display_option (
	    Options[SelectedOption].Option,
	    Options[SelectedOption].Value,
	    Options[SelectedOption].Required
	  );
	  Redraw := TRUE
	END
      END;
    END;

    IF SelectedMode < 0 THEN SelectedMode := 0;
    IF SelectedMode > ModesCount + 1 THEN SelectedMode := ModesCount + 1;
    IF SelectedOption < 0 THEN SelectedOption := 0;
    IF SelectedOption >= OptionsCount THEN SelectedOption := OptionsCount - 1;
    IF SelectedMode < FirstVisibleRow THEN FirstVisibleRow := SelectedMode;
    IF SelectedMode > FirstVisibleRow + VisibleRows - 1 THEN
      FirstVisibleRow := SelectedMode - VisibleRows + 1;

    IF Redraw AND al_is_event_queue_empty (Queue) THEN
    BEGIN
      Redraw := false;
      al_clear_to_color (al_map_rgb_f (1, 1, 1));
      DisplayOptions (Display);
      al_flip_display
    END
  UNTIL EndExample;

  al_destroy_font (Font)
END.
