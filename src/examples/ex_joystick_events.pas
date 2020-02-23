PROGRAM ex_joysick_events;
(*
 *    Example program for the Allegro library, by Peter Wang.
 *    Pascal version by Guillermo "Ñuño" Martínez.
 *
 *    This program tests joystick events.
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

  USES
    Common,
    Allegro5, al5base, al5font, al5primitives;

  CONST
    MAX_AXES    = 3;
    MAX_STICKS  = 16;
    MAX_BUTTONS = 32;

  VAR
  (* globals *)
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Font: ALLEGRO_FONTptr;
    Black, Grey, White: ALLEGRO_COLOR;

    NumSticks, NumButtons: INTEGER;
    NumAxes: ARRAY [0..MAX_STICKS-1] OF INTEGER;
    Joys: ARRAY [0..MAX_STICKS-1] OF ARRAY [0..MAX_AXES-1] OF SINGLE;
    JoyButtons: ARRAY [0..MAX_BUTTONS-1] OF BOOLEAN;

  PROCEDURE InitStickValues;
  VAR
    Ndx, Ndx2: INTEGER;
  BEGIN
    NumSticks := 0;
    NumButtons := 0;
    FOR Ndx := LOW (NumAxes) TO HIGH (NumAxes) DO NumAxes[Ndx] := 0;
    FOR Ndx := LOW (Joys) TO HIGH (Joys) DO
      FOR Ndx2 := LOW (Joys[Ndx]) TO HIGH (Joys[Ndx]) DO
        Joys[Ndx][Ndx2] := 0;
    FOR Ndx := LOW (JoyButtons) TO HIGH (JoyButtons) DO
      JoyButtons[Ndx] := FALSE
  END;



  PROCEDURE SetupJoystickValues (Joy: ALLEGRO_JOYSTICKptr);
  VAR
    Jst: ALLEGRO_JOYSTICK_STATE;
    i, j: INTEGER;
  BEGIN
    InitStickValues;
    IF Joy = NIL THEN
    BEGIN
      NumSticks := 0;
      NumButtons := 0;
      EXIT
    END;

    al_get_joystick_state (Joy, Jst);

    NumSticks := al_get_joystick_num_sticks (Joy);
    IF NumSticks > MAX_STICKS THEN NumSticks := MAX_STICKS;
    FOR i := 0 TO NumSticks - 1 DO
    BEGIN
      NumAxes[i] := al_get_joystick_num_axes (Joy, i);
      FOR j := 0 TO NumAxes[i] - 1 DO
        Joys[i][j] := jst.stick[i].axis[j]
    END;

    NumButtons := al_get_joystick_num_buttons (Joy);
    IF NumButtons > MAX_BUTTONS THEN NumButtons := MAX_BUTTONS;
    FOR i := 0 TO NumButtons - 1 DO
      JoyButtons[i] := (Jst.button[i] >= 16384)
  END;



  PROCEDURE DrawJoystickAxes (Joy: ALLEGRO_JOYSTICKptr; cx, cy, Stick: INTEGER);
  CONST
    Size = 30;
    cSize = 5;
    oSize = Size + cSize;
  VAR
    zx, x, y, z, i: INTEGER;
  BEGIN
    zx := cx + oSize + cSize * 2;
    x := TRUNC (cx + Joys[Stick][0] * Size);
    y := TRUNC (cy + Joys[Stick][1] * Size);
    z := TRUNC (cy + Joys[Stick][2] * Size);

    al_draw_filled_rectangle
      (cx - oSize, cy - oSize, cx + oSize, cy + oSize, Grey);
    al_draw_rectangle (
      cx - oSize + 0.5, cy - oSize + 0.5, cx + oSize - 0.5, cy + oSize - 0.5,
      Black, 0
    );
    al_draw_filled_rectangle (x - 5, y - 5, x + 5, y + 5, Black);

    IF NumAxes[stick] >= 3 THEN
    BEGIN
      al_draw_filled_rectangle
        (zx - cSize, cy - oSize, zx + cSize, cy + oSize, Grey);
      al_draw_rectangle (
        zx - cSize + 0.5, cy - oSize + 0.5, zx + cSize - 0.5, cy + oSize - 0.5,
        Black, 0
      );
      al_draw_filled_rectangle (zx - 5, z - 5, zx + 5, z + 5, Black)
    END;

    IF Joy <> NIL THEN
    BEGIN
      al_draw_text (
        Font, Black, cx, cy + oSize + 1, ALLEGRO_ALIGN_CENTRE,
        al_get_joystick_stick_name (Joy, Stick)
      );
      FOR i := 0 TO NumAxes[Stick] - 1 DO
      BEGIN
        al_draw_text (
          Font, Black, cx, cy + oSize + (1 + i) * 10,
          ALLEGRO_ALIGN_CENTRE,
          al_get_joystick_axis_name (Joy, Stick, i)
        )
      END
    END
  END;



  PROCEDURE DrawJoystickButton (Joy: ALLEGRO_JOYSTICKptr; Button: INTEGER; Down: BOOLEAN);
  VAR
    Bmp: ALLEGRO_BITMAPptr;
    x, y: INTEGER;
    fg: ALLEGRO_COLOR;
    Name: AL_STR;
  BEGIN
    bmp := al_get_target_bitmap;
    x := TRUNC (
      al_get_bitmap_width (Bmp) / 2 - 120 + (button MOD 8) * 30
    );
    y := TRUNC (
      al_get_bitmap_height (bmp) - 120 + (button / 8) * 30
    );

    al_draw_filled_rectangle (x, y, x + 25, y + 25, Grey);
    al_draw_rectangle (x + 0.5, y + 0.5, x + 24.5, y + 24.5, Black, 0);
    IF Down THEN
    BEGIN
      al_draw_filled_rectangle (x + 2, y + 2, x + 23, y + 23, Black);
      fg := White
    END
    ELSE BEGIN
      fg := black
    END;

    IF Joy <> NIL THEN
    BEGIN
      Name := al_get_joystick_button_name (Joy, Button);
      IF Length (Name) < 4 THEN
        al_draw_text (Font, fg, x + 13, y + 8, ALLEGRO_ALIGN_CENTRE, Name)
    END
  END;



  PROCEDURE DrawAll (Joy: ALLEGRO_JOYSTICKptr);
  VAR
    Bmp: ALLEGRO_BITMAPptr;
    Width, Height, i,
    u, v, cx, cy: INTEGER;
  BEGIN
    Bmp := al_get_target_bitmap;
    Width := al_get_bitmap_width (Bmp);
    Height := al_get_bitmap_height (Bmp);

    al_clear_to_color (al_map_rgb ($FF, $FF, $C0));

    IF Joy <> NIL THEN
      al_draw_textf (
        Font, Black, Width / 2, 10, ALLEGRO_ALIGN_CENTRE,
        'Joystick: %s', [al_get_joystick_name (Joy)]
      );

    FOR i := 0 TO NumSticks - 1 DO
    BEGIN
      u := i MOD 4;
      v := i DIV 4;
      cx := TRUNC ((u + 0.5) * Width / 4);
      cy := TRUNC ((v + 0.5) * Height / 6);
      DrawJoystickAxes (Joy, cx, cy, i)
    END;

    FOR i := 0 TO NumButtons - 1 DO
      DrawJoystickButton (Joy, i, JoyButtons[i]);

    al_flip_display
  END;



  PROCEDURE MainLoop;
  VAR
    Event: ALLEGRO_EVENT;
  BEGIN
    WHILE TRUE DO
    BEGIN
      IF al_is_event_queue_empty (EventQueue) THEN
	DrawAll (al_get_joystick (0));

      al_wait_for_event (EventQueue, @Event);

      CASE Event.ftype OF
      { ALLEGRO_EVENT_JOYSTICK_AXIS - a joystick axis value changed. }
      ALLEGRO_EVENT_JOYSTICK_AXIS:
	IF (Event.joystick.stick < MAX_STICKS)
	AND (Event.joystick.axis < MAX_AXES)
	THEN
	  Joys[Event.joystick.stick][Event.joystick.axis] := Event.joystick.pos;
      { ALLEGRO_EVENT_JOYSTICK_BUTTON_DOWN - a joystick button was pressed. }
      ALLEGRO_EVENT_JOYSTICK_BUTTON_DOWN:
        JoyButtons[Event.joystick.button] := TRUE;
      { ALLEGRO_EVENT_JOYSTICK_BUTTON_UP - a joystick button was released. }
      ALLEGRO_EVENT_JOYSTICK_BUTTON_UP:
	JoyButtons[Event.joystick.button] := FALSE;
      ALLEGRO_EVENT_KEY_DOWN:
	IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN EXIT;
      { ALLEGRO_EVENT_DISPLAY_CLOSE - the window close button was pressed. }
      ALLEGRO_EVENT_DISPLAY_CLOSE:
	EXIT;
      ALLEGRO_EVENT_JOYSTICK_CONFIGURATION:
	BEGIN
	  al_reconfigure_joysticks;
	  SetupJoystickValues (al_get_joystick (0))
	END;
       { We received an event of some type we don't know about.
         Just ignore it. }
      ELSE
        ;
      END
    END
  END;



  VAR
    Display: ALLEGRO_DISPLAYptr;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');
  al_init_primitives_addon;
  al_init_font_addon;

  Display := al_create_display (1024, 768);
  IF Display = NIL THEN AbortExample ('Could not create display');

  al_install_keyboard;

  Black := al_map_rgb (0, 0, 0);
  Grey := al_map_rgb ($E0, $E0, $E0);
  White := al_map_rgb (255, 255, 255);
  Font := al_create_builtin_font;

  al_install_joystick;

  EventQueue := al_create_event_queue;
  IF EventQueue = NIL THEN AbortExample ('al_create_event_queue failed');

  IF al_get_keyboard_event_source <> NIL THEN
    al_register_event_source (EventQueue, al_get_keyboard_event_source);
  al_register_event_source (EventQueue, al_get_display_event_source (Display));
  al_register_event_source (EventQueue, al_get_joystick_event_source);

  SetupJoystickValues (al_get_joystick (0));

  MainLoop;

  al_destroy_font (Font)
END.
