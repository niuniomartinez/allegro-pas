program ex_joysick_events;
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

  uses
    Common,
    Allegro5, al5base, al5font, al5primitives;

  const
    MAX_AXES    = 3;
    MAX_STICKS  = 16;
    MAX_BUTTONS = 32;

  var
  (* globals *)
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Font: ALLEGRO_FONTptr;
    Black, Grey, White: ALLEGRO_COLOR;

    NumSticks, NumButtons: Integer;
    NumAxes: array [0..MAX_STICKS-1] of Integer;
    Joys: array [0..MAX_STICKS-1] of array [0..MAX_AXES-1] of Single;
    JoyButtons: array [0..MAX_BUTTONS-1] of Boolean;

  procedure InitStickValues;
  var
    Ndx, Ndx2: Integer;
  begin
    NumSticks := 0;
    NumButtons := 0;
    for Ndx := Low (NumAxes) to High (NumAxes) do NumAxes[Ndx] := 0;
    for Ndx := Low (Joys) to High (Joys) do
      for Ndx2 := Low (Joys[Ndx]) to High (Joys[Ndx]) do
        Joys[Ndx][Ndx2] := 0;
    for Ndx := Low (JoyButtons) to High (JoyButtons) do
      JoyButtons[Ndx] := False
  end;



  procedure SetupJoystickValues (Joy: ALLEGRO_JOYSTICKptr);
  var
    Jst: ALLEGRO_JOYSTICK_STATE;
    i, j: Integer;
  begin
    InitStickValues;
    if Joy = Nil then
    begin
      NumSticks := 0;
      NumButtons := 0;
      Exit
    end;

    al_get_joystick_state (Joy, Jst);

    NumSticks := al_get_joystick_num_sticks (Joy);
    if NumSticks > MAX_STICKS then NumSticks := MAX_STICKS;
    for i := 0 to NumSticks - 1 do
    begin
      NumAxes[i] := al_get_joystick_num_axes (Joy, i);
      for j := 0 to NumAxes[i] - 1 do
        Joys[i][j] := jst.stick[i].axis[j]
    end;

    NumButtons := al_get_joystick_num_buttons (Joy);
    if NumButtons > MAX_BUTTONS then NumButtons := MAX_BUTTONS;
    for i := 0 to NumButtons - 1 do
      JoyButtons[i] := (Jst.button[i] >= 16384)
  end;



  procedure DrawJoystickAxes (Joy: ALLEGRO_JOYSTICKptr; cx, cy, Stick: Integer);
  const
    Size = 30;
    cSize = 5;
    oSize = Size + cSize;
  var
    zx, x, y, z, i: Integer;
  begin
    zx := cx + oSize + cSize * 2;
    x := Trunc (cx + Joys[Stick][0] * Size);
    y := Trunc (cy + Joys[Stick][1] * Size);
    z := Trunc (cy + Joys[Stick][2] * Size);

    al_draw_filled_rectangle
      (cx - oSize, cy - oSize, cx + oSize, cy + oSize, Grey);
    al_draw_rectangle (
      cx - oSize + 0.5, cy - oSize + 0.5, cx + oSize - 0.5, cy + oSize - 0.5,
      Black, 0
    );
    al_draw_filled_rectangle (x - 5, y - 5, x + 5, y + 5, Black);

    if NumAxes[stick] >= 3 then
    begin
      al_draw_filled_rectangle
        (zx - cSize, cy - oSize, zx + cSize, cy + oSize, Grey);
      al_draw_rectangle (
        zx - cSize + 0.5, cy - oSize + 0.5, zx + cSize - 0.5, cy + oSize - 0.5,
        Black, 0
      );
      al_draw_filled_rectangle (zx - 5, z - 5, zx + 5, z + 5, Black)
    end;

    if Joy <> Nil then
    begin
      al_draw_text (
        Font, Black, cx, cy + oSize + 1, ALLEGRO_ALIGN_CENTRE,
        al_get_joystick_stick_name (Joy, Stick)
      );
      for i := 0 to NumAxes[Stick] - 1 do
      begin
        al_draw_text (
          Font, Black, cx, cy + oSize + (1 + i) * 10,
          ALLEGRO_ALIGN_CENTRE,
          al_get_joystick_axis_name (Joy, Stick, i)
        )
      end
    end
  end;



  procedure DrawJoystickButton (Joy: ALLEGRO_JOYSTICKptr; Button: Integer; Down: Boolean);
  var
    Bmp: ALLEGRO_BITMAPptr;
    x, y: Integer;
    fg: ALLEGRO_COLOR;
    Name: AL_STR;
  begin
    bmp := al_get_target_bitmap;
    x := Trunc (
      al_get_bitmap_width (Bmp) / 2 - 120 + (button mod 8) * 30
    );
    y := Trunc (
      al_get_bitmap_height (bmp) - 120 + (button / 8) * 30
    );

    al_draw_filled_rectangle (x, y, x + 25, y + 25, Grey);
    al_draw_rectangle (x + 0.5, y + 0.5, x + 24.5, y + 24.5, Black, 0);
    if Down then
    begin
      al_draw_filled_rectangle (x + 2, y + 2, x + 23, y + 23, Black);
      fg := White
    end
    else begin
      fg := black
    end;

    if Joy <> Nil then
    begin
      Name := al_get_joystick_button_name (Joy, Button);
      if Length (Name) < 4 then
        al_draw_text (Font, fg, x + 13, y + 8, ALLEGRO_ALIGN_CENTRE, Name)
    end
  end;



  procedure DrawAll (Joy: ALLEGRO_JOYSTICKptr);
  var
    Bmp: ALLEGRO_BITMAPptr;
    Width, Height, i,
    u, v, cx, cy: Integer;
  begin
    Bmp := al_get_target_bitmap;
    Width := al_get_bitmap_width (Bmp);
    Height := al_get_bitmap_height (Bmp);

    al_clear_to_color (al_map_rgb ($FF, $FF, $C0));

    if Joy <> Nil then
      al_draw_textf (
        Font, Black, Width / 2, 10, ALLEGRO_ALIGN_CENTRE,
        'Joystick: %s', [al_get_joystick_name (Joy)]
      );

    for i := 0 to NumSticks - 1 do
    begin
      u := i mod 4;
      v := i div 4;
      cx := Trunc ((u + 0.5) * Width / 4);
      cy := Trunc ((v + 0.5) * Height / 6);
      DrawJoystickAxes (Joy, cx, cy, i)
    end;

    for i := 0 to NumButtons - 1 do
      DrawJoystickButton (Joy, i, JoyButtons[i]);

    al_flip_display
  end;



  procedure MainLoop;
  var
    Event: ALLEGRO_EVENT;
  begin
    while True do
    begin
      if al_is_event_queue_empty (EventQueue) then
	DrawAll (al_get_joystick (0));

      al_wait_for_event (EventQueue, @Event);

      case Event.ftype of
      { ALLEGRO_EVENT_JOYSTICK_AXIS - a joystick axis value changed. }
      ALLEGRO_EVENT_JOYSTICK_AXIS:
	if (Event.joystick.stick < MAX_STICKS)
	AND (Event.joystick.axis < MAX_AXES)
	then
	  Joys[Event.joystick.stick][Event.joystick.axis] := Event.joystick.pos;
      { ALLEGRO_EVENT_JOYSTICK_BUTTON_DOWN - a joystick button was pressed. }
      ALLEGRO_EVENT_JOYSTICK_BUTTON_DOWN:
        JoyButtons[Event.joystick.button] := True;
      { ALLEGRO_EVENT_JOYSTICK_BUTTON_UP - a joystick button was released. }
      ALLEGRO_EVENT_JOYSTICK_BUTTON_UP:
	JoyButtons[Event.joystick.button] := False;
      ALLEGRO_EVENT_KEY_DOWN:
	if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then Exit;
      { ALLEGRO_EVENT_DISPLAY_CLOSE - the window close button was pressed. }
      ALLEGRO_EVENT_DISPLAY_CLOSE:
	Exit;
      ALLEGRO_EVENT_JOYSTICK_CONFIGURATION:
	begin
	  al_reconfigure_joysticks;
	  SetupJoystickValues (al_get_joystick (0))
	end;
       { We received an event of some type we don't know about.
         Just ignore it. }
      else
        ;
      end
    end
  end;



  var
    Display: ALLEGRO_DISPLAYptr;
begin
  if not al_init then AbortExample ('Could not init Allegro.');
  al_init_primitives_addon;
  al_init_font_addon;

  Display := al_create_display (1024, 768);
  if Display = Nil then AbortExample ('Could not create display');

  al_install_keyboard;

  Black := al_map_rgb (0, 0, 0);
  Grey := al_map_rgb ($E0, $E0, $E0);
  White := al_map_rgb (255, 255, 255);
  Font := al_create_builtin_font;

  al_install_joystick;

  EventQueue := al_create_event_queue;
  if EventQueue = Nil then AbortExample ('al_create_event_queue failed');

  if al_get_keyboard_event_source <> Nil then
    al_register_event_source (EventQueue, al_get_keyboard_event_source);
  al_register_event_source (EventQueue, al_get_display_event_source (Display));
  al_register_event_source (EventQueue, al_get_joystick_event_source);

  SetupJoystickValues (al_get_joystick (0));

  MainLoop;

  al_destroy_font (Font)
end.
