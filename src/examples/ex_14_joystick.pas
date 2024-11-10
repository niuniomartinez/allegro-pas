program ex_14_joystick;
(* Shows how to get input from joysticks and gamepads.  Note it uses the first
 * joystick only.
 *
 * From an example program by Peter Wang.
 *)
(*
  Copyright (c) 2012-2023 Guillermo Martínez J.

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
    allegro5, al5font, al5primitives, al5strings;

  const
  (* Window size. *)
    wWidth = 800; wHeight = 600;
    MaxAxes    = 3;
    MaxSticks = 16;
    MaxButtons = 32;
  var
  (* The event queue. *)
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
  (* An event. *)
    Event: ALLEGRO_EVENT;
  (* Pointer to the window information. *)
    Window: ALLEGRO_DISPLAYptr;
  (* To store a text font. *)
    TextFont: ALLEGRO_FONTptr;
  (* To know when to stop. *)
    Terminated: Boolean;
  (* Colors. *)
    clrBlack, clrGrey, clrWhite,
    clrBackground: ALLEGRO_COLOR;
  (* Joystick information. *)
    Joystick: ALLEGRO_JOYSTICKptr;
    NumSticks, NumButtons: Integer;
    NumAxes: array [0 .. MaxSticks - 1] of Integer;
    Joys: array [0 .. MaxSticks - 1] of array [0 .. MaxAxes] of Single;
    JoyButtons: array [0 .. MaxButtons - 1] of Boolean;

(* Get information from joystick and updates the joystick information. *)
  procedure SetupJoystickValues;
  var
    lJoystickState: ALLEGRO_JOYSTICK_STATE;
    lNdx, lNdx2: Integer;
  begin
  { Get joystick state. }
    al_get_joystick_state (Joystick, lJoystickState);
  { Get sticks state. }
    NumSticks := al_get_joystick_num_sticks (Joystick);
    if NumSticks > MaxSticks then NumSticks := MaxSticks;
    for lNdx := 0 to NumSticks - 1 do
    begin
      NumAxes[lNdx] := al_get_joystick_num_axes (Joystick, lNdx);
      for lNdx2 := 0 to NumAxes[lNdx] - 1 do
        Joys[lNdx][lNdx2] := lJoystickState.stick[lNdx].axis[lNdx2]
    end;
  { Get button information. }
    NumButtons := al_get_joystick_num_buttons (Joystick);
    if NumButtons > MaxButtons then NumButtons := MaxButtons;
    for lNdx := 0 to NumButtons - 1 do
      JoyButtons[lNdx] := lJoystickState.button[lNdx] >= 16384
  end;


(* Program initialization. *)
  function Initialize: Boolean;

  (* Initializes the joystick stuff. *)
    function InitJoystick: Boolean;
    var
      lNdx, lNdx2: Integer;
    begin
    { Init Allegro's joystick support. }
      if not al_install_joystick then
      begin
        ErrorMessage ('Can''t initialize joystics.  Check drivers.');
        Exit (False)
      end;
    { Set initial values. }
      Joystick := al_get_joystick (0);
      if not Assigned (Joystick) then
      begin
        ErrorMessage ('Can''t find joystick.');
        Exit (False)
      end;
      NumSticks := 0;
      NumButtons := 0;
      for lNdx := Low (NumAxes) to High (NumAxes) do
        NumAxes[lNdx] := 0;
      for lNdx := Low (Joys) to High (Joys) do
        for lNdx2 := Low (Joys[lNdx]) to High (Joys[lNdx]) do
          Joys[lNdx][lNdx2] := 0;
      for lNdx := Low (JoyButtons) to High (JoyButtons) do
        JoyButtons[lNdx] := False;
    { Et al... }
      SetupJoystickValues;

      Result := True
    end;

  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_init_font_addon
    or not al_init_primitives_addon
    then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;
  { Create window. }
    al_set_new_display_flags (ALLEGRO_WINDOWED);
    Window := al_create_display (wWidth, wHeight);
    if not Assigned (Window) then
    begin
      WriteLn ('Can''t create window.');
      Exit (False)
    end;
  { Initialize joysticks. }
    if not InitJoystick then Exit (False);
  { Build colors. }
    clrGrey := al_map_rgb ($E0, $E0, $E0);
    clrBlack := al_map_rgb (0, 0, 0);
    clrWhite := al_map_rgb ($FF, $FF, $FF);
    clrBackground := al_map_rgb ($FF, $FF, $C0);
  { Load text font. }
    TextFont := al_create_builtin_font;
    if not Assigned (TextFont) then
    begin
      ErrorMessage ('Error creating text font.');
      Exit (False)
    end;
  { Create the event queue. }
    EventQueue := al_create_event_queue;
    if not Assigned (EventQueue) then
    begin
      ErrorMessage ('Can''t initialize event queue!');
      Exit (False)
    end;
    al_register_event_source (EventQueue, al_get_keyboard_event_source);
    al_register_event_source (EventQueue, al_get_display_event_source (Window));
    al_register_event_source (EventQueue, al_get_joystick_event_source);

    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (TextFont) then al_destroy_font (TextFont);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Draw window content. *)
  procedure UpdateScreen;

  (* Helper to draw joystick axes.

     Coordinates are center position.
   *)
    procedure DrawAxes (
      aX, aY, aStick: Integer
    );
    const
      Size = 30;
      cSize = 5;
      oSize = Size + cSize;
    var
      lzx, lx, ly, lz, lNdx: Integer;
    begin
    { Get screen position. }
      lzx := ax + oSize + cSize * 2;
      lx := Trunc (ax + Joys[aStick][0] * Size);
      ly := Trunc (ay + Joys[aStick][1] * Size);
      lz := Trunc (ay + Joys[aStick][2] * Size);
    { Draw. }
      al_draw_filled_rectangle (
        ax - oSize, ay - oSize,
        ax + oSize, ay + oSize,
        clrGrey
      );
      al_draw_rectangle (
        ax - oSize + 0.5, ay - oSize + 0.5,
        ax + oSize - 0.5, ay + oSize - 0.5,
        clrBlack, 0
      );
    { Axis tip. }
      al_draw_filled_rectangle (lx - 5, ly - 5, lx + 5, ly + 5, clrBlack);
    { 3D axis (if available). }
      if NumAxes[astick] >= 3 then
      begin
        al_draw_filled_rectangle (
          lzx - cSize, ay - oSize,
          lzx + cSize, ay + oSize,
          clrGrey
        );
        al_draw_rectangle (
          lzx - cSize + 0.5, ay - oSize + 0.5,
          lzx + cSize - 0.5, ay + oSize - 0.5,
          clrBlack, 0
        );
        al_draw_filled_rectangle (lzx - 5, lz - 5, lzx + 5, lz + 5, clrBlack)
      end;
    { Joystick and axis names. }
      al_draw_text (
        TextFont, clrBlack,
        ax, ay + oSize + 1, ALLEGRO_ALIGN_CENTRE,
        al_get_joystick_stick_name (Joystick, aStick)
      );
      for lNdx := 0 to NumAxes[aStick] - 1 do
      begin
        al_draw_text (
          TextFont, clrBlack,
          ax, ay + oSize + (1 + lNdx) * 10,
          ALLEGRO_ALIGN_CENTRE,
          al_get_joystick_axis_name (Joystick, aStick, lNdx)
        )
      end
    end;

  (* Helper to draw joystick buttons. *)
    procedure DrawButton (
      aButton: Integer;
      aPressed: Boolean
    );
    var
      lx, ly: Integer;
      lColor: ALLEGRO_COLOR;
      lName: String;
    begin
    { Button position. }
      lx := wWidth div 2 - 120 + (aButton mod 8) * 30;
      ly := wHeight - 120 + (aButton div 8) * 30;
    { Draw. }
      al_draw_filled_rectangle (lx, ly, lx + 25, ly + 25, clrGrey);
      al_draw_rectangle (lx + 0.5, ly + 0.5, lx + 24.5, ly + 24.5, clrBlack, 0);
      if aPressed then
      begin
        al_draw_filled_rectangle (lx + 2, ly + 2, lx + 23, ly + 23, clrBlack);
        lColor := clrWhite
      end
      else
        lColor := clrBlack;
    { Joystick name. }
      lName := al_str_to_string (
        al_get_joystick_button_name (Joystick, aButton)
      );
      al_draw_text (
        TextFont, lColor,
        lx + 13, ly + 8, ALLEGRO_ALIGN_CENTRE,
        al_string_to_str (lName)
      );
    end;

  var
    lNdx: Integer;
  begin
    al_clear_to_color (clrBackground);
  { Joystick name. }
    al_draw_textf (
      TextFont, clrBlack,
      wWidth / 2, 0, ALLEGRO_ALIGN_CENTRE,
      'Joystick: %s',
      [al_get_joystick_name (Joystick)]
    );
  { Draw axes. }
    for lNdx := 0 to NumSticks - 1 do
      DrawAxes (
        Trunc ((lNdx mod 4 + 0.5) * wWidth / 4),
        Trunc ((lndx div 4 + 0.5) * wHeight / 6),
        lNdx
      );
  { Draw buttons. }
    for lNdx := 0 to NumButtons - 1 do DrawButton (lNdx, JoyButtons[lNdx]);

    al_flip_display
  end;

begin
{ Program initialization. }
  if not Initialize then Exit;
  Terminated := False;
  repeat
    if al_is_event_queue_empty (EventQueue) then UpdateScreen;
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
  { ALLEGRO_EVENT_DISPLAY_CLOSE - the window close button was pressed. }
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
  { ALLEGRO_EVENT_KEY_DOWN - a keyboard key was pressed. }
    ALLEGRO_EVENT_KEY_DOWN:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
        Terminated := True;
    { ALLEGRO_EVENT_JOYSTICK_AXIS - a joystick axis value changed. }
    ALLEGRO_EVENT_JOYSTICK_AXIS:
      if (Event.joystick.stick < MaxSticks)
      and (Event.joystick.axis < MaxAxes)
      then
        Joys[Event.joystick.stick][Event.joystick.axis] := Event.joystick.pos;
    { ALLEGRO_EVENT_JOYSTICK_BUTTON_DOWN - a joystick button was pressed. }
    ALLEGRO_EVENT_JOYSTICK_BUTTON_DOWN:
      JoyButtons[Event.joystick.button] := True;
    { ALLEGRO_EVENT_JOYSTICK_BUTTON_UP - a joystick button was released. }
    ALLEGRO_EVENT_JOYSTICK_BUTTON_UP:
      JoyButtons[Event.joystick.button] := False;
    { ALLEGRO_EVENT_JOYSTICK_CONFIGURATION - joystick configuration changed. }
    ALLEGRO_EVENT_JOYSTICK_CONFIGURATION:
      begin
        al_reconfigure_joysticks;
        SetupJoystickValues
      end;
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
