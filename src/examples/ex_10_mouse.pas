program ex_10_mouse;
(* Shows mouse events. *)
(*
  Copyright (c) 2012-2024 Guillermo Martínez J.

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
    allegro5, al5font, al5image, al5primitives, al5strings;

  const
  (* Window size. *)
    wWidth = 640; wHeight = 480;
  (* How many buttons to manage. *)
    MaxButtons = 5;

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
  (* Mouse stuff. *)
    Cursor: ALLEGRO_BITMAPptr;
    NumButtons: Integer;
    MouseState: record
      X, Y, Z, W: integer;
      mX, mY, mZ, mW: Integer;
      Buttons: array [1..MaxButtons] of Boolean;
      Pressure: Real;
      InWindow: Boolean
    end;
  (* Colors to use. *)
    clrGrey, ClrBlack, clrBackground: ALLEGRO_COLOR;

(* Program initialization. *)
  function Initialize: Boolean;
  var
    lCount: Integer;
  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_init_font_addon
    or not al_init_image_addon or not al_init_primitives_addon
    then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;
  { Create window. }
    al_set_new_display_flags (ALLEGRO_WINDOWED or ALLEGRO_RESIZABLE);
    Window := al_create_display (wWidth, wHeight);
    if not Assigned (Window) then
    begin
      WriteLn ('Can''t create window.');
      Exit (False)
    end;
  { Build colors. }
    clrGrey := al_map_rgb ($E0, $E0, $E0);
    clrBlack := al_map_rgb (0, 0, 0);
    clrBackground := al_map_rgb ($FF, $FF, $C0);
  { Load text font. }
    TextFont := al_create_builtin_font;
    if not Assigned (TextFont) then
    begin
      ErrorMessage ('Error creating text font.');
      Exit (False)
    end;
  { Initialize mouse. }
    if not al_install_mouse then
    begin
      ErrorMessage ('Can''t initialize Mouse!');
      Exit (False)
    end;
    NumButtons := al_get_mouse_num_buttons;
    if NumButtons > MaxButtons then NumButtons := MaxButtons;
    for lCount := 1 to MaxButtons do
      MouseState.Buttons[lCount] := False;
  { Load mouse cursor. }
    Cursor := al_load_bitmap ('data/cursor.tga');
    if not Assigned (Cursor) then
    begin
      ErrorMessage ('Error loading cursor.tga');
      Exit (False)
    end;
  { Hide cursor to use our one. }
    al_hide_mouse_cursor (Window);
  { Create the event queue. }
    EventQueue := al_create_event_queue;
    if not Assigned (EventQueue) then
    begin
      ErrorMessage ('Can''t initialize event queue!');
      Exit (False)
    end;
    al_register_event_source (EventQueue, al_get_keyboard_event_source);
    al_register_event_source (EventQueue, al_get_mouse_event_source);
    al_register_event_source (EventQueue, al_get_display_event_source (Window));

    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (TextFont) then al_destroy_font (TextFont);
    if Assigned (Cursor) then al_destroy_bitmap (Cursor);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Draw window content. *)
  procedure UpdateScreen;

  (* Draw a single button. *)
    procedure DrawMouseButton (aButton: Integer; aDown: Boolean);
    const
      OffsetX: array [1..MaxButtons] of Integer = (0, 70, 35, 105, 140);
      PosY = 130;
    var
      lx, ly: Integer;
    begin
      lx := 400 + OffsetX[aButton];
      ly := PosY;

      al_draw_filled_rectangle (lx, ly, lx + 27, ly + 42, clrGrey);
      al_draw_rectangle (lx + 0.5, ly + 0.5, lx + 26.5, ly + 41.5, clrBlack, 0);
      if aDown then
        al_draw_filled_rectangle (lx + 2, ly + 2, lx + 25, ly + 40, clrBlack)
    end;

    procedure PrintData (
      const ay: Integer;
      const aFmt: AnsiString;
      aVar: array of const
    );
    begin
      al_draw_text (
        TextFont, clrBlack,
        5, ay, 0,
        al_str_format (al_string_to_str (aFmt), aVar)
      )
    end;

  var
    lCount: Integer;
    lInOut: AnsiString;
  begin
    al_clear_to_color (clrBackground);
    if MouseState.InWindow then lInOut := 'in' else lInOut := 'out';
  { Show mouse button. }
    for lCount := 1 to NumButtons do
      DrawMouseButton (lCount, MouseState.Buttons[lCount]);
  { Print mouse data. }
    PrintData (
      5,
      'Mickeys:  {%d, %d, %d, %d }',
      [MouseState.mX, MouseState.mY, MouseState.mZ, MouseState.mW]
    );
    PrintData (
      25,
      'Position:  {%d, %d, %d, %d }',
      [MouseState.X, MouseState.Y, MouseState.Z, MouseState.W]
    );
    PrintData (45, 'Pressure: %f', [MouseState.Pressure]);
    PrintData (65, 'Window: %s', [lInOut]);
  { Draw our mouse cursor. }
    if MouseState.InWindow then
      al_draw_bitmap (Cursor, MouseState.X, MouseState.Y, 0);
  { Restore mickeys. }
    MouseState.mX := 0; MouseState.mY := 0; MouseState.mZ := 0;

    al_flip_display
  end;

begin
  if not Initialize then Exit;
  Terminated := False;
  repeat
    if al_is_event_queue_empty (EventQueue) then UpdateScreen;
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      al_acknowledge_resize (Event.display.source);
    ALLEGRO_EVENT_KEY_DOWN:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
        Terminated := True;
    ALLEGRO_EVENT_MOUSE_AXES:
      begin
        MouseState.x := Event.mouse.x;
        MouseState.y := Event.mouse.y;
        MouseState.z := Event.mouse.z;
        MouseState.w := Event.mouse.w;
        MouseState.mx := Event.mouse.dx;
        MouseState.my := Event.mouse.dy;
        MouseState.mz := Event.mouse.dz;
        MouseState.mw := Event.mouse.dw;
        MouseState.Pressure := Event.mouse.pressure
      end;
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
      begin
        if Event.mouse.button <= NumButtons then
          MouseState.Buttons[Event.mouse.button] := True;
        MouseState.Pressure := Event.mouse.pressure
      end;
    ALLEGRO_EVENT_MOUSE_BUTTON_UP:
      begin
        if Event.mouse.button <= NumButtons then
          MouseState.Buttons[Event.mouse.button] := False;
        MouseState.Pressure := Event.mouse.pressure
      end;
    ALLEGRO_EVENT_MOUSE_ENTER_DISPLAY:
      MouseState.InWindow := Event.mouse.display = Window;
    ALLEGRO_EVENT_MOUSE_LEAVE_DISPLAY:
      MouseState.InWindow := False;
    end
  until Terminated;
  Finalize
end.
