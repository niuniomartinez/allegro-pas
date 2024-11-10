program ex_09_keyboard;
(* Shows how to get keyboard events.

   Partially from an example by Peter Wang and Ryan Dickie.
 *)
(*
  Copyright (c) 2024 Guillermo Martínez J.

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
    Common, alcon,
    allegro5, al5font, al5strings;

  const
  (* Initial window size. *)
    wWidth = 800; wHeight = 600;

  var
  (* The event queue. *)
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
  (* Pointer to the window information. *)
    Window: ALLEGRO_DISPLAYptr;
  (* To control program flow. *)
    Terminated: Boolean;
  (* An event. *)
    Event: ALLEGRO_EVENT;
    EventName: String;

(* Program initialization. *)
  function Initialize: Boolean;
  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_init_font_addon
    then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;
  { Create window. }
    Window := al_create_display (wWidth, wHeight);
    if not Assigned (Window) then
    begin
      WriteLn ('Can''t create window.');
      Exit (False)
    end;
    if not alcon.Initialize then Exit (False);
  { Create the event queue. }
    EventQueue := al_create_event_queue;
    if not Assigned (EventQueue) then
    begin
      ErrorMessage ('Can''t initialize event queue!');
      Exit (False)
    end;
    al_register_event_source (EventQueue, al_get_keyboard_event_source);
    al_register_event_source (EventQueue, al_get_display_event_source (Window));

    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
    alcon.Finalize;
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Helper to log key events. *)
  procedure LogKey (
    const aEvent: String;
    aKeycode, aUnichar, aModifiers: Integer
  );
  var
    lKeyname: String;
    lCharacter: AnsiString;
  begin
    lKeyName := al_keycode_to_name (aKeyCode);
    if aUnichar < 32 then
      lCharacter := ' '
    else if aUnichar < 128 then
      lCharacter := Char (aUnichar)
    else
    { I didn't found a way to convert the UNICODE code point in to a valid
      character that can be rendered by Allegro.  Even the Allegro's
      al_utf8_encode seems to be broken wen called from Pascal
      (EAccessViolation).
    }
      lCharacter := '^';
    PrintLn (
      '%-8s  code=%03d, char=''%s'' (%4d), modifiers=%08x, [%s]',
      [aEvent, aKeycode, lCharacter, aUnichar, aModifiers, lKeyName]
    );
  end;



  procedure UpdateScreen;
  begin
    alcon.DrawConsole;
    al_flip_display
  end;

begin
{ Program initialization. }
  if not Initialize then Exit;
  PrintLn ('Focus on the window and press keys to see events.');
  PrintLn ('Escape quits.');
  PrintLn ('');
{ "Game loop". }
  Terminated := False;
  repeat
  { Check events. }
    if al_is_event_queue_empty (EventQueue) then UpdateScreen;
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_KEY_DOWN:
    { ALLEGRO_EVENT_KEY_DOWN - a keyboard key was pressed. }
      begin
        if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
          Terminated := True;
        LogKey ('KEY_DOWN', Event.keyboard.keycode, 0, 0);
      end;
    ALLEGRO_EVENT_KEY_UP:
    { ALLEGRO_EVENT_KEY_UP - a keyboard key was released. }
      LogKey ('KEY_UP', Event.keyboard.keycode, 0, 0);
    ALLEGRO_EVENT_KEY_CHAR:
    { ALLEGRO_EVENT_KEY_CHAR - a character was typed or repeated. }
      begin
      { For some reason, Allegro sets frepeat to True always, except in the
        first ALLEGRO_EVENT_KEY_CHAR.
        if Event.keyboard.frepeat then
          EventName := 'repeat'
        else
      }
          EventName := 'KEY_CHAR';
        LogKey (EventName,
          Event.keyboard.keycode,
          Event.keyboard.unichar,
          Event.keyboard.modifiers
        )
      end;
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
