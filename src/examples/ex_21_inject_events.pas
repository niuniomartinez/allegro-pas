program ex_21_inject_events;
(* Example that injects regular (non-user-type) allegro events into a queue.
 * This could be useful for 'faking' certain event sources.
 * For example, you could imitate joystick events without a joystick.
 *
 * Partially from Ryan Roden-Corrent's and the ex_user_events.c examples.
 *)
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
    Common, alcon,
    allegro5;

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
  (* Event management. *)
    Event: ALLEGRO_EVENT;
    FakeSource: ALLEGRO_EVENT_SOURCE;

(* Program initialization. *)
  function Initialize: Boolean;
  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard then
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
  { Create and register our 'fake' event source with the Queue. }
    al_init_user_event_source (@FakeSource);
    al_register_event_source (EventQueue, @FakeSource);
    Result := True
  end;



(* Fake a joystick event. *)
  procedure EmitJoystickEvent;
  var
    lEvent: ALLEGRO_EVENT;
  begin
    lEvent.any.ftype := ALLEGRO_EVENT_JOYSTICK_AXIS;
    lEvent.joystick.stick := 1;
    lEvent.joystick.axis := 0;
    lEvent.joystick.pos := 0.5;
    al_emit_user_event (@FakeSource, @lEvent, Nil)
  end;



(* Fake a keyboard event. *)
  procedure EmitKeyboardEvent;
  var
    lEvent: ALLEGRO_EVENT;
  begin
    lEvent.any.ftype := ALLEGRO_EVENT_KEY_DOWN;
    lEvent.keyboard.keycode := ALLEGRO_KEY_ENTER;
    al_emit_user_event (@FakeSource, @lEvent, Nil)
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
    alcon.Finalize;
    al_destroy_user_event_source (@FakeSource);
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (Window) then al_destroy_display (Window)
  end;



  procedure UpdateScreen;
  begin
    alcon.DrawConsole;
    al_flip_display
  end;

begin
{ Program initialization. }
  if not Initialize then Exit;
  Terminated := False;
{ Emit fake events. }
  EmitKeyboardEvent;
  EmitJoystickEvent;
{ The loop. }
  repeat
  { Check events. }
    if al_is_event_queue_empty (EventQueue) then UpdateScreen;
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_KEY_DOWN:
      begin
        if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
          Terminated := True;
        PrintLn ('Got keydown: %d.', [Event.keyboard.keycode]);
      end;
    ALLEGRO_EVENT_JOYSTICK_AXIS:
      PrintLn (
        'Got joystick axis: stick=%d axis=%d pos=%f.',
        [Event.joystick.stick, Event.joystick.axis, Event.joystick.pos]
      );
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
