program ex_22_user_events;
(* Example of how to create user-type events into a queue. *)
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
    allegro5, al5base;

  const
  (* Initial window size. *)
    wWidth = 800; wHeight = 600;

  type
    PUserEvent = ^UserEvent;
  (* Just some fantasy event, supposedly used in an RPG - it's just to show that
   * in practice, the 4 user fields we have now never will be enough. *)
    UserEvent = record
      Id,
      What, { For example "attack" or "buy". }
      x, y, z, { Position in the game world the event takes place. }
      ServerTime, { Game time in ticks the event takes place. }
      SourceUnitId, { E.g. attacker or seller. }
      DestinationUnitId, { E.g. defender of buyer. }
      ItemId, { E.g. weapon used or item sold. }
      Amount: Integer; { Gold the item is sold for. }
    end;

  var
  (* The event queue. *)
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
  (* Pointer to the window information. *)
    Window: ALLEGRO_DISPLAYptr;
  (* To control program flow. *)
    Terminated: Boolean;
    Timer: ALLEGRO_TIMERptr;
  (* Event management. *)
    Event: ALLEGRO_EVENT;
    SimpleUserEventType, ComplexUserEventType: LongWord;
    UserSource: ALLEGRO_EVENT_SOURCE;
    MyEvent: PUserEvent;


  procedure EmitSimpleUserEvent (const aCount: AL_INT64);
  var
    lEvent: ALLEGRO_EVENT;
  begin
    lEvent.user.ftype := SimpleUserEventType;
    lEvent.user.data1.int_value := aCount;
    al_emit_user_event (@UserSource, @lEvent, Nil)
  end;



  procedure UserEventDestructor (aEvent: ALLEGRO_USER_EVENTptr);
    CDECL; { <--- do not FORGET THIS! }
  var
    lPointer: Pointer;
  begin
    lPointer := aEvent^.data1.ptr_value;
    PrintLn ('UserEventDestructor: %p', [lPointer]);
    Freemem (lPointer, sizeof (PUserEvent))
  end;



  procedure EmitComplexUserEvent (const aCount: AL_INT64);
  var
    lEvent: ALLEGRO_EVENT;
    lUserEvent: PUserEvent;
  begin
    Getmem (lUserEvent, sizeof (PUserEvent));
    lUserEvent^.Id := aCount;
    lEvent.user.ftype := ComplexUserEventType;
    lEvent.user.data1.ptr_value := lUserEvent;
    al_emit_user_event (@UserSource, @lEvent, @UserEventDestructor)
  end;



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
  { Timer. }
    Timer := al_create_timer (0.5);
    if not Assigned (Timer) then
    begin
      ErrorMessage ('Could not install timer.');
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
    al_register_event_source (EventQueue, al_get_timer_event_source (Timer));
  { User events. }
    SimpleUserEventType := ALLEGRO_GET_EVENT_TYPE ('mset');
    ComplexUserEventType := ALLEGRO_GET_EVENT_TYPE ('mcet');
    al_init_user_event_source (@UserSource);
    al_register_event_source (EventQueue, @UserSource);
    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
    alcon.Finalize;
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    al_destroy_user_event_source (@UserSource);
    if Assigned (Timer) then al_destroy_timer (Timer);
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
{ The loop.}
  Terminated := False;
  al_start_timer (Timer);
  repeat
  { Check events. }
    if al_is_event_queue_empty (EventQueue) then UpdateScreen;
    al_wait_for_event (EventQueue, @Event);
  { Use IF instead of CASE because user events mnemonics are variables. }
    if Event.ftype = ALLEGRO_EVENT_TIMER then
    begin
      if Event.timer.count < 6 then
      begin
        PrintLn ('Got timer event %d.', [Event.timer.count]);
        EmitSimpleUserEvent (Event.timer.count);
        EmitComplexUserEvent (Event.timer.count)
      end
    end
    else if Event.ftype = ALLEGRO_EVENT_DISPLAY_CLOSE then
      Terminated := True
    else if Event.ftype = ALLEGRO_EVENT_KEY_DOWN then
    begin
      PrintLn ('Got keydown: %d.', [Event.keyboard.keycode]);
      case Event.keyboard.keycode of
      ALLEGRO_KEY_ESCAPE:
        Terminated := True;
      ALLEGRO_KEY_C:
        EmitComplexUserEvent (Event.keyboard.keycode);
      ALLEGRO_KEY_S:
        EmitSimpleUserEvent (Event.keyboard.keycode);
      end
    end
    else if Event.ftype = SimpleUserEventType then
    begin
      al_unref_user_event (@Event.user);
      PrintLn (
        'Got simple user event %d.',
        [Integer (Event.user.data1.int_value)]
      )
    end
    else if Event.ftype = ComplexUserEventType then
    begin
    { Get user event data. }
      MyEvent := Event.user.data1.ptr_value;
      PrintLn ('Got complex user event %d.', [MyEvent^.id]);
      al_unref_user_event (@Event.user)
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
