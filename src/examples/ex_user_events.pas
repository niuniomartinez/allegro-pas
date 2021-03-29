PROGRAM ex_user_events;
(*
 *    Example program for the Allegro library.
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
  allegro5, al5base;

type
  MY_EVENTptr = ^MY_EVENT;
(* Just some fantasy event, supposedly used in an RPG - it's just to show that
 * in practice, the 4 user fields we have now never will be enough. *)
  MY_EVENT = record
    id,
    ftype, { For example "attack" or "buy". }
    x, y, z, { Position in the game world the event takes place. }
    server_time, { Game time in ticks the event takes place. }
    source_unit_id, { E.g. attacker or seller. }
    destination_unit_id, { E.g. defender of buyer. }
    item_id, { E.g. weapon used or item sold. }
    amount: Integer; { Gold the item is sold for. }
  end;



  function NewEvent (id: Integer): MY_EVENTptr;
  var
    Event: MY_EVENTptr;
  begin
    Getmem (Event, sizeof (MY_EVENT));
    Event^.id := id;
    Exit (Event)
  end;



  procedure MyEventDtor (Event: ALLEGRO_USER_EVENTptr);
  CDECL; { do not FORGET THIS! }
  var
    lPointer: POINTER;
  begin
    lPointer := Event^.data1.ptr_value;
    LogPrintLn ('MyEventDtor: %p', [lPointer]);
    Freemem (lPointer, sizeof (MY_EVENT))
  end;

var
  MY_SIMPLE_EVENT_type, MY_COMPLEX_EVENT_type: LONGWORD;
  Timer: ALLEGRO_TIMERptr;
  UserSrc: ALLEGRO_EVENT_SOURCE;
  Queue: ALLEGRO_EVENT_QUEUEptr;
  UserEvent, Event: ALLEGRO_EVENT;
  MyEvent: MY_EVENTptr;

begin
  MY_SIMPLE_EVENT_type := ALLEGRO_GET_EVENT_type ('mset');
  MY_COMPLEX_EVENT_type := ALLEGRO_GET_EVENT_type ('mcet');

  if not al_init then AbortExample ('Could not init Allegro.');

  Timer := al_create_timer (0.5);
  if Timer = Nil then AbortExample ('Could not install timer.');

  OpenLog;

  al_init_user_event_source (UserSrc);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, @UserSrc);
  al_register_event_source (Queue, al_get_timer_event_source (Timer));

  al_start_timer (Timer);

  while True do
  begin
    al_wait_for_event (Queue, @Event);

    if Event.ftype = ALLEGRO_EVENT_TIMER then
    begin
      LogPrintLn ('Got timer event %d.', [Event.timer.count]);
      UserEvent.user.ftype := MY_SIMPLE_EVENT_type;
      UserEvent.user.data1.int_value := Event.timer.count;
      al_emit_user_event (@UserSrc, @UserEvent, Nil);

      UserEvent.user.ftype := MY_COMPLEX_EVENT_type;
      UserEvent.user.data1.ptr_value := NewEvent (Event.timer.count);
      al_emit_user_event (@UserSrc, @UserEvent, @MyEventDtor);
    end
    else if Event.ftype = MY_SIMPLE_EVENT_type then
    begin
      al_unref_user_event (@Event.user);

      LogPrintLn
        ('Got simple user event %d.', [Integer (Event.user.data1.int_value)]);
      if Event.user.data1.int_value = 5 then
      begin
        al_destroy_user_event_source (UserSrc);
        al_destroy_event_queue (Queue);
        al_destroy_timer (Timer);

        LogWrite ('Done.');
        CloseLog (True);
        Exit
      end
    end
    else if Event.ftype = MY_COMPLEX_EVENT_type then
    begin
      MyEvent := UserEvent.user.data1.ptr_value;
      LogPrintLn ('Got complex user event %d.', [MyEvent^.id]);
      al_unref_user_event (@Event.user)
    end
  end;

end.
