program ex_inject_events;
(*
 * Ryan Roden-Corrent
 * Example that injects regular (non-user-type) allegro events into a queue.
 * This could be useful for 'faking' certain event sources.
 * For example, you could imitate joystick events without a * joystick.
 *
 * Based on the ex_user_events.c example.
 *)
(*
  Copyright (c) 2012-2019 Guillermo Martínez J.

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
  allegro5;

var
  FakeSrc: ALLEGRO_EVENT_SOURCE;
  Queue: ALLEGRO_EVENT_QUEUEptr;
  FakeKeydownEvent, FakeJoystickEvent, Event: ALLEGRO_EVENT;

begin
  if not al_init then AbortExample ('Could not init Allegro.');

  OpenLog;

{ register our 'fake' event source with the queue }
  al_init_user_event_source (FakeSrc);
  Queue := al_create_event_queue;
  al_register_event_source (Queue, @FakeSrc);

{ fake a joystick event }
  FakeJoystickEvent.any.ftype := ALLEGRO_EVENT_JOYSTICK_AXIS;
  FakeJoystickEvent.joystick.stick := 1;
  FakeJoystickEvent.joystick.axis := 0;
  FakeJoystickEvent.joystick.pos := 0.5;
  al_emit_user_event (@FakeSrc, @FakeJoystickEvent, Nil);

{ fake a keyboard event }
  FakeKeydownEvent.any.ftype := ALLEGRO_EVENT_KEY_DOWN;
  FakeKeydownEvent.keyboard.keycode := ALLEGRO_KEY_ENTER;
  al_emit_user_event (@FakeSrc, @FakeKeydownEvent, Nil);

{ poll for the events we injected }
  while not al_is_event_queue_empty (Queue) do
  begin
    al_wait_for_event (Queue, @Event);

    case Event.ftype of
    ALLEGRO_EVENT_KEY_DOWN:
      LogPrintLn ('Got keydown: %d.', [Event.keyboard.keycode]);
    ALLEGRO_EVENT_JOYSTICK_AXIS:
      LogPrintLn ('Got joystick axis: stick=%d axis=%d pos=%f.',
	[Event.joystick.stick, Event.joystick.axis, Event.joystick.pos]
      );
    end
  end;

  al_destroy_user_event_source (FakeSrc);
  al_destroy_event_queue (Queue);

  LogWriteLn ('Done.');
  CloseLog (True)
end.
