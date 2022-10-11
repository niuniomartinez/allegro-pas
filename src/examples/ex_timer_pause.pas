program ex_timer_pause;
(* A test of pausing/resuming a timer.
 *
 * 1. Create two 5s long timers.
 * 2. Let each run for 2s, then stop each for 2s.
 * 3. Call al_resume_timer on Timer1 and al_start_timer on Timer2.
 * 4. Wait for timer events
 *
 * Timer1 should finish before Timer2, as it was resumed rather than restarted.
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

const
  Duration = 5.0; { Timer lasts for 5 seconds. }
  PrePause = 2.0; { How long to wait before pausing. }
  Pause = 2.0;    { How long to pause timer for. }

var
  Timer1, Timer2: ALLEGRO_TIMERptr;
  Queue: ALLEGRO_EVENT_QUEUEptr;
  ev: ALLEGRO_EVENT;
  TimerId: Integer;

{ Run our test. }
begin
  Timer1 := Nil;
  Timer2 := Nil;
  Queue  := Nil;

  if not al_init then AbortExample ('Could not init Allegro.');

  OpenLog;

  LogPrintLn ('Creating a pair of %2.2fs timers.', [Duration]);
  Queue := al_create_event_queue;
  Timer1 := al_create_timer (Duration);
  Timer2 := al_create_timer (Duration);
  al_register_event_source (Queue, al_get_timer_event_source (Timer1));
  al_register_event_source (Queue, al_get_timer_event_source (Timer2));

  LogPrintLn ('Starting both timers at: %2.2fs.', [al_get_time * 100]);
  al_start_timer (Timer1);
  al_start_timer (Timer2);
  al_rest (PrePause);

  LogPrintLn ('Pausing timers at: %2.2fs.', [al_get_time * 100]);
  al_stop_timer (Timer1);
  al_stop_timer (Timer2);
  al_rest (Pause);

  LogPrintLn ('Resume  Timer1 at: %2.2fs.', [al_get_time * 100]);
  al_resume_timer (Timer1);

  LogPrintLn ('Restart Timer2 at: %2.2fs.', [al_get_time * 100]);
  al_start_timer (Timer2);

  al_wait_for_event (Queue, @ev);
  if al_get_timer_event_source (Timer1) = ev.any.source then
    TimerId := 1
  else
    TimerId := 2;
  LogPrintLn ('Timer%d finished at: %2.2fs.', [TimerId, al_get_time() * 100]);

  al_wait_for_event (Queue, @ev);
  if al_get_timer_event_source (Timer1) = ev.any.source then
    TimerId := 1
  else
    TimerId := 2;
  LogPrintLn ('Timer%d finished at: %2.2fs.', [TimerId, al_get_time() * 100]);

  al_destroy_event_queue (Queue);
  al_destroy_timer (Timer1);
  al_destroy_timer (Timer2);

  LogWriteLn ('Done');
  CloseLog (True)
end.
