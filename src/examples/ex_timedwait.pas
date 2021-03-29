PROGRAM ex_timedwait;
(*
 *    Example program for the Allegro library, by Peter Wang.
 *
 *    Test timed version of al_wait_for_event().
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
  allegro5;


procedure TestRelativeTimeout (Queue: ALLEGRO_EVENT_QUEUEptr);
var
  Event: ALLEGRO_EVENT;
  Shade: REAL;
begin
  Shade := 0.1;

  while True do
  begin
    if al_wait_for_event_timed (Queue, @Event, 0.1) then
    begin
      if Event.ftype = ALLEGRO_EVENT_KEY_DOWN then
      begin
        if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
          Exit
        else
          Shade := 0.1;
      end
    end
    else begin
    { timed out }
      Shade := Shade + 0.1;
      if Shade > 1 then Shade := 1
    end;

    al_clear_to_color (al_map_rgba_f (0.5 * Shade, 0.25 * Shade, Shade, 0));
    al_flip_display
  end
end;



  procedure TestAbsoluteTimeout (Queue: ALLEGRO_EVENT_QUEUEptr);
  var
    Timeout: ALLEGRO_TIMEOUT;
    Event: ALLEGRO_EVENT;
    Shade: REAL;
    Ret: Boolean;
  begin
    Shade := 0.1;

    while True do
    begin
      al_init_timeout (Timeout, 0.1);
      Ret := al_wait_for_event_until (Queue, @Event, Timeout);
      while Ret do
      begin
        if Event.ftype = ALLEGRO_EVENT_KEY_DOWN then
        begin
          if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
            Exit
          else
            Shade := 0.1
        end;
        Ret := al_wait_for_event_until (Queue, @Event, Timeout)
      end;

      if not Ret then
      begin
      { timed out }
        Shade := Shade + 0.1;
        if Shade > 1 then Shade := 1
      end;

      al_clear_to_color (al_map_rgba_f (Shade, 0.5 * Shade, 0.25 * Shade, 0));
      al_flip_display
    end
  end;



var
  dpy: ALLEGRO_DISPLAYptr;
  Queue: ALLEGRO_EVENT_QUEUEptr;
begin
  if not al_init then AbortExample ('Could not init Allegro.');

  al_install_keyboard;

  dpy := al_create_display (640, 480);
  if dpy = Nil then AbortExample ('Unable to set any graphic mode.');

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);

  TestRelativeTimeout (Queue);
  TestAbsoluteTimeout (Queue);
end.
