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

USES
  Common,
  allegro5;


PROCEDURE TestRelativeTimeout (Queue: ALLEGRO_EVENT_QUEUEptr);
VAR
  Event: ALLEGRO_EVENT;
  Shade: REAL;
BEGIN
  Shade := 0.1;

  WHILE TRUE DO
  BEGIN
    IF al_wait_for_event_timed (Queue, @Event, 0.1) THEN
    BEGIN
      IF Event.ftype = ALLEGRO_EVENT_KEY_DOWN THEN
      BEGIN
        IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
          EXIT
        ELSE
          Shade := 0.1;
      END
    END
    ELSE BEGIN
    { timed out }
      Shade := Shade + 0.1;
      IF Shade > 1 THEN Shade := 1
    END;

    al_clear_to_color (al_map_rgba_f (0.5 * Shade, 0.25 * Shade, Shade, 0));
    al_flip_display
  END
END;



  PROCEDURE TestAbsoluteTimeout (Queue: ALLEGRO_EVENT_QUEUEptr);
  VAR
    Timeout: ALLEGRO_TIMEOUT;
    Event: ALLEGRO_EVENT;
    Shade: REAL;
    Ret: BOOLEAN;
  BEGIN
    Shade := 0.1;

    WHILE TRUE DO
    BEGIN
      al_init_timeout (Timeout, 0.1);
      Ret := al_wait_for_event_until (Queue, @Event, Timeout);
      WHILE Ret DO
      BEGIN
        IF Event.ftype = ALLEGRO_EVENT_KEY_DOWN THEN
        BEGIN
          IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
            EXIT
          ELSE
            Shade := 0.1
        END;
        Ret := al_wait_for_event_until (Queue, @Event, Timeout)
      END;

      IF NOT Ret THEN
      BEGIN
      { timed out }
        Shade := Shade + 0.1;
        IF Shade > 1 THEN Shade := 1
      END;

      al_clear_to_color (al_map_rgba_f (Shade, 0.5 * Shade, 0.25 * Shade, 0));
      al_flip_display
    END
  END;



VAR
  dpy: ALLEGRO_DISPLAYptr;
  Queue: ALLEGRO_EVENT_QUEUEptr;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  al_install_keyboard;

  dpy := al_create_display (640, 480);
  IF dpy = NIL THEN AbortExample ('Unable to set any graphic mode.');

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);

  TestRelativeTimeout (Queue);
  TestAbsoluteTimeout (Queue);
END.
