PROGRAM ex_resize;
(* Display a pulsating window until a key or the closebutton is pressed. *)
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

USES
  Common,
  allegro5, al5primitives;



  PROCEDURE Redraw;
  VAR
    Black, White: ALLEGRO_COLOR;
    w, h: INTEGER;
  BEGIN
    White := al_map_rgba_f (1, 1, 1, 1);
    Black := al_map_rgba_f (0, 0, 0, 1);

    al_clear_to_color (White);
    w := al_get_bitmap_width (al_get_target_bitmap);
    h := al_get_bitmap_height (al_get_target_bitmap);
    al_draw_line (0, h, w / 2, 0, Black, 0);
    al_draw_line (w / 2, 0, w, h, Black, 0);
    al_draw_line (w / 4, h / 2, 3 * w / 4, h / 2, Black, 0);
    al_flip_display
  END;



VAR
  Display: ALLEGRO_DISPLAYptr;
  Timer: ALLEGRO_TIMERptr;
  Events: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  rs, s: INTEGER;
  Resize: BOOLEAN;
BEGIN
  rs := 100;
  Resize := FALSE;

{ Initialize Allegro and create an event queue. }
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');
  al_init_primitives_addon;
  Events := al_create_event_queue;

{ Setup a display driver and register events from it. }
  al_set_new_display_flags (ALLEGRO_RESIZABLE);
  Display := al_create_display (rs, rs);
  IF Display = NIL THEN AbortExample ('Could not create display.');
  al_register_event_source (Events, al_get_display_event_source (Display));

  Timer := al_create_timer (0.1);
  al_start_timer (Timer);

{ Setup a keyboard driver and register events from it. }
  al_install_keyboard;
  al_register_event_source (Events, al_get_keyboard_event_source);
  al_register_event_source (Events, al_get_timer_event_source (Timer));

{ Display a pulsating window until a key or the closebutton is pressed. }
  Redraw;
  WHILE TRUE DO
  BEGIN
    IF Resize THEN
    BEGIN
      INC (rs, 10);
      IF rs = 300 THEN rs := 100;
      s := rs;
      IF s > 200 THEN s := 400 - s;
      al_resize_display (Display, s, s);
      Redraw;
      Resize := FALSE;
    END;
    al_wait_for_event (Events, @Event);
    CASE Event.ftype OF
    ALLEGRO_EVENT_TIMER:
      Resize := TRUE;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EXIT;
    ALLEGRO_EVENT_KEY_DOWN:
      EXIT;
    END
  END
END.
