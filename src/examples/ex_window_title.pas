PROGRAM ex_window_title;
(* An example showing how to set the title of a window, by Beoran. *)
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
  allegro5, al5base, al5image, al5font, al5strings;

CONST
  INTERVAL = 1.0;
  NEW_WINDOW_TITLE = 'A Custom Window Title. Press space to start changing it.';

VAR
  Step: INTEGER;
  Display: ALLEGRO_DISPLAYptr;
  Timer: ALLEGRO_TIMERptr;
  Queue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Font: ALLEGRO_FONTptr;
  Title, TextStr: AL_STR;
  Done, Redraw: BOOLEAN;

BEGIN
  Step := 0;
  Title := '';
  Done := FALSE;
  Redraw := TRUE;

  IF NOT al_init THEN AbortExample ('Could not init Allegro.');
  IF NOT al_init_image_addon THEN AbortExample ('Could not init IIO addon.');
  al_init_font_addon;
  InitPlatformSpecific;

  TextStr := NEW_WINDOW_TITLE;
  al_set_new_window_title (NEW_WINDOW_TITLE);

  Display := al_create_display (640, 480);
  IF Display = NIL THEN AbortExample ('Error creating display.');

  IF NOT al_install_keyboard THEN AbortExample ('Error installing keyboard.');

  Font := al_load_font ('data/fixed_font.tga', 0, 0);
  IF Font = NIL THEN AbortExample ('Error loading data/fixed_font.tga.');

  TextStr := al_get_new_window_title;

  Timer := al_create_timer (INTERVAL);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_timer_event_source (Timer));
  al_register_event_source (Queue, al_get_display_event_source (Display));

  REPEAT
    IF Redraw AND al_is_event_queue_empty (Queue) THEN
    BEGIN
      al_clear_to_color (al_map_rgb_f (0, 0, 0));
      al_draw_text (Font, al_map_rgba_f (1, 1, 1, 0.5), 0, 0, 0, TextStr);
      al_flip_display;
      Redraw := FALSE
    END;

    al_wait_for_event (Queue, @Event);
    CASE Event.ftype OF
    ALLEGRO_EVENT_KEY_DOWN:
      IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
        Done := TRUE
      ELSE IF Event.keyboard.keycode = ALLEGRO_KEY_SPACE THEN
        al_start_timer (Timer);
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Done := TRUE;
    ALLEGRO_EVENT_TIMER:
      BEGIN
        Redraw := TRUE;
        INC (Step);
        Title := al_str_format ('Title: %d', [Step]);
        TextStr := Title;
        al_set_window_title (Display, Title)
      END;
    END
  UNTIL Done
END.
