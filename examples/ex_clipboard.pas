PROGRAM ex_clipboard;
(* An example showing bitmap flipping flags, by Steven Wallace.

  NOTE: Previous comment seems misplaced... *)
(*
  Copyright (c) 2012-2018 Guillermo Martínez J.

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
    allegro5, al5base, al5image, al5font;

  CONST
    INTERVAL = 0.1;

  VAR
    Display: ALLEGRO_DISPLAYptr;
    Timer: ALLEGRO_TIMERptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Font: ALLEGRO_FONTptr;
    Text: AL_STRptr;
    Done, Redraw: BOOLEAN;

BEGIN
  Text := NIL;
  Done := FALSE;
  Redraw := TRUE;

  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  IF NOT al_init_image_addon THEN AbortExample ('Failed to init IIO addon.');

  al_init_font_addon;
  InitPlatformSpecific;

  Display := al_create_display (640, 480);
  IF Display = NIL THEN AbortExample ('Error creating display.');


  IF NOT al_install_keyboard THEN AbortExample ('Error installing keyboard.');

  Font := al_load_font ('data/fixed_font.tga', 0, 0);
  IF Font = NIL THEN AbortExample ('Error loading data/fixed_font.tga');

  Timer := al_create_timer (INTERVAL);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_timer_event_source (Timer));
  al_register_event_source (Queue, al_get_display_event_source (Display));

  al_start_timer (Timer);

  al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);

  REPEAT
    IF Redraw AND al_is_event_queue_empty (Queue) THEN
    BEGIN
      IF Text <> NIL THEN al_free (Text);
      IF al_clipboard_has_text (Display) THEN
        Text := al_get_clipboard_text (Display)
      ELSE
        Text := NIL;

      al_clear_to_color (al_map_rgb_f (0, 0, 0));

      IF Text <> NIL THEN
        al_draw_text (Font, al_map_rgba_f (1, 1, 1, 1.0), 0, 0, 0, Text)
      ELSE
        al_draw_text (Font, al_map_rgba_f(1, 0, 0, 1.0), 0, 0, 0,
                      'No clipboard text available.');
      al_flip_display;
      Redraw := FALSE;
    END;

    al_wait_for_event (Queue, Event);
    CASE Event.ftype OF
    ALLEGRO_EVENT_KEY_DOWN:
      IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
        Done := true
      ELSE IF Event.keyboard.keycode = ALLEGRO_KEY_SPACE THEN
        al_set_clipboard_text (Display, 'Copied from Allegro!');
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Done := true;
    ALLEGRO_EVENT_TIMER:
      Redraw := true
    END
  UNTIL Done;

  al_destroy_font (Font);
END.
