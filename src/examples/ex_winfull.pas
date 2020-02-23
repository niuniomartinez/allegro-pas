PROGRAM ex_winfull;
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
  Common, allegro5;

VAR
  Win, Full: ALLEGRO_DISPLAYptr;
  Events: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;

BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  al_install_keyboard;

  IF al_get_num_video_adapters < 2 THEN
    AbortExample ('This example requires multiple video adapters.');

  al_set_new_display_adapter (1);
  al_set_new_display_flags (ALLEGRO_WINDOWED);
  Win := al_create_display (640, 480);
  IF Win = NIL THEN
    AbortExample ('Error creating windowed display on adapter 1 '
	+'(do you have multiple adapters?).');

  al_set_new_display_adapter (0);
  al_set_new_display_flags (ALLEGRO_FULLSCREEN);
  full := al_create_display (640, 480);
  IF Full = NIL THEN
    AbortExample ('Error creating fullscreen display on adapter 0.');

  Events := al_create_event_queue;
  al_register_event_source (Events, al_get_keyboard_event_source);

  WHILE TRUE DO
  BEGIN
    WHILE NOT al_is_event_queue_empty (Events) DO
    BEGIN
      al_get_next_event (Events, Event);
      IF (Event.ftype = ALLEGRO_EVENT_KEY_DOWN)
      AND (Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE)
      THEN BEGIN
	al_destroy_event_queue (Events);

	al_destroy_display (Win);
	al_destroy_display (Full);

	HALT (0)
      END
    END;

    al_set_target_backbuffer (Full);
    al_clear_to_color (al_map_rgb (random (255), random (255), random (255)));
    al_flip_display;

    al_set_target_backbuffer (Win);
    al_clear_to_color (al_map_rgb (random (255), random (255), random (255)));
    al_flip_display;

    al_rest (0.5)
  END;

  al_destroy_event_queue (Events);

  al_destroy_display (Win);
  al_destroy_display (Full)
END.
