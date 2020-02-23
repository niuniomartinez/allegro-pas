PROGRAM ex_noframe;
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
  allegro5, al5image;

VAR
  Display: ALLEGRO_DISPLAYptr;
  Bitmap: ALLEGRO_BITMAPptr;
  Events: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Down: BOOLEAN;
  DownX, DownY, cx, cy: LONGINT;
  Timer: ALLEGRO_TIMERptr;

BEGIN
  Down := FALSE;
  DownX := 0; DownY := 0;
  cx := 0; cy := 0;

  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  al_install_mouse;
  al_install_keyboard;
  al_init_image_addon;
  InitPlatformSpecific;

  al_set_new_display_flags (ALLEGRO_FRAMELESS);
  Display := al_create_display (300, 200);
  IF Display = NIL THEN AbortExample ('Error creating display.');

  Bitmap := al_load_bitmap ('data/fakeamp.bmp');
  IF Bitmap = NIL THEN AbortExample ('Error loading fakeamp.bmp.');

  Timer := al_create_timer (1 / 30);

  Events := al_create_event_queue;
  al_register_event_source (Events, al_get_mouse_event_source);
  al_register_event_source (Events, al_get_keyboard_event_source);
  al_register_event_source (Events, al_get_display_event_source (Display));
  al_register_event_source (Events, al_get_timer_event_source (Timer));

  al_start_timer (Timer);

  WHILE TRUE DO
  BEGIN
    al_wait_for_event (Events, @Event);
    CASE Event.ftype OF
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
      BEGIN
	IF (Event.mouse.button = 1) AND (Event.mouse.x <> 0) THEN
	BEGIN
	  Down := TRUE;
	  DownX := Event.mouse.x;
	  DownY := Event.mouse.y
	END;
	IF Event.mouse.button = 2 THEN
	  al_set_display_flag (
	    Display, ALLEGRO_FRAMELESS,
	    NOT ((al_get_display_flags (Display) AND ALLEGRO_FRAMELESS) <> 0)
	  )
      END;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      BREAK;
    ALLEGRO_EVENT_MOUSE_BUTTON_UP:
      IF Event.mouse.button = 1 THEN Down := FALSE;
    ALLEGRO_EVENT_MOUSE_AXES:
      IF Down THEN
	IF al_get_mouse_cursor_position (cx, cy) THEN
	  al_set_window_position (Display, cx - DownX, cy - DownY);
    ALLEGRO_EVENT_KEY_DOWN:
      IF event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN BREAK;
    ALLEGRO_EVENT_TIMER:
      BEGIN
	al_draw_bitmap (Bitmap, 0, 0, 0);
	al_flip_display
      END;
    END
  END;

  al_destroy_timer (Timer);
  al_destroy_event_queue (Events);
  al_destroy_display (Display)
END.
