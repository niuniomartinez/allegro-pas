PROGRAM ex_bitmap;
(*
 * This example displays a picture on the screen, with support for
 * command-line parameters, multi-screen, screen-orientation and
 * zooming.
 *)
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
  Allegro5, al5image,
  sysutils;
VAR
  FileName: STRING;
  Bitmap: ALLEGRO_BITMAPptr;
  Timer: ALLEGRO_TIMERptr;
  Display: ALLEGRO_DISPLAYptr;
  EventQueue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Redraw, EndLoop: BOOLEAN;
  Zoom, T0, T1: DOUBLE;
BEGIN
{ The first commandline argument can optionally specify an
  image to display instead of the default. Allegro's image
  addon suports BMP, DDS, PCX, TGA and can be compiled with
  PNG and JPG support on all platforms. Additional formats
  are supported by platform specific libraries and support for
  image formats can also be added at runtime. }
  IF ParamCount > 0 THEN
    FileName := ParamStr (1)
  ELSE
    FileName := 'data/mysha.pcx';

  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

{ Initializes and displays a log window for debugging purposes. }
  OpenLog;

{ The second parameter to the process can optionally specify what
  adapter to use. }
  IF ParamCount > 2 THEN al_set_new_display_adapter (StrToInt (ParamStr(2)));

{ Allegro requires installing drivers for all input devices before
  they can be used. }
  al_install_mouse;
  al_install_keyboard;

{ Initialize the image addon. Requires the allegro_image addon
  library. }
  al_init_image_addon;

{ Helper functions from common.pas. }
  InitPlatformSpecific;

{ Create a new display that we can render the image to. }
  Display := al_create_display (640, 480);
  IF Display = NIL THEN AbortExample ('Could not create display');

  al_set_window_title (Display, FileName);

{ Load the image and time how long it took for the log. }
  T0 := al_get_time;
  Bitmap := al_load_bitmap (FileName);
  t1 := al_get_time;
  IF Bitmap = NIL THEN
    AbortExample (Format ('"%s" not found or failed to load.', [filename]));

  LogWriteLn (Format ('Loading took %.4f seconds', [t1 - t0]));

{ Create a timer that fires 30 times a second. }
  Timer := al_create_timer (1.0 / 30);
  EventQueue := al_create_event_queue;
  al_register_event_source (EventQueue, al_get_keyboard_event_source);
  al_register_event_source (EventQueue, al_get_display_event_source (Display));
  al_register_event_source (EventQueue, al_get_timer_event_source (Timer));
  al_start_timer (Timer);

{ Primary 'game' loop. }
  Zoom := 1;
  Redraw := TRUE;
  EndLoop := FALSE;
  REPEAT
    al_wait_for_event (EventQueue, Event); { Wait for and get an event. }
    CASE Event.ftype OF
    ALLEGRO_EVENT_DISPLAY_ORIENTATION:
      CASE Event.display.orientation OF
      ALLEGRO_DISPLAY_ORIENTATION_0_DEGREES:
        LogWriteLn ('0 degrees');
      ALLEGRO_DISPLAY_ORIENTATION_90_DEGREES:
        LogWriteLn ('90 degrees');
      ALLEGRO_DISPLAY_ORIENTATION_180_DEGREES:
        LogWriteLn ('180 degrees');
      ALLEGRO_DISPLAY_ORIENTATION_270_DEGREES:
        LogWriteLn ('270 degrees');
      ALLEGRO_DISPLAY_ORIENTATION_FACE_UP:
        LogWriteLn ('Face up');
      ALLEGRO_DISPLAY_ORIENTATION_FACE_DOWN:
        LogWriteLn ('Face down');
      END;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EndLoop := TRUE;
    ALLEGRO_EVENT_KEY_CHAR:
    { Use keyboard to zoom image in and out.
      1: Reset zoom.
      +: Zoom in 10%
      -: Zoom out 10%
      f: Zoom to width of window
    }
      BEGIN
        IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
          EndLoop := TRUE; { Quit on escape key. }
        IF event.keyboard.unichar = ORD ('1') THEN
          Zoom := 1;
        IF event.keyboard.unichar = ORD ('+') THEN
          Zoom := Zoom * 1.1;
        IF event.keyboard.unichar = ORD ('-') THEN
          Zoom := Zoom / 1.1;
        IF event.keyboard.unichar = ORD ('f') THEN
          Zoom := al_get_display_width (Display) / al_get_bitmap_width (Bitmap);
      END;
    ALLEGRO_EVENT_TIMER:
    { Trigger a redraw on the timer event. }
      Redraw := TRUE;
    END;

  { Redraw, but only if the event queue is empty. }
    IF Redraw AND  al_is_event_queue_empty (EventQueue) THEN
    BEGIN
      Redraw := FALSE;
    { Clear so we don't get trippy artifacts left after zoom. }
      al_clear_to_color (al_map_rgb_f (0, 0, 0));
      IF Zoom = 1 THEN
        al_draw_bitmap (Bitmap, 0, 0, 0)
      ELSE
        al_draw_scaled_rotated_bitmap (
          Bitmap, 0, 0, 0, 0, Zoom, Zoom, 0, 0);
      al_flip_display;
    END;
  UNTIL EndLoop;

  al_destroy_bitmap (Bitmap);

  CloseLog (FALSE);
END.
