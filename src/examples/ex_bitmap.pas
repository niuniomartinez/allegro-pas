PROGRAM ex_bitmap;
(*
 * This example displays a picture on the screen, with support for
 * command-line parameters, multi-screen, screen-orientation and
 * zooming.
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
  Allegro5, al5base, al5image, al5strings,
  sysutils;
var
  FileName: AL_STR;
  Bitmap: ALLEGRO_BITMAPptr;
  Timer: ALLEGRO_TIMERptr;
  Display: ALLEGRO_DISPLAYptr;
  EventQueue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Redraw, EndLoop: Boolean;
  Zoom, T0, T1: Double;
begin
{ The first commandline argument can optionally specify an
  image to display instead of the default. Allegro's image
  addon suports BMP, DDS, PCX, TGA and can be compiled with
  PNG and JPG support on all platforms. Additional formats
  are supported by platform specific libraries and support for
  image formats can also be added at runtime. }
  if ParamCount > 0 then
    FileName := al_string_to_str (ParamStr (1))
  else
    FileName := 'data/mysha.pcx';

  if not al_init then AbortExample ('Could not init Allegro.');

{ Initializes and displays a log window for debugging purposes. }
  OpenLog;

{ The second parameter to the process can optionally specify what
  adapter to use. }
  if ParamCount > 2 then al_set_new_display_adapter (StrToInt (ParamStr(2)));

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
  if Display = Nil then AbortExample ('Could not create display');

  al_set_window_title (Display, FileName);

{ Load the image and time how long it took for the log. }
  T0 := al_get_time;
  Bitmap := al_load_bitmap (FileName);
  t1 := al_get_time;
  if Bitmap = Nil then
    AbortExample (
      al_str_format ('"%s" not found or failed to load.', [filename])
    );

  LogPrintLn ('Loading took %.4f seconds', [t1 - t0]);

{ Create a timer that fires 30 times a second. }
  Timer := al_create_timer (1.0 / 30);
  EventQueue := al_create_event_queue;
  al_register_event_source (EventQueue, al_get_keyboard_event_source);
  al_register_event_source (EventQueue, al_get_display_event_source (Display));
  al_register_event_source (EventQueue, al_get_timer_event_source (Timer));
  al_start_timer (Timer);

{ Primary 'game' loop. }
  Zoom := 1;
  Redraw := True;
  EndLoop := False;
  repeat
    al_wait_for_event (EventQueue, @Event); { Wait for and get an event. }
    case Event.ftype OF
    ALLEGRO_EVENT_DISPLAY_ORIENTATION:
      case Event.display.orientation OF
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
      end;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EndLoop := True;
    ALLEGRO_EVENT_KEY_CHAR:
    { Use keyboard to zoom image in and out.
      1: Reset zoom.
      +: Zoom in 10%
      -: Zoom out 10%
      f: Zoom to width of window
    }
      begin
        if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
          EndLoop := True; { Quit on escape key. }
        if event.keyboard.unichar = Ord ('1') then
          Zoom := 1;
        if event.keyboard.unichar = Ord ('+') then
          Zoom := Zoom * 1.1;
        if event.keyboard.unichar = Ord ('-') then
          Zoom := Zoom / 1.1;
        if event.keyboard.unichar = Ord ('f') then
          Zoom := al_get_display_width (Display) / al_get_bitmap_width (Bitmap);
      end;
    ALLEGRO_EVENT_TIMER:
    { Trigger a redraw on the timer event. }
      Redraw := True;
    end;

  { Redraw, but only if the event queue is empty. }
    if Redraw and  al_is_event_queue_empty (EventQueue) then
    begin
      Redraw := False;
    { Clear so we don't get trippy artifacts left after zoom. }
      al_clear_to_color (al_map_rgb_f (0, 0, 0));
      if Zoom = 1 then
        al_draw_bitmap (Bitmap, 0, 0, 0)
      else
        al_draw_scaled_rotated_bitmap (
          Bitmap, 0, 0, 0, 0, Zoom, Zoom, 0, 0);
      al_flip_display;
    end;
  until EndLoop;

  al_destroy_bitmap (Bitmap);

  CloseLog (False);
end.
