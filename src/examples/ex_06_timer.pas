program ex_06_timer;
(* Shows how to use Allegro's timer to control time.

   It also uses a backbuffer to simulate an old 320x200 display.
 *)
(*
  Copyright (c) 2024 Guillermo Martínez J.

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
    allegro5, al5base, al5font, al5strings;

  const
  (* Window size. *)
    wWidth = 800; wHeight = 600;
  (* Backbuffer size. *)
    bWidth = 320; bHeight = 280;
  type
  (* Manages a clock. *)
    TClock = record Minutes, Seconds, dSeconds: Integer end;
  var
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Window: ALLEGRO_DISPLAYptr;
    Backbuffer: ALLEGRO_BITMAPptr;
    Timer: ALLEGRO_TIMERptr;
    TextFont: ALLEGRO_FONTptr;
    Terminated: Boolean;
    Clock: TClock;

(* Update a clock. *)
  procedure UpdateClock (var aClock: TClock);
  begin
    Inc (aClock.dSeconds);
    if aClock.dSeconds > 9 then
    begin
      aClock.dSeconds := 0;
      Inc (aClock.Seconds);
      if aClock.Seconds > 59 then
      begin
        aClock.Seconds := 0;
        Inc (aClock.Minutes)
      end
    end
  end;



(* Draw a clock. *)
  procedure DrawClock (const aClock: TClock);
  var
    lClockDisplay: AL_STR;
  begin
  { Draw clock. }
    al_set_target_bitmap (Backbuffer);
    lClockDisplay := al_str_format (
      '%0.2d:%0.2d:%d',
      [aClock.Minutes, aClock.Seconds, aClock.dSeconds]
    );
    al_clear_to_color (al_map_rgb (51, 51, 204));
    al_draw_text (
      TextFont, al_map_rgb (0, 0, 0),
      bWidth div 2 + 1, bHeight div 2 + 1, ALLEGRO_ALIGN_CENTRE OR ALLEGRO_ALIGN_INTEGER,
      lClockDisplay
    );
    al_draw_text (
      TextFont, al_map_rgb (255, 255, 255),
      bWidth div 2, bHeight div 2, ALLEGRO_ALIGN_CENTRE OR ALLEGRO_ALIGN_INTEGER,
      lClockDisplay
    );
  { Put clock on window. }
    al_set_target_backbuffer (Window);
    al_draw_scaled_bitmap (
      Backbuffer,
      0, 0, bWidth, bHeight,
      0, 0, wWidth, wHeight,
      0
    )
  end;



(* Program initialization. *)
  function Initialize: Boolean;
  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_init_font_addon then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;
  { Create the timer. }
    Timer := al_create_timer (ALLEGRO_BPS_TO_SECS (10));{ 10 ticks per second. }
    if not Assigned (Timer) then
    begin
      WriteLn ('Can''t initialize timer.');
      Exit (False)
    end;
  { Create window. }
    al_set_new_display_flags (ALLEGRO_WINDOWED);
    Window := al_create_display (wWidth, wHeight);
    if not Assigned (Window) then
    begin
      WriteLn ('Can''t create window.');
      Exit (False)
    end;
  { Create backbuffer.  It is used to zoom-in the clock. }
    Backbuffer := al_create_bitmap (bWidth, bHeight);
    if not Assigned (Backbuffer) then
    begin
      ErrorMessage ('Can''t create backbuffer.');
      Exit (False)
    end;
  { Create text font. }
    TextFont := al_create_builtin_font;
    if not Assigned (TextFont) then
    begin
      ErrorMessage ('Error creating text font.');
      Exit (False)
    end;
  { Create the event queue. }
    EventQueue := al_create_event_queue;
    if not Assigned (EventQueue) then
    begin
      ErrorMessage ('Can''t initialize event queue!');
      Exit (False)
    end;
    al_register_event_source (EventQueue, al_get_keyboard_event_source);
    al_register_event_source (EventQueue, al_get_display_event_source (Window));
    al_register_event_source (EventQueue, al_get_timer_event_source (Timer));

    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
  { Allegro will destroy most objects at exit but it is a good idea to get used
    to destroy all created objects.
  }
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (Backbuffer) then al_destroy_bitmap (Backbuffer);
    if Assigned (Timer) then al_destroy_timer (Timer);
    if Assigned (TextFont) then al_destroy_font (TextFont);
    if Assigned (Window) then al_destroy_display (Window)
  end;

begin
  if not Initialize then Exit;
{ Initialize clock. }
  Clock.dSeconds := 0; Clock.Seconds := 0; Clock.Minutes := 0;
  al_start_timer (Timer);
{ "Game loop". }
  Terminated := False;
  repeat
  { Window update. }
    if al_is_event_queue_empty (EventQueue) then
    begin
      DrawClock (Clock);
      al_flip_display
    end;
  { Events. }
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_KEY_DOWN:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
        Terminated := True;
    ALLEGRO_EVENT_TIMER:
      UpdateClock (Clock);
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
