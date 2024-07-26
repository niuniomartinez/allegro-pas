program ex_noframe;
(* Show how to manage frameless windows using mouse to drag them. *)
(*
  Copyright (c) 2012-2024 Guillermo Martínez J.

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
    allegro5, al5image;

  const
  (* Window size. *)
    wWidth = 300; wHeight = 200;
  var
  (* The event queue. *)
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
  (* An event. *)
    Event: ALLEGRO_EVENT;
  (* Pointer to the window information. *)
    Window: ALLEGRO_DISPLAYptr;
  (* Background bitmap. *)
    Bitmap: ALLEGRO_BITMAPptr;
  (* Timer to keep window updated. *)
    Timer: ALLEGRO_TIMERptr;
  (* To know when to stop. *)
    Terminated: Boolean;
  (* Mouse interaction. *)
    Mouse: record
      Down: Boolean;
      X, Y: LongInt
    end;

(* Program initialization. *)
  function Initialize: Boolean;
  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_install_mouse
    or not al_init_image_addon then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;
  { Create window without frame. }
    al_set_new_display_flags (ALLEGRO_WINDOWED or ALLEGRO_FRAMELESS);
    Window := al_create_display (wWidth, wHeight);
    if not Assigned (Window) then
    begin
      WriteLn ('Can''t create window.');
      Exit (False)
    end;
  { Load bitmap. }
    Bitmap := al_load_bitmap ('data/fakeamp.bmp');
    if not Assigned (Bitmap) then
    begin
      ErrorMessage ('Error loading fakeamp.bmp.');
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
    al_register_event_source (EventQueue, al_get_mouse_event_source);
    al_register_event_source (EventQueue, al_get_display_event_source (Window));
    Timer := al_create_timer (1 / 30);
    al_register_event_source (EventQueue, al_get_timer_event_source (Timer));

    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (Timer) then al_destroy_timer (Timer);
    if Assigned (Bitmap) then al_destroy_bitmap (Bitmap);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Mouse events. *)
  procedure MouseButtonDown (aMouseEvent: ALLEGRO_MOUSE_EVENT);
  begin
    case aMouseEvent.button of
    1:
      if aMouseEvent.x <> 0 then
      begin
        Mouse.Down := True;
        Mouse.x := aMouseEvent.x;
        Mouse.y := aMouseEvent.y
      end;
    2:
      al_set_display_flag (
        Window, ALLEGRO_FRAMELESS,
        not ((al_get_display_flags (Window) and ALLEGRO_FRAMELESS) <> 0)
      );
    end
  end;



  procedure MouseButtonUp (aMouseEvent: ALLEGRO_MOUSE_EVENT);
  begin
    if aMouseEvent.button = 1 then
      Mouse.Down := False
  end;



  procedure MouseMove (aMouseEvent: ALLEGRO_MOUSE_EVENT);
  var
    lx, ly: Integer;
  begin
    if Mouse.Down then
      if al_get_mouse_cursor_position (lx, ly) then
        al_set_window_position (Window, lx - Mouse.y, ly - Mouse.y)
  end;



(* Draw window content. *)
  procedure UpdateScreen;
  begin
    al_draw_bitmap (Bitmap, 0, 0, 0);
    al_flip_display
  end;


begin
  if not Initialize then Exit;
{ "Game loop". }
  Terminated := False;
  Mouse.Down := False;
  al_start_timer (Timer);
  repeat
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_KEY_DOWN:
      if event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
        Terminated := True;
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
      MouseButtonDown (Event.mouse);
    ALLEGRO_EVENT_MOUSE_BUTTON_UP:
      MouseButtonUp (Event.mouse);
    ALLEGRO_EVENT_MOUSE_AXES:
      MouseMove (Event.mouse);
    ALLEGRO_EVENT_TIMER:
      UpdateScreen;
    end
  until Terminated
end.
