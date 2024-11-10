program ex_02_events;
(* Shows how does the Allegro's event system work. *)
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
    allegro5, al5font;

  const
  (* Window size. *)
    wWidth = 800; wHeight = 600;
  var
  (* The event queue. *)
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
  (* An event. *)
    Event: ALLEGRO_EVENT;
  (* Pointer to the window information. *)
    Window: ALLEGRO_DISPLAYptr;
  (* To store a text font. *)
    TextFont: ALLEGRO_FONTptr;
  (* To know when to stop. *)
    Terminated: Boolean;

(* Program initialization. *)
  function Initialize: Boolean;
  begin
  { Initialize Allegro. }
    if not al_init then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;
  { Initialize keyboard. }
    if not al_install_keyboard then
    begin
      WriteLn ('Can''t initialize keyboard input!');
      Exit (False)
    end;
  { Initialize add-ons. }
    if not al_init_font_addon then
    begin
      WriteLn ('Can''t use text fonts!');
      Exit (False)
    end;
  { Create the event queue. }
    EventQueue := al_create_event_queue;
    if not Assigned (EventQueue) then
    begin
      WriteLn ('Can''t initialize event queue!');
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
  { Create text font. }
    TextFont := al_create_builtin_font;
    if not Assigned (TextFont) then
    begin
    { Close window so it doesn't obscure the console. }
      al_destroy_display (Window); Window := Nil;
      WriteLn ('Error creating text font.');
      Exit (False)
    end;
  { Register event sources. }
    al_register_event_source (EventQueue, al_get_keyboard_event_source);
    al_register_event_source (EventQueue, al_get_display_event_source (Window));

    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
  { Allegro will destroy most objects at exit but it is a good idea to get used
    to destroy all created objects.
  }
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (TextFont) then al_destroy_font (TextFont);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Draw window content. *)
  procedure UpdateScreen;
  begin
  { Draw text on screen. }
    al_draw_text (
      TextFont, al_map_rgb (255, 255, 255),
      wWidth div 2, wHeight div 2, ALLEGRO_ALIGN_CENTRE OR ALLEGRO_ALIGN_INTEGER,
      'Hello, World!'
    );
  { Make text visible. }
    al_flip_display
  end;

begin
{ Program initialization. }
  if not Initialize then Exit;
{ The main loop of the program.  Here we wait for events to come in from
  any one of the event sources and react to each one accordingly.  While
  there are no events to react to the program sleeps and consumes very
  little CPU time.  See function Initialize to see how the event sources and
  event queue are set up.
}
  Terminated := False;
  repeat
  { Draw screen inside the loop.  This is because some systems need the
    window to be redrawn when they're hidden or minimized (check
    ex_helloworld).

    It does if the event queue is empty only.
  }
    if al_is_event_queue_empty (EventQueue) then UpdateScreen;
  { Take the next event out of the event queue, and store it in `event'. }
    al_wait_for_event (EventQueue, @Event);
  { Check what type of event we got and act accordingly.  ALLEGRO_EVENT is
    a union type and interpretation of its contents is dependent on the
    event type, which is given by the 'ftype' field.

    Each event also comes from an event source and has a timestamp.  These
    are accessible through the 'any.source' and 'any.timestamp' fields
    respectively, e.g. 'event.any.timestamp'
  }
    case Event.ftype of
  { ALLEGRO_EVENT_DISPLAY_CLOSE - the window close button was pressed. }
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
  { ALLEGRO_EVENT_KEY_DOWN - a keyboard key was pressed. }
    ALLEGRO_EVENT_KEY_DOWN:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
        Terminated := True;
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
