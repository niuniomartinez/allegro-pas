program ex_11_mouse_warp;
(* Show how to keep the mouse inside the window. *)
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
    allegro5      in '../lib/allegro5.pas',
    al5font       in '../lib/al5font.pas',
    al5image      in '../lib/al5image.pas',
    al5primitives in '../lib/al5primitives.pas',
    al5strings    in '../lib/al5strings.pas';

  const
  (* Window size. *)
    wWidth = 640; wHeight = 480;

  var
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Window: ALLEGRO_DISPLAYptr;
    TextFont: ALLEGRO_FONTptr;
    Redraw, Terminated: Boolean;
    MouseState: record
      X, Y: Integer;
      RightButtonDown: Boolean
    end;
  (* Colors to use. *)
    clrWhite, clrBlack, clrRed: ALLEGRO_COLOR;

(* Program initialization. *)
  function Initialize: Boolean;
  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_install_mouse
    or not al_init_font_addon or not al_init_image_addon
    or not al_init_primitives_addon
    then
    begin
      WriteLn ('Can''t initialize Allegro!');
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
  { Build colors. }
    clrWhite := al_map_rgb_f (1, 1, 1);
    clrBlack := al_map_rgb_f (0, 0, 0);
    clrRed   := al_map_rgb_f (1, 0, 0);
  { Load text font. }
    TextFont := al_create_builtin_font;
    if not Assigned (TextFont) then
    begin
      ErrorMessage ('Error creating text font.');
      Exit (False)
    end;
  { Initialize mouse. }
    if not al_install_mouse then
    begin
      ErrorMessage ('Can''t initialize Mouse!');
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
    al_clear_to_color (clrBlack);
    al_draw_text (
      TextFont, clrWhite,
      0, 0, 0,
      al_str_format (
        'x: %d y: %d dx: %d dy %d',
        [event.mouse.x, event.mouse.y, event.mouse.dx, event.mouse.dy]
      )
    );
    al_draw_text (
      TextFont, clrWhite,
      wWidth div 2, wHeight div 2 - 8, ALLEGRO_ALIGN_CENTRE,
      'Left-Click to warp pointer to the middle once.'
    );
    al_draw_text (
      TextFont, clrWhite,
      wWidth div 2, wHeight div 2, ALLEGRO_ALIGN_CENTRE,
      'Hold right mouse button to constantly move pointer to the middle.'
    );
    if MouseState.RightButtonDown then
    begin
      al_draw_line (
        wWidth div 2, wHeight div 2,
        MouseState.X, MouseState.Y,
        clrRed, 1
      );
      al_draw_line (
        MouseState.X - 5, MouseState.Y,
        MouseState.X + 5, MouseState.Y,
        clrWhite, 2
      );
      al_draw_line (
        MouseState.X, MouseState.Y - 5,
        MouseState.X, MouseState.Y + 5,
        clrWhite, 2
      )
    end;
    al_flip_display
  end;

begin
  if not Initialize then Exit;
  MouseState.X := 0; MouseState.Y := 0;
  MouseState.RightButtonDown := False;
{ Loop. }
  Redraw := True; Terminated := False;
  repeat
    if Redraw and al_is_event_queue_empty (EventQueue) then
    begin
      UpdateScreen;
      Redraw := False
    end;
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_KEY_DOWN:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
        Terminated := True;
    ALLEGRO_EVENT_MOUSE_WARPED: { ??? }
    { WriteLn ('Warp') };
    ALLEGRO_EVENT_MOUSE_AXES:
      begin
        if MouseState.RightButtonDown then
        begin
          al_set_mouse_xy (Window, wWidth div 2, wHeight div 2);
          Inc (MouseState.X, Event.mouse.dx);
          Inc (MouseState.Y, Event.mouse.dy)
        end;
        Redraw := True;
      end;
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
      begin
        case Event.mouse.button of
        1:
          al_set_mouse_xy (Window, wWidth div 2, wHeight div 2);
        2:
          begin
            MouseState.RightButtonDown := True;
            MouseState.X := wWidth div 2;
            MouseState.Y := wHeight div 2
          end;
        end;
        Redraw := True
      end;
    ALLEGRO_EVENT_MOUSE_BUTTON_UP:
      if Event.mouse.button = 2 then
        MouseState.RightButtonDown := False;
    end
  until Terminated;
  Finalize
end.
