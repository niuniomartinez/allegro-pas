program ex_clipboard;
(* Shows how clipboard works. *)
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
    allegro5, al5base, al5image, al5font;

  const
  (* Window size. *)
    wWidth = 800; wHeight = 600;
    TimeInterval = 0.1;

  var
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Window: ALLEGRO_DISPLAYptr;
    Timer: ALLEGRO_TIMERptr;
    TextFont: ALLEGRO_FONTptr;
    LineHeight: Integer;
    ClipboardText: AL_STRptr;
    Redraw, Terminated: Boolean;

(* Helper to draw text. *)
  procedure PrintText (
    const aText: AL_STRptr;
    const y: Integer;
    const aColor: ALLEGRO_COLOR
  );
  const
    Margin = 8;
  begin
    al_draw_text (TextFont, aColor, Margin, Margin + y * LineHeight, 0, aText)
  end;



(* Program initialization. *)
  function Initialize: Boolean;
  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_init_image_addon
    or not al_init_font_addon
    then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;
  { Create the timer. }
    Timer := al_create_timer (TimeInterval);
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
  { Create text font. }
    TextFont := al_load_font ('data/fixed_font.tga', 0, 0);
    if not Assigned (TextFont) then
    begin
      ErrorMessage ('Can''t load "data/fixed_font.tga".');
      Exit (False)
    end;
    LineHeight := al_get_font_line_height (TextFont);
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
    if Assigned (ClipboardText) then al_free (ClipboardText);
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (Timer) then al_destroy_timer (Timer);
    if Assigned (TextFont) then al_destroy_font (TextFont);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Get reference to the clipboard text, if any. *)
  procedure GetClipboardContent;
  begin
    if Assigned (ClipboardText) then al_free (ClipboardText);
    if al_clipboard_has_text (Window) then
      ClipboardText := al_get_clipboard_text (Window)
    else
      ClipboardText := Nil
  end;



(* Draw screen. *)
  procedure UpdateScreen;
  begin
  { Draw clipboard content and instructions. }
    al_clear_to_color (al_map_rgb_f (0, 0, 0));
    PrintText (
      'Copy a text from another application or press [Space bar]...',
      0,
      al_map_rgb_f (1, 1, 1)
    );
    if Assigned (ClipboardText) then
      PrintText (ClipboardText, 2, al_map_rgba_f (0, 1, 1, 1.0))
    else
      PrintText ('No clipboard text available.', 2, al_map_rgba_f(1, 0, 0, 1.0));
    al_flip_display;
    Redraw := False
  end;

begin
  if not Initialize then Exit;
{ "Game loop". }
  ClipboardText := Nil;
  Redraw := True; Terminated := False;
  al_start_timer (Timer);
  repeat
  { Window update. }
    if Redraw and al_is_event_queue_empty (EventQueue) then
    begin
      GetClipboardContent;
      UpdateScreen
    end;
  { Check events. }
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_KEY_DOWN:
      case Event.keyboard.keycode of
      ALLEGRO_KEY_ESCAPE:
        Terminated := True;
      ALLEGRO_KEY_SPACE:
      { Set text to the clipboard (i.e. "copy" text). }
        al_set_clipboard_text (Window, 'Copied from Allegro!');
      end;
    ALLEGRO_EVENT_TIMER:
      Redraw := true;
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
