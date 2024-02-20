program ex_17_window;
(* Shows display stuff as icons, events and such. *)
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
    allegro5   in '../lib/allegro5.pas',
    al5font    in '../lib/al5font.pas',
    al5image   in '../lib/al5image.pas',
    al5strings in '../lib/al5strings.pas';

  const
  (* Initial window size. *)
    wWidth = 800; wHeight = 600;

  var
  (* The event queue. *)
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
  (* Pointer to the window information. *)
    Window: ALLEGRO_DISPLAYptr;
  (* Pointers to icons. *)
    IconBmp: array [0..1] of ALLEGRO_BITMAPptr;
    CurrentIcon: Integer;
  (* To store a text font. *)
    TextFont: ALLEGRO_FONTptr;
  (* To control program flow. *)
    Redraw, Terminated: Boolean;
  (* An event. *)
    Event: ALLEGRO_EVENT;

    Counter: Integer;

(* Swap window icons. *)
  procedure SwapWindowIcons; inline;
  begin
    CurrentIcon := 1 - CurrentIcon; { <- !!! }
    al_set_display_icon (Window, IconBmp[CurrentIcon])
  end;



(* Tell if window is maximized. *)
  function WindowMaximized: Boolean; inline;
  begin
    Result := (al_get_display_flags (Window) and ALLEGRO_MAXIMIZED) <> 0
  end;



(* Program initialization. *)
  function Initialize: Boolean;

  (* Helper to load icons. *)
    function LoadIcon (const aFilename: AnsiString): ALLEGRO_BITMAPptr;
    begin
      Result := al_load_bitmap (
        al_string_to_str (Concat ('data/', aFilename))
      );
      if not Assigned (Result) then
      begin
        ErrorMessage (Concat ('Can''t load "data/', aFilename,'".'));
        Exit
      end;
    { Change "pink" to "transparent". }
      al_convert_mask_to_alpha (Result, al_map_rgb (255, 0, 255))
    end;

  const
    IconNames: array [0..1] of AnsiString = ('icon.tga', 'alexlogo.png');
  var
    Ndx: Integer;
  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_init_font_addon
    or not al_init_image_addon
    then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;
  { Create window. }
    al_set_new_display_flags (
      ALLEGRO_RESIZABLE or ALLEGRO_GENERATE_EXPOSE_EVENTS
    );
    Window := al_create_display (wWidth, wHeight);
    if not Assigned (Window) then
    begin
      WriteLn ('Can''t create window.');
      Exit (False)
    end;
  { Window title. }
    Counter := 0;
    al_set_window_title (Window, 'This is the first title');
  { Load and set the window icons. }
    for Ndx := Low (IconBmp) to High (IconBmp) do
    begin
      IconBmp[Ndx] := LoadIcon (IconNames[Ndx]);
      if not Assigned (IconBmp[Ndx]) then Exit (False)
    end;
    CurrentIcon := 0;
    SwapWindowIcons; { <- Set new icon. }
  { Create text font. }
    TextFont := al_load_font ('data/font.tga', 0, 0);
    if not Assigned (TextFont) then
    begin
      ErrorMessage ('Can''t load "data/font.tga".');
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

    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  var
    lIcon: ALLEGRO_BITMAPptr;
  begin
  { Allegro will destroy most objects at exit but it is a good idea to get used
    to destroy all created objects.
  }
    for lIcon in IconBmp do if Assigned (lIcon) then al_destroy_bitmap (lIcon);
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (TextFont) then al_destroy_font (TextFont);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Draw window content. *)
  procedure UpdateScreen;
  var
    lLineHeight, lPosX, lPosY: Integer;
    lColor: ALLEGRO_COLOR;
    lMaximiced: AnsiString;

    procedure DrawText (const aText: AnsiString); inline;
    begin
      al_draw_text (TextFont, lColor, lPosX, lPosY, 0, al_string_to_str(aText));
      Inc (lPosY, lLineHeight)
    end;

  begin
    lLineHeight := al_get_font_line_height (TextFont);
    lPosX := lLineHeight; lPosY := lLineHeight;
    lColor := al_map_rgb (255, 255, 255);
    if WindowMaximized then lMaximiced := 'yes' else lMaximiced := 'no';
  { Draw text on screen. }
    al_clear_to_color (al_map_rgb (0, 0, 0));
    DrawText ('You can resize the window.');
    DrawText (al_str_format ('Maximized: %s', [lMaximiced]));
    DrawText ('[I] Change icon.');
    DrawText ('[T] Change title.');
    DrawText ('[+] To maximize (buggy?).');
    DrawText ('[-] To un-maximize.');
    DrawText ('[Esc] Terminate.');
    DrawText ('');
    DrawText ('BUG:  After [+] it looks locked.  maximize/un-maximize the window');
    DrawText ('      as much times you pressed [+] to unlock.');
  { Make text visible. }
    al_flip_display
  end;

begin
{ Program initialization. }
  if not Initialize then Exit;
{ "Game loop". }
  Redraw := True; Terminated := False;
  repeat
    if Redraw and al_is_event_queue_empty (EventQueue) then
    begin
      UpdateScreen;
      Redraw := False
    end;
  { Check events. }
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      begin
        al_acknowledge_resize (Event.display.source);
        Redraw := True
      end;
    ALLEGRO_EVENT_DISPLAY_EXPOSE:
      Redraw := True;
    ALLEGRO_EVENT_KEY_DOWN:
      case Event.keyboard.keycode of
      ALLEGRO_KEY_ESCAPE:
        Terminated := True;
      ALLEGRO_KEY_I:
        SwapWindowIcons;
      ALLEGRO_KEY_T:
        begin
          Inc (Counter);
          al_set_window_title (
            Window,
            al_str_format ('Title number %d', [Counter])
          )
        end;
      else { otherwise }
        if Event.keyboard.unichar = Ord ('+') then
          al_set_display_flag (Window, ALLEGRO_MAXIMIZED, True)
        else
        if Event.keyboard.unichar = Ord ('-') then
          al_set_display_flag (Window, ALLEGRO_MAXIMIZED, False)
      end;
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
