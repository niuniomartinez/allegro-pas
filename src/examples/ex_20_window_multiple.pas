program ex_20_window_multiple;
(* Shows how to manage multiple windows (displays). *)
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
    allegro5, al5font, al5image;

  const
  (* Initial window size. *)
    wWidth = 400; wHeight = 200;
    Margin = 20;

  type
  (* To store window information. *)
    TWindow = record
      Display: ALLEGRO_DISPLAYptr;
      Color: ALLEGRO_COLOR;
    { Information about the "jump". }
      jX, jY, jAdapter: Integer;
    end;

  var
  (* Monitor information. *)
    MonitorInfo: array of ALLEGRO_MONITOR_INFO;
  (* Windows. *)
    Windows: array [0..1] of TWindow;
  (* Events. *)
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
  (* Display stuff. *)
    TextFont: ALLEGRO_FONTptr;
    clrBlack: ALLEGRO_COLOR;
  (* To control program flow. *)
    Terminated: Boolean;

  function Initialize: Boolean;

    procedure GetMonitorInformation;
    var
      lNdx: Integer;
    begin
      SetLength (MonitorInfo, al_get_num_video_adapters);
      for lNdx := Low (MonitorInfo) to High (MonitorInfo) do
        al_get_monitor_info (lNdx, MonitorInfo[lNdx])
    end;

    function CreateWindow (pX, pY: Integer): ALLEGRO_DISPLAYptr;
    begin
      al_set_new_display_flags (
        ALLEGRO_WINDOWED or ALLEGRO_GENERATE_EXPOSE_EVENTS
      );
      al_set_new_window_position (pX, pY);
      Result := al_create_display (wWidth, wHeight);
      if Assigned (Result) then
        al_register_event_source (
          EventQueue,
          al_get_display_event_source (Result)
        )
    end;

  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_install_mouse
    or not al_init_font_addon or not al_init_image_addon
    then
    begin
      WriteLn ('Can''t initialize Allegro!');
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
  { Create windows. }
    GetMonitorInformation;
    Windows[0].Display := CreateWindow (
      Trunc (((MonitorInfo[0].x2 - MonitorInfo[0].x1) / 3) - (wWidth / 2)),
      Trunc (((MonitorInfo[0].y2 - MonitorInfo[0].y1) / 2) - (wHeight / 2))
    );
    Windows[1].Display := CreateWindow (
      Trunc (((MonitorInfo[0].x2 - MonitorInfo[0].x1) / 3) - (wWidth / 2)) * 2,
      Trunc (((MonitorInfo[0].y2 - MonitorInfo[0].y1) / 2) - (wHeight / 2))
    );
    if not Assigned (Windows[0].Display) or not Assigned (Windows[1].Display)
    then
    begin
      if Assigned (Windows[0].Display) then
      begin
        al_destroy_display (Windows[0].Display);
        Windows[0].Display := nil
      end;
      WriteLn ('Can''t create windows!');
      Exit (False)
    end;
    Windows[0].Color := al_map_rgb (255, 0, 255);
    Windows[1].Color := al_map_rgb (155, 255, 0);
    clrBlack := al_map_rgb (0, 0, 0);
  { By default, fonts and bitmaps are loaded/created in VRAM.  That VRAM is
    "linked" to a display, so there won't work to draw a bitmap or a font in
    to a different display.

    Regular RAM aren't "linked" to any display.
  }
    al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
    TextFont := al_load_font ('data/fixed_font.tga', 0, 0);
    if not Assigned (TextFont) then
    begin
      ErrorMessage ('Can''t load "data/fixed_font.tga".');
      Exit (False)
    end;
  { Improve random values. }
    Randomize;
    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  var
    lNdx: Integer;
  begin
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (TextFont) then al_destroy_font (TextFont);
    for lNdx := Low (Windows) to High (Windows) do
      if Assigned (Windows[lNdx].Display) then
        al_destroy_display (Windows[lNdx].Display);
  end;



(* Draw window content. *)
  procedure UpdateWindow (const aWindow: TWindow);
  var
    lx, ly, lw, lh: Integer;
  begin
    al_get_window_position (aWindow.Display, lx, ly);
    lw := al_get_display_width (aWindow.Display);
    lh := al_get_display_height (aWindow.Display);

    al_set_target_backbuffer (aWindow.Display);
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    al_clear_to_color (aWindow.Color);
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);

    al_draw_textf (
      TextFont, clrBlack,
      lw div 2, lh div 2 - 15, ALLEGRO_ALIGN_CENTRE,
      'Location: %d %d',
      [lx, ly]
    );
    al_draw_textf (
      TextFont, clrBlack,
      lw div 2, lh div 2, ALLEGRO_ALIGN_CENTRE,
      'Last jumped to: %d, %d (adapter %d)',
      [aWindow.jX, aWindow.jY, aWindow.jAdapter]
    );
    al_draw_textf (
      TextFont, clrBlack,
      lw div 2, lh div 2 + 15, ALLEGRO_ALIGN_CENTRE,
      'Size: %d, %d',
      [lw, lh]
    );
    al_draw_text (
      Textfont, clrBlack,
      lw div 2, lh div 2 + 30, ALLEGRO_ALIGN_CENTRE,
      'Click me to jump!'
    );
    al_flip_display
  end;



(* Changes window position. *)
  procedure WindowJump (var aWindow: TWindow);
  var
    lX, lY, lW, lH, lAdapter: Integer;
  begin
    lAdapter := Random (Length (MonitorInfo));
    lW := MonitorInfo[lAdapter].x2 - MonitorInfo[lAdapter].x1;
    lH := MonitorInfo[lAdapter].y2 - MonitorInfo[lAdapter].y1;
    lX := Margin + MonitorInfo[lAdapter].x1 + (Random (lW - wWidth - Margin));
    lY := Margin + MonitorInfo[lAdapter].y1 + (Random (lH - wHeight - Margin));
    aWindow.jX := lX; aWindow.jY := lY; aWindow.jAdapter := lAdapter;
    al_set_window_position (aWindow.Display, lX, lY);
  end;

var
  Ndx: Integer;
begin
  if not Initialize then Exit;
  for Ndx := Low (Windows) to High (Windows) do
    UpdateWindow (Windows[Ndx]);
{ "Game loop" }
  Terminated := False;
  repeat
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_DISPLAY_EXPOSE:
      for Ndx := Low (Windows) to High (Windows) do
        if Windows[Ndx].Display = Event.display.source then
          UpdateWindow (Windows[Ndx]);
    ALLEGRO_EVENT_KEY_DOWN:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
        Terminated := True;
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
      for Ndx := Low (Windows) to High (Windows) do
        if Windows[Ndx].Display = Event.mouse.display then
        begin
          WindowJump (Windows[Ndx]);
          UpdateWindow (Windows[Ndx]);
        end;
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
