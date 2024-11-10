program ex_08_filter;
(* Shows how to use bitmap filters. *)
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
    allegro5, al5font, al5image, al5strings;

  const
  (* Window size. *)
    wWidth = 640; wHeight = 480;
  (* Speed in frames per second. *)
    FPS = 60;
  (* Filters to apply. *)
    FilterFlags: array [0..5] of Integer = (
      0,
      ALLEGRO_MIN_LINEAR,
      ALLEGRO_MIPMAP,
      ALLEGRO_MIN_LINEAR or ALLEGRO_MIPMAP,
      0,
      ALLEGRO_MAG_LINEAR
    );
  (* Filter names. *)
    FilterText: array [0..3] of String = (
      'nearest', 'linear',
      'nearest mipmap', 'linear mipmap'
    );

  var
  (* The event queue. *)
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
  (* An event. *)
    Event: ALLEGRO_EVENT;
  (* Pointer to the window information. *)
    Window: ALLEGRO_DISPLAYptr;
  (* To store a text font. *)
    TextFont: ALLEGRO_FONTptr;
  (* To store the bitmaps used. *)
    Bitmaps: array [0..1] of array [0..8] of ALLEGRO_BITMAPptr;
  (* Bitmap to draw, how many ticks have passed. *)
    BitmapNdx, Ticks: Integer;
  (* Colors used. *)
    clrBlack, clrWhite, clrInfo: ALLEGRO_COLOR;
  (* Timer. *)
    Timer: ALLEGRO_TIMERptr;
  (* To know when to update the screen, when to stop. *)
    NeedRedraw, Terminated: Boolean;

(* Program initialization. *)
  function Initialize: Boolean;

  (* Create the bitmaps with mipmaps. *)
    function CreateBitmaps: Boolean;

    (* Create a chessboard bitmap. *)
      function CreateChessboard: ALLEGRO_BITMAPptr;
      const
        bmpSize = 1024;
        SquareSize = 3;
      var
        lLock: ALLEGRO_LOCKED_REGIONptr;
        lColor: ALLEGRO_COLOR;
        lx, ly, lx2, ly2: Integer;
      begin
        Result := al_create_bitmap (bmpSize, bmpSize);
        al_set_target_bitmap (Result);
      {$IFDEF LINUX}
      { For some reason al_lock_bitmap doesn't work on Windows, neither
        compilling with FPC nor Delphi.
      }
        lLock := al_lock_bitmap (Result, al_get_bitmap_format (Result), ALLEGRO_LOCK_WRITEONLY);
      {$ENDIF}
        for ly := 0 to bmpSize - 1 do
          for lx := 0 to bmpSize - 1 do
          begin
            lx2 := lx div SquareSize; ly2 := ly div SquareSize;
            if ((lx2 and ly2 and 1) = 1) or (((lx2 or ly2) and 1) = 0) then
              lColor := clrWhite
            else
              lColor := clrBlack;
            al_draw_pixel (lx + 0.5, ly + 0.5, lColor)
          end;
      {$IFDEF LINUX}
        al_unlock_bitmap (Result);
      {$ENDIF}
        al_set_target_backbuffer (Window)
      end;

    var
      lMysha, lChessBoard: ALLEGRO_BITMAPptr;
      lNdx: Integer;
    begin
      al_draw_text (
        TextFont, clrWhite, 10, 10, 0,
        'Creating textures...'
      );
      al_flip_display;
    { Get bitmaps. }
      lMysha := al_load_bitmap ('data/mysha256x256.png');
      if not Assigned (lMysha) then
      begin
        ErrorMessage ('Error loading data/mysha256x256.png');
        Exit (False)
      end;
      lChessBoard := CreateChessboard;
    { Build bitmaps with mipmaps. }
      for lNdx := Low (FilterFlags) to High (FilterFlags) do
      begin
        al_set_new_bitmap_flags (FilterFlags[lNdx]);
        Bitmaps[0][lNdx] := al_clone_bitmap (lChessBoard);
        Bitmaps[1][lNdx] := al_clone_bitmap (lMysha)
      end;
    { Not needed anymore. }
      al_destroy_bitmap (lMysha);
      al_destroy_bitmap (lChessBoard);

      Result := True
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
  { Create window. }
    al_set_new_display_flags (ALLEGRO_WINDOWED);
    Window := al_create_display (wWidth, wHeight);
    if not Assigned (Window) then
    begin
      WriteLn ('Can''t create window.');
      Exit (False)
    end;
    clrBlack := al_map_rgb (0, 0, 0);
    clrWhite := al_map_rgb (255, 255, 255);
    clrInfo  := al_map_rgb (128, 128, 255);
  { The timer. }
    Timer := al_create_timer (1 / FPS);
  { Load text font. }
    TextFont := al_load_font ('data/fixed_font.tga', 0, 0);
    if not Assigned (TextFont) then
    begin
      ErrorMessage ('Error loading fixed_font.tga');
      Exit (False)
    end;
  { The bitmaps. }
    if not CreateBitmaps then Exit (False);
  { Create the event queue. }
    EventQueue := al_create_event_queue;
    if not Assigned (EventQueue) then
    begin
      ErrorMessage ('Can''t initialize event queue!');
      Exit (False)
    end;
    al_register_event_source (EventQueue, al_get_keyboard_event_source);
    al_register_event_source (EventQueue, al_get_mouse_event_source);
    al_register_event_source (EventQueue, al_get_timer_event_source (Timer));
    al_register_event_source (EventQueue, al_get_display_event_source (Window));

    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  var
    lBmp, lFilter: integer;
  begin
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (Timer) then al_destroy_timer (Timer);
    if Assigned (TextFont) then al_destroy_font (TextFont);
    for lBmp := Low (Bitmaps) to High (Bitmaps) do
      for lFilter := Low (Bitmaps[lBmp]) to High (Bitmaps[lBmp]) do
        if Assigned (Bitmaps[lBmp][lFilter]) then
          al_destroy_bitmap (Bitmaps[lBmp][lFilter]);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Draw window content. *)
  procedure UpdateScreen;
  var
    lNdx, lBmpW, lBmpH: Integer;
    lx, ly, lScaleDelta, lScale, lAngle: Real;
    lBmp: ALLEGRO_BITMAPptr;
  begin
    al_clear_to_color (clrBlack);
  { Draw bitmaps in different scales and angles. }
    for lNdx := 0 to 5 do
    begin
      lBmp := Bitmaps[BitmapNdx][lNdx];
      lBmpW := al_get_bitmap_width (lBmp);
      lBmpH := al_get_bitmap_height (lBmp);
    { Bitmap position. }
      lx := (lNdx div 2) * wWidth / 3 + wWidth / 6;
      ly := (lNdx mod 2) * wHeight / 2 + wHeight / 4;
    { Bitmap angle. }
      lAngle := Ticks * ALLEGRO_TAU / FPS / 8;
    { Bitmap scale. }
      lScaleDelta := 1 - 2 * Abs ((Ticks mod (FPS * 16)) / 16 / FPS - 0.5);
      if lNdx < 4 then
         lScale := 1 - lScaleDelta * 0.9
      else
         lScale := 1 + lScaleDelta * 9;
    { Legend. }
      al_draw_text (
        TextFont, clrWhite, lx, ly - 64 - 14, ALLEGRO_ALIGN_CENTRE,
        al_string_to_str (FilterText[lNdx mod 4])
      );
    { Draw bitmap. }
      al_set_clipping_rectangle (Trunc (lx - 64), Trunc (ly - 64), 128, 128);
      al_draw_scaled_rotated_bitmap (
        lBmp, lBmpW / 2, lBmpH / 2,
        lx, ly, lScale, lScale, lAngle, 0
      );
      al_set_clipping_rectangle (0, 0, wWidth, wWidth)
    end;
  { Done. }
    al_draw_text (
      TextFont, clrInfo, wWidth / 2, wHeight - 14, ALLEGRO_ALIGN_CENTRE,
      'press space to change'
    );
  { Done. }
    al_flip_display;
    NeedRedraw := False
  end;

begin
  if not Initialize then Exit;
{ "Game loop". }
  BitmapNdx := 0;
  Terminated := False;
  NeedRedraw := False;
  Ticks := 0;
  al_start_timer (Timer);
  repeat
    if NeedRedraw and al_is_event_queue_empty (EventQueue) then UpdateScreen;
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_KEY_DOWN:
      case Event.keyboard.keycode of
      ALLEGRO_KEY_ESCAPE:
        Terminated := True;
      ALLEGRO_KEY_SPACE:
        BitmapNdx := 1 - BitmapNdx;
      end;
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
      BitmapNdx := 1 - BitmapNdx;
    ALLEGRO_EVENT_TIMER:
      begin
        Inc (Ticks);
        NeedRedraw := True
      end;
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
