program ex_24_draw;
(* Tests some drawing primitives. *)
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
    allegro5, al5color, al5font, al5image, al5primitives, al5strings;

  type
  (* Identifies the primitives. *)
    TPrimitives = (pFilledRect, pRect, pFilledCircle, pCircle, pLine);

  const
  (* Window size. *)
    wWidth = 640; wHeight = 640;
  (* Speed in frames per second. *)
    FPS = 60;
  (* Size of bitmaps. *)
    BmpWidth = 32; BmpHeight = BmpWidth;
    ZoomWidth = BmpWidth; ZoomHeight = BmpHeight;
  (* Primitive names. *)
    PrimitiveNames: array [TPrimitives] of AnsiString = (
      'filled rectangles',
      'rectangles',
      'filled circles',
      'circles',
      'lines'
    );

  var
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Window: ALLEGRO_DISPLAYptr;
    TextFont: ALLEGRO_FONTptr;
    Timer: ALLEGRO_TIMERptr;
  (* Color definitions, *)
    clrBackground, clrText, clrForeground, clrOutline: ALLEGRO_COLOR;
  (* Bitmaps *)
    Pattern, Zoom: ALLEGRO_BITMAPptr;
  (* What primitive to draw. *)
    What: TPrimitives;
  (* Tells if use software.  Otherwise it uses hardware. *)
    Software: Boolean;
  (* ? *)
    Samples: Integer;
  (* Line thickness. *)
    Thickness: Integer;
  (* To know when to update the screen, when to stop. *)
    NeedRedraw, Terminated: Boolean;

(* Program initialization. *)
  function Initialize: Boolean;

    function GetNumSamples: Integer;
    { It uses Allegro config API but TIni object is valid too.}
    var
      lConfig: ALLEGRO_CONFIGptr;
      lValue, lStr: AnsiString;
      lError: Integer;
    begin
      Result := 0;
      lConfig := al_load_config_file ('ex_draw.ini');
      if not Assigned (lConfig) then lConfig := al_create_config;
      lValue := al_get_config_value (lConfig, 'settings', 'samples');
      if lValue <> '' then Val (lValue, Result, lError);
      if lError <> 0 then Result := 0;
      Str (Result, lStr);
      al_set_config_value (lConfig, 'settings', 'samples', lStr);
      al_save_config_file ('ex_draw.ini', lConfig);
      al_destroy_config (lConfig)
    end;

  (* Draw a checker board in the given bitmap. *)
    function CreateCheckboard: ALLEGRO_BITMAPptr;
    var
      lX, lY: Integer;
      lLight, lDark: ALLEGRO_COLOR;
      lBmpLock: ALLEGRO_LOCKED_REGIONptr;
    begin
      Result := al_create_bitmap (BmpWidth, BmpHeight);
      if not Assigned (Result) then Exit (Nil);
      lLight := al_map_rgb_f (1, 1, 1);
      lDark := al_map_rgb_f (1, 0.9, 0.8);

      al_set_target_bitmap (Result);
      lBmpLock := al_lock_bitmap (
        Result,
        al_get_bitmap_format (Result),
        ALLEGRO_LOCK_WRITEONLY
      );
      for lY := 0 to BmpWidth - 1 do
        for lX := 0 to BmpHeight - 1 do
          if ((lX + lY) and 1) <> 0 then
            al_put_pixel (lX, lY, lLight)
          else
            al_put_pixel (lX, lY, lDark);
      al_set_target_backbuffer (Window);
      al_unlock_bitmap (Result)
    end;

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
    Samples := GetNumSamples;
    if Samples > 0 then
    begin
      al_set_new_display_option (ALLEGRO_SAMPLE_BUFFERS, 1, ALLEGRO_REQUIRE);
      al_set_new_display_option (ALLEGRO_SAMPLES, Samples, ALLEGRO_SUGGEST)
    end;
    al_set_new_display_flags (ALLEGRO_WINDOWED);
    Window := al_create_display (wWidth, wHeight);
    if not Assigned (Window) then
    begin
      WriteLn ('Can''t create window.');
      Exit (False)
    end;
  { The timer. }
    Timer := al_create_timer (1 / FPS);
  { Create bitmaps. }
    Pattern := CreateCheckboard;
    Zoom := al_create_bitmap (ZoomWidth, ZoomHeight);
    if not (Assigned (Pattern) and Assigned (Zoom)) then
    begin
      ErrorMessage ('Can''t create bitmaps!');
      Exit (False)
    end;
  (* Colors. *)
    clrBackground := al_color_name ('beige');
    clrForeground := al_color_name ('black');
    clrOutline := al_color_name ('red');
    clrText := al_color_name ('blue');
  { Load text font. }
    TextFont := al_load_font ('data/fixed_font.tga', 0, 0);
    if not Assigned (TextFont) then
    begin
      ErrorMessage ('Error loading fixed_font.tga');
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
    al_register_event_source (EventQueue, al_get_timer_event_source (Timer));
    al_register_event_source (EventQueue, al_get_display_event_source (Window));

    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
    if Assigned (Pattern) then al_destroy_bitmap (Pattern);
    if Assigned (Zoom) then al_destroy_bitmap (Zoom);
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (Timer) then al_destroy_timer (Timer);
    if Assigned (TextFont) then al_destroy_font (TextFont);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Change to next primitive. *)
  procedure NextPrimitive;
  begin
    if What = High (TPrimitives) then
      What := Low (TPrimitives)
    else
      Inc (What)
  end;



(* Draw window content. *)
  procedure UpdateScreen;
  var
    lTextX, lTextY: Real;

  (* Set text position. *)
    procedure SetXY (aX, aY: Integer);
    begin
      lTextX := aX + 0.5;
      lTextY := aY + 0.5
    end;

  (* Helper to draw text. *)
    procedure Printf (const aFmt: String; const aVals: array of const);
    var
      lAllegroState: ALLEGRO_STATE;
    begin
      al_store_state(&lAllegroState, ALLEGRO_STATE_BLENDER);
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_draw_textf (
        TextFont, clrText,
        lTextX, lTextY, 0,
        al_string_to_str (aFmt), aVals
      );
      al_restore_state (lAllegroState);
      lTextY := lTextY + al_get_font_line_height (TextFont)
    end;

    procedure Print (const aText: String); inline;
    begin
      Printf (aText, [])
    end;

  const
    NumRects = 16;
  var
    lX, lY: Real;
    lOutputBitmap: ALLEGRO_BITMAPptr;
    lRects: array [0..(16 * 4) - 1] of Real;
    lColor: ALLEGRO_COLOR;
    lModeName: AnsiString;

    procedure SetUp;
    var
      j, i: Integer;
    begin
      lOutputBitmap := al_get_target_bitmap;
      for j := 0 to 3 do
        for i := 0 to 3 do
        begin
          lRects[(j * 4 + i) * 4 + 0] := 2 + i * 0.25 + i * 7;
          lRects[(j * 4 + i) * 4 + 1] := 2 + j * 0.25 + j * 7;
          lRects[(j * 4 + i) * 4 + 2] := 2 + i * 0.25 + i * 7 + 5;
          lRects[(j * 4 + i) * 4 + 3] := 2 + j * 0.25 + j * 7 + 5;
        end;
      al_clear_to_color (clrBackground)
    end;

    procedure DrawTexts;
    begin
      SetXY (8, 0);
      Printf ('Drawing %s (press SPACE to change)', [PrimitiveNames[What]]);
      SetXY(8, 16);
      Print ('Original');
      SetXY (80, 16);
      Print ('Enlarged x 16');
      SetXY (8, 640 - 48);
      Printf ('Thickness: %d (press T to change)', [Thickness]);
      Printf ('Drawing with: %s (press S to change)', [lModeName]);
      Printf ('Supersampling: %dx (edit ex_draw.ini to change)', [Samples])
    end;

    procedure DrawPrimitive (
      aLeft, aTop, aRight, aBottom: Real;
      aColor: ALLEGRO_COLOR;
      aNotFilled: Boolean
    );
    var
      cx, cy, rx, ry: Real;
      lThickness: Integer;
      lWhat: TPrimitives;
    begin
      cx := (aLeft + aRight) / 2;
      cy := (aTop + aBottom) / 2;
      rx := (aRight - aLeft) / 2;
      ry := (aBottom - aTop) / 2;
      if aNotFilled then lThickness := 0 else lThickness := Thickness;
      lWhat := What;
      if aNotFilled then case lWhat of
        pFilledRect:   lWhat := pRect;
        pFilledCircle: lWhat := pCircle;
      end;
      case lWhat of
      pFilledRect:
        al_draw_filled_rectangle (aLeft, aTop, aRight, aBottom, aColor);
      pRect:
        al_draw_rectangle (aLeft, aTop, aRight, aBottom, aColor, lThickness);
      pFilledCircle:
        al_draw_filled_ellipse (cx, cy, rx, ry, aColor);
      pCircle:
        al_draw_ellipse (cx, cy, rx, ry, aColor, lThickness);
      pLine:
        al_draw_line (aLeft, aTop, aRight, aBottom, aColor, lThickness);
      end
    end;

    procedure DrawTestScene;
    var
      lCnt: Integer;
      lMemBitmap: ALLEGRO_BITMAPptr;
    begin
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
    { If software, draw in a memory bitmap. }
      if Software then
      begin
        al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
        al_set_new_bitmap_format (al_get_bitmap_format (al_get_target_bitmap));
        lMemBitmap := al_create_bitmap (ZoomWidth, ZoomHeight);
        al_set_target_bitmap (lMemBitmap);
        lX := 0;
        lY := 0;
        lModeName := 'software'
      end
      else
      begin
        lMemBitmap := Nil;
        lX := 8;
        lY := 40;
        lModeName := 'hardware'
      end;
    { Background. }
      al_draw_bitmap (Pattern, lX, lY, 0);
    { Draw the test scene. }
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      for lCnt := 0 to (NumRects - 1) do
      begin
        lColor := clrForeground;
        lColor.a := lColor.a / 2;
        DrawPrimitive (
          lX + lRects[lCnt * 4 + 0],
          lY + lRects[lCnt * 4 + 1],
          lX + lRects[lCnt * 4 + 2],
          lY + lRects[lCnt * 4 + 3],
          lColor, False
        )
      end;
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
    { Show memory bitmap if software. }
      if Software then
      begin
        al_set_target_bitmap (lOutputBitmap);
        lX := 8;
        lY := 40;
        al_draw_bitmap(lMemBitmap, lX, lY, 0);
        al_destroy_bitmap (lMemBitmap)
      end
    end;

    procedure DrawEnlarged;
    var i: Integer;
    begin
    { Grab content. }
      al_set_target_bitmap (Zoom);
      al_draw_bitmap_region (
        lOutputBitmap,
        lX, lY, ZoomWidth, ZoomHeight,
        0, 0,
        0
      );
    { Draw enlarged. }
      al_set_target_bitmap (lOutputBitmap);
      lX := 80;
      lY := 40;
      al_draw_scaled_bitmap (Zoom, 0, 0, ZoomWidth, ZoomHeight, lX, lY, ZoomWidth * 16, ZoomHeight * 16, 0);
    { Draw outlines. }
      for i := 0 to (NumRects - 1) do
        DrawPrimitive (
          lX + lRects[i * 4 + 0] * 16,
          lY + lRects[i * 4 + 1] * 16,
          lX + lRects[i * 4 + 2] * 16,
          lY + lRects[i * 4 + 3] * 16,
          clrOutline, True
        )
    end;

  begin
    SetUp;
    DrawTestScene;
    DrawEnlarged;
    DrawTexts;
    al_flip_display;
    NeedRedraw := False
  end;



(* Process keyboard event. *)
  procedure ProcessKeyboardEvent (const aEvent: ALLEGRO_KEYBOARD_EVENT);
  begin
    case aEvent.keycode of
    ALLEGRO_KEY_ESCAPE:
      Terminated := True;
    ALLEGRO_KEY_SPACE:
      NextPrimitive;
    ALLEGRO_KEY_S:
      Software := not Software;
    ALLEGRO_KEY_T:
      begin
        Inc (Thickness);
        if Thickness >= 2 then Thickness := 0
      end;
    end
  end;

begin
  if not Initialize then Exit;
{ "Game loop". }
  Terminated := False;
  NeedRedraw := True;
  al_start_timer (Timer);
  repeat
    if NeedRedraw and al_is_event_queue_empty (EventQueue) then UpdateScreen;
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_KEY_DOWN:
      ProcessKeyboardEvent (Event.keyboard);
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
      NextPrimitive;
    ALLEGRO_EVENT_TIMER:
      NeedRedraw := True;
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
