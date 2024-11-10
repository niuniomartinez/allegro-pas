program ex_12_mouse_cursor;
(* Shows how to change mouse cursor.
 *
 * Example program for the Allegro library, by Peter Wang.
 * Translated to Pascal by Guillermo Martínez J.
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
    allegro5, al5font, al5image;

  type
  (* To store mouse descriptions. *)
    TCursorDescription = record
      SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR;
      Caption: AnsiString
    end;

  const
  (* Window size. *)
    wWidth = 400; wHeight = 400;
    MarginLeft = 20; MarginTop  = 20;
    NumCursors = 20;
  (* List of available cursors. *)
    CursorList: array [1..NumCursors] of TCursorDescription =
    (
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_DEFAULT;     Caption: 'DEFAULT'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_ARROW;       Caption: 'ARROW'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_BUSY;        Caption: 'BUSY'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_QUESTION;    Caption: 'QUESTION'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_EDIT;        Caption: 'EDIT'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_MOVE;        Caption: 'MOVE'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_N;    Caption: 'RESIZE_N'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_W;    Caption: 'RESIZE_W'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_S;    Caption: 'RESIZE_S'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_E;    Caption: 'RESIZE_E'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_NW;   Caption: 'RESIZE_NW'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_SW;   Caption: 'RESIZE_SW'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_SE;   Caption: 'RESIZE_SE'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_NE;   Caption: 'RESIZE_NE'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_PROGRESS;    Caption: 'PROGRESS'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_PRECISION;   Caption: 'PRECISION'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_LINK;        Caption: 'LINK'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_ALT_SELECT;  Caption: 'ALT_SELECT'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_UNAVAILABLE; Caption: 'UNAVAILABLE'),
      (SystemCursor: ALLEGRO_SYSTEM_MOUSE_CURSOR_CUSTOM;      Caption: 'CUSTOM')
    );

  var
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Window: ALLEGRO_DISPLAYptr;
    clrBack, clrFore: ALLEGRO_COLOR;
    TextFont: ALLEGRO_FONTptr;
    TextHeight: Integer;
    Terminated: Boolean;
    CustomCursor: ALLEGRO_MOUSE_CURSORptr;
    CurrentCursor: Integer;

(* Program initialization. *)
  function Initialize: Boolean;
  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_install_mouse
    or not al_init_image_addon
    then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;
  { Create window. }
    al_set_new_display_flags (
      ALLEGRO_WINDOWED or ALLEGRO_GENERATE_EXPOSE_EVENTS
    );
    Window := al_create_display (wWidth, wHeight);
    if not Assigned (Window) then
    begin
      WriteLn ('Can''t create window.');
      Exit (False)
    end;
    clrBack := al_map_rgb (128, 128, 128);
    clrFore := al_map_rgba_f (0, 0, 0, 1);
  { Load text font. }
    TextFont := al_load_bitmap_font ('data/fixed_font.tga');
    if not Assigned (TextFont) then
    begin
      ErrorMessage ('Can''t load data/fixed_font.tga.');
      Exit (False)
    end;
    TextHeight := al_get_font_line_height (TextFont);
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
    if Assigned (CustomCursor) then al_destroy_mouse_cursor (CustomCursor);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Show how to create a custom mouse cursor. *)
  function CreateCustomCursor: ALLEGRO_MOUSE_CURSORptr;
  var
    lBmp, lShrunkBmp: ALLEGRO_BITMAPptr;
  begin
  { Load the bitmap. }
    lBmp := al_load_bitmap ('data/allegro.pcx');
    if not Assigned (lBmp) then
    begin
      ErrorMessage ('Error loading data/allegro.pcx.');
      Exit (Nil)
    end;
  { Create a bitmap for the new cursor. }
    lShrunkBmp := al_create_bitmap (32, 32);
    if not Assigned (lShrunkBmp) then
      Exit (Nil);
  { "Build" the cursor in the bitmap. }
    al_set_target_bitmap (lShrunkBmp);
    al_draw_scaled_bitmap
    (
      lBmp,
      0, 0, al_get_bitmap_width (lBmp), al_get_bitmap_height (lBmp),
      0, 0, 32, 32,
      0
    );
    al_set_target_backbuffer (Window);
  { Create the custom cursor. }
    Result := al_create_mouse_cursor (lShrunkBmp, 0, 0);
  { Release resources. }
    al_destroy_bitmap (lShrunkBmp);
    al_destroy_bitmap (lBmp)
  end;



(* Draw window content. *)
  procedure UpdateScreen;
  var
    i: Integer;
    lCurrent: TCursorDescription;
  begin
    al_clear_to_color (clrBack);
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    i := 0;
    for lCurrent in CursorList do
    begin
      al_draw_text
      (
        TextFont, clrFore,
        MarginLeft, MarginTop + i * TextHeight, 0,
        lCurrent.Caption
      );
      Inc (i)
    end;
    al_draw_text (
      TextFont, clrFore,
      MarginLeft, MarginTop + NumCursors * TextHeight, 0,
     'Press S/H to show/hide cursor'
    );
    al_flip_display
  end;



(* Change cursor. *)
  procedure ChangeCursor (const ay: Integer);

  (* Return the cursor index to use. *)
    function GetCursorIndex: Integer;
    begin
      if ay < MarginTop then Exit (-1);
      Result := ((ay - MarginTop) div TextHeight) + Low (CursorList);
      if Result <= NumCursors then Exit;
      Result := -1
    end;

  var
    lCursor: Integer;
  begin
    lCursor := GetCursorIndex;
    if (lCursor <> CurrentCursor)
    and (Low (CursorList) <= lCursor) and (lCursor <= High (CursorList)) then
    begin
      if CursorList[lCursor].SystemCursor = ALLEGRO_SYSTEM_MOUSE_CURSOR_CUSTOM
      then
        al_set_mouse_cursor (Window, CustomCursor)
      else
        al_set_system_mouse_cursor (Window, CursorList[lCursor].SystemCursor);
      CurrentCursor := lCursor
    end
  end;

begin
  if not Initialize then Exit;
  CustomCursor := CreateCustomCursor;
  if not Assigned (CustomCursor) then Exit;
  UpdateScreen;
  Terminated := False;
  repeat
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_DISPLAY_EXPOSE:
      begin
        al_set_target_backbuffer (Event.display.source);
        UpdateScreen
      end;
    ALLEGRO_EVENT_KEY_DOWN:
      case event.keyboard.keycode of
      ALLEGRO_KEY_ESCAPE:
        Terminated := True;
      ALLEGRO_KEY_H:
        al_hide_mouse_cursor (Event.display.source);
      ALLEGRO_KEY_S:
        al_show_mouse_cursor (Event.display.source);
      end;
    ALLEGRO_EVENT_MOUSE_AXES:
      ChangeCursor (Event.mouse.y);
    end
  until Terminated;
  Finalize
end.
