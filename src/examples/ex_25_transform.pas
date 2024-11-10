program ex_25_transform;
(* Shows how to use transformation matrices. *)
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
    Allegro5, al5image, al5font, al5primitives, al5strings;

  const
  (* Window size. *)
    wWidth = 640; wHeight = 480;
  var
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Window: ALLEGRO_DISPLAYptr;
    Buffer, Bitmap, Subbitmap, BufferSubbitmap, Overlay: ALLEGRO_BITMAPptr;
    Software, Redraw, Blend, UseSubbitmap: Boolean;
    bWidth, bHeight: Integer;
    DeltaTime: Single;
    TextFont, SoftTextFont: ALLEGRO_FONTptr;
    Timer: ALLEGRO_TIMERptr;
    Terminated: Boolean;

(* Program initialization. *)
  function Initialize: Boolean;
  var
    lFilename: String;
    lTransform: ALLEGRO_TRANSFORM;
  begin
  { The first commandline argument can optionally specify an
    image to display instead of the default.
  }
    if ParamCount > 0 then
      lFileName := ParamStr (1)
    else
      lFileName := 'data/mysha.pcx';
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_init_image_addon
    or not al_init_font_addon or not al_init_primitives_addon
    then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;
  { Create the timer. }
    Timer := al_create_timer (1 / 60);
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
  { Prepare bitmaps. }
    Subbitmap := al_create_sub_bitmap (
      al_get_backbuffer (Window),
      50, 50,
      640 - 50, 480 - 50
    );
    Overlay := al_create_sub_bitmap (
      al_get_backbuffer (Window),
      100, 100,
      300, 50
    );
    Bitmap := al_load_bitmap (al_string_to_str (lFileName));
    if not Assigned (Bitmap) then
    begin
      ErrorMessage (Concat ('Can''t load "', lFileName, '".'));
      Exit (False)
    end;
    bWidth := al_get_bitmap_width (Bitmap);
    bHeight := al_get_bitmap_height (Bitmap);
  { Load text font. }
    TextFont := al_load_font ('data/bmpfont.tga', 0, 0);
    if not Assigned (TextFont) then
    begin
      ErrorMessage ('data/bmpfont.tga not found or failed to load');
      Exit (False)
    end;
  { Software bitmap and text font. }
    al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
    Buffer := al_create_bitmap (wWidth, wHeight);
    BufferSubbitmap := al_create_sub_bitmap (
      Buffer,
      50, 50,
      640 - 50, 480 - 50
    );
    SoftTextFont := al_load_font ('data/bmpfont.tga', 0, 0);
    if not Assigned (SoftTextFont) then
    begin
      ErrorMessage ('data/bmpfont.tga not found or failed to load');
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
  { Initial values. }
    Software := False;
    Blend := False;
    UseSubbitmap := True;
  { Set overlay rotation. }
    al_set_target_bitmap (Overlay);
    al_identity_transform (lTransform);
    al_rotate_transform (lTransform, -0.06);
    al_use_transform (lTransform);

    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (Timer) then al_destroy_timer (Timer);
    if Assigned (Subbitmap) then al_destroy_bitmap (Subbitmap);
    if Assigned (Overlay) then al_destroy_bitmap (Overlay);
    if Assigned (Bitmap) then al_destroy_bitmap (Bitmap);
    if Assigned (Buffer) then al_destroy_bitmap (Buffer);
    if Assigned (BufferSubbitmap) then al_destroy_bitmap (BufferSubbitmap);
    if Assigned (TextFont) then al_destroy_font (TextFont);
    if Assigned (SoftTextFont) then al_destroy_font (SoftTextFont);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Draw window content. *)
  procedure UpdateScreen;
  var
    lTint: ALLEGRO_COLOR;
    lTransform: ALLEGRO_TRANSFORM;
  begin
    DeltaTime := 3 + al_get_time;
  { Set blender function. }
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ONE);
    if Blend then
      lTint := al_map_rgba_f (0.3, 0.6, 0.6, 0.5)
    else
      lTint := al_map_rgba_f (1, 1, 1, 1);
  { Set target. }
    if Software then
    begin
      al_set_target_bitmap (Buffer);
      if UseSubbitmap then
      begin
        al_clear_to_color (al_map_rgb_f (1, 0, 0));
        al_set_target_bitmap (BufferSubbitmap)
      end
    end
    else begin
      al_set_target_backbuffer (window);
      if UseSubbitmap then
      begin;
        al_clear_to_color (al_map_rgb_f (1, 0, 0));
        al_set_target_bitmap (Subbitmap)
      end
    end;
  { Set the transformation on the target bitmap. }
    al_identity_transform (lTransform);
    al_translate_transform (lTransform, -wWidth / 2, -wHeight / 2);
    al_scale_transform (
      lTransform,
      0.15 + sin (DeltaTime / 5),
      0.15 + cos (DeltaTime / 5)
    );
    al_rotate_transform (lTransform, DeltaTime / 50);
    al_translate_transform (lTransform, wWidth / 2, wHeight / 2);
    al_use_transform (lTransform);
  { Draw some stuff. }
    al_clear_to_color (al_map_rgb_f (0, 0, 0));
    al_draw_tinted_bitmap (Bitmap, lTint, 0, 0, 0);
    al_draw_tinted_scaled_bitmap (
      Bitmap,
      lTint,
      bWidth / 4, bHeight / 4,
      bWidth / 2, bHeight / 2,
      bWidth, 0, bWidth / 2,
      bHeight / 4,
      0
    );
    al_draw_tinted_bitmap_region (
      Bitmap,
      lTint,
      bWidth / 4, bHeight / 4,
      bWidth / 2, bHeight / 2,
      0, bHeight,
      ALLEGRO_FLIP_VERTICAL
    );
    al_draw_tinted_scaled_rotated_bitmap (
      Bitmap,
      lTint,
      bWidth / 2, bHeight / 2,
      bWidth + bWidth / 2, bHeight + bHeight / 2,
      0.7, 0.7, 0.3,
      0
    );
    al_draw_pixel (
      bWidth + bWidth / 2,
      bHeight + bHeight / 2,
      al_map_rgb_f (0, 1, 0)
    );
    al_put_pixel (
      bWidth + bWidth div 2 + 2,
      bHeight + bHeight div 2 + 2,
      al_map_rgb_f (1, 1, 0)
    );
    al_draw_circle (bWidth, bHeight, 50, al_map_rgb_f (1, 0.5, 0), 3);
  { Draw other stuff with other blender function. }
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
    if Software then
    begin
      al_draw_text (
        SoftTextFont, al_map_rgba_f (1, 1, 1, 1),
        640 / 2, 430, ALLEGRO_ALIGN_CENTRE,
        'Software Rendering'
      );
      al_set_target_backbuffer (Window);
      al_draw_bitmap (Buffer, 0, 0, 0)
    end
    else
      al_draw_text (
        TextFont, al_map_rgba_f (1, 1, 1, 1),
        640 / 2, 430, ALLEGRO_ALIGN_CENTRE,
        'Hardware Rendering'
      );
  { Each target bitmap has its own transformation matrix, so this
    overlay is unaffected by the transformations set earlier.
  }
    al_set_target_bitmap (Overlay);
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ONE);
    al_draw_text (
      TextFont, al_map_rgba_f (1, 1, 0, 1),
      0, 10, ALLEGRO_ALIGN_LEFT,
      'hello!'
    );
  { Show result. }
    al_set_target_backbuffer (Window);
    al_flip_display
  end;



(* Process keyboard event. *)
  procedure ProcessKeyboardEvent (const aEvent: ALLEGRO_KEYBOARD_EVENT);
  var
    lIdentity: ALLEGRO_TRANSFORM;
  begin
    case aEvent.keycode of
    ALLEGRO_KEY_S:
      begin
        Software := not Software;
        if Software then
        begin
        { Restore identity transform on display bitmap. }
          al_identity_transform (lIdentity);
          al_use_transform (lIdentity)
        end
      end;
    ALLEGRO_KEY_L:
      Blend := not Blend;
    ALLEGRO_KEY_B:
      UseSubbitmap := not UseSubbitmap;
    ALLEGRO_KEY_ESCAPE:
      Terminated := True;
    end
  end;



begin
  if not Initialize then Exit;
{ "Game loop". }
  al_start_timer (Timer);
  Redraw := False;
  Terminated := False;
  repeat
  { Screen update. }
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
    ALLEGRO_EVENT_KEY_DOWN:
      ProcessKeyboardEvent (Event.keyboard);
    ALLEGRO_EVENT_TIMER:
      Redraw := True;
    end;
  until Terminated;
{ Program finalization. }
  Finalize
end.
