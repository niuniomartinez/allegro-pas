program ex_26_transform_projection;
(* Show how to use transformation matrices to render 3D models. *)
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
    allegro5, al5image, al5font, al5primitives, al5strings,
    Math;

  const
  (* Window size. *)
    wWidth = 800; wHeight = 600;
  (* Rotation speed. *)
    RotationSpeed = 0.025;

  var
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Window: ALLEGRO_DISPLAYptr;
  (* A sub-bitmap of the display with perspective transformation. *)
    DisplaySubPerspective,
  (* A sub-bitmap of the display witth orthographic transformation. *)
    DisplaySubOrtho,
  (* Perspective transformation, purposefully non-POT *)
    Buffer: ALLEGRO_BITMAPptr;
    TextFont: ALLEGRO_FONTptr;
    Timer: ALLEGRO_TIMERptr;
    Redraw, FullScreen, Background, Terminated: Boolean;
  (* Model texture. *)
    ModelTexture: ALLEGRO_BITMAPptr;
  (* Model vertices. *)
    ModelVtx: array [0..4] of ALLEGRO_VERTEX;
  (* Model rotation. *)
    Theta: Real;
  (* Triangle indices to vertices.

     Note that FPC allows this array to be constant but Delphi doesn't allow it.
   *)
    TriangleIndices: array [0..11] of Integer = (
      0, 1, 2,
      0, 2, 3,
      0, 3, 4,
      0, 4, 1
    );



 
(* Set transformation matrix to perspective. *)
  procedure SetPerspectiveTransform (aBmp: ALLEGRO_BITMAPptr);
  var
    lTransform: ALLEGRO_TRANSFORM;
    lAspectRatio: Real;
  begin
    lAspectRatio := al_get_bitmap_height (aBmp) / al_get_bitmap_width (aBmp);
    al_set_target_bitmap (aBmp);
    al_identity_transform (lTransform);
    al_perspective_transform (
      lTransform,
      -1,  lAspectRatio, 1,
       1, -lAspectRatio, 1000
    );
    al_use_projection_transform (lTransform)
  end;



(* Render a pyramid. *)
  procedure DrawPyramid (aTexture: ALLEGRO_BITMAPptr; aX, aY, aZ, aTheta: Real);
  var
    lTransform: ALLEGRO_TRANSFORM;
  begin
  { Build transformation matrix. }
    al_identity_transform (lTransform);
    al_rotate_transform_3d (lTransform, 0, 1, 0, aTheta);
    al_translate_transform_3d (lTransform, aX, aY, aZ);
  { Render. }
    al_use_transform (lTransform);
    al_draw_indexed_prim (
      ModelVtx,
      aTexture,
      TriangleIndices, 12,
      ALLEGRO_PRIM_TRIANGLE_LIST
    )
  end;



(* Program initialization. *)
  function Initialize: Boolean;

  (* Populate model data. *)
    function LoadModelData: Boolean;
    const
    (* Pyramid vertices. *)
      vX: array [0..4] of Real = (0, -1,  1,  1, -1);
      vY: array [0..4] of Real = (1, -1, -1, -1, -1);
      vZ: array [0..4] of Real = (0, -1, -1,  1,  1);
    (* Texture mapping. *)
      tU: array [0..4] of Integer = ( 0, 0, 64, 64, 64);
      tV: array [0..4] of Integer = (64, 0, 64,  0, 64);
    var
      lColor: ALLEGRO_COLOR;
      lNdx: Integer;
    begin
    { Vertices. }
      lColor := al_map_rgb_f (1, 1, 1);
      for lNdx := 0 to 4 do
      begin
        ModelVtx[lNdx].x := vX[lNdx];
        ModelVtx[lNdx].y := vY[lNdx];
        ModelVtx[lNdx].z := vZ[lNdx];
        ModelVtx[lNdx].u := tU[lNdx];
        ModelVtx[lNdx].v := tV[lNdx];
        ModelVtx[lNdx].color := lColor
      end;
    { Load texture. }
      al_set_new_bitmap_flags (
        ALLEGRO_MIN_LINEAR or ALLEGRO_MAG_LINEAR or ALLEGRO_MIPMAP
      );
      ModelTexture := al_load_bitmap ('data/bkg.png');
      if not Assigned (ModelTexture) then
      begin
        ErrorMessage ('Could not load "data/bkg.png".');
        Exit (False)
      end;
      Exit (True)
    end;

  begin
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
    al_set_new_display_flags (ALLEGRO_RESIZABLE);
    al_set_new_display_option (ALLEGRO_DEPTH_SIZE, 16, ALLEGRO_SUGGEST);
  { Load everything as a POT bitmap to make sure the projection stuff works with
    mismatched backing texture and bitmap sizes.
  }
    al_set_new_display_option (ALLEGRO_SUPPORT_NPOT_BITMAP, 0, ALLEGRO_REQUIRE);
    Window := al_create_display (wWidth, wHeight);
    if not Assigned (Window) then
    begin
      WriteLn ('Can''t create window.');
      Exit (False)
    end;
    al_set_window_constraints (Window, 256, 512, 0, 0);
    al_apply_window_constraints (Window, True);
    SetPerspectiveTransform (al_get_backbuffer (Window));
  { BItmaps and sub-bitmaps. }
    DisplaySubPerspective := al_create_sub_bitmap (
      al_get_backbuffer (Window),
      0, 0, 256, 256
    );
    SetPerspectiveTransform (DisplaySubPerspective);
    DisplaySubOrtho := al_create_sub_bitmap (
      al_get_backbuffer (Window),
      0, 0, 256, 512
    );
    Buffer := al_create_bitmap (200, 200);
    SetPerspectiveTransform (Buffer);
  { Create text font. }
    TextFont := al_create_builtin_font;
    if not Assigned (TextFont) then
    begin
      ErrorMessage ('Error creating text font.');
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
  { Other values. }
    if not LoadModelData then Exit (False);
    FullScreen := False;
    Background := False;
    Theta := 0;

    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (Timer) then al_destroy_timer (Timer);
    if Assigned (TextFont) then al_destroy_font (TextFont);
    if Assigned (DisplaySubPerspective) then al_destroy_bitmap (DisplaySubPerspective);
    if Assigned (DisplaySubOrtho) then al_destroy_bitmap (DisplaySubOrtho);
    if Assigned (Buffer) then al_destroy_bitmap (Buffer);
    if Assigned (ModelTexture) then al_destroy_bitmap (ModelTexture);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Draw window content. *)
  procedure UpdateScreen;
  begin
  { If window is in background (i.e. minimized) then don't draw anything. }
    if Background then Exit;
  { Render the pyramid directly in the display. }
    al_set_target_backbuffer (Window);
    al_set_render_state (ALLEGRO_DEPTH_TEST, 1);
    al_clear_to_color (al_map_rgb_f (0, 0, 0));
    al_clear_depth_buffer (1000);
    DrawPyramid (ModelTexture, 0, 0, -4, Theta);
  { Render the pyramid in a bitmap. }
    al_set_target_bitmap (Buffer);
    al_set_render_state (ALLEGRO_DEPTH_TEST, 1);
    al_clear_to_color (al_map_rgb_f (0, 0.1, 0.1));
    al_clear_depth_buffer (1000);
    DrawPyramid (ModelTexture, 0, 0, -4, Theta);
  { Render the pyramid in a sub-bitmap. }
    al_set_target_bitmap (DisplaySubPerspective);
    al_set_render_state (ALLEGRO_DEPTH_TEST, 1);
    al_clear_to_color (al_map_rgb_f (0, 0, 0.25));
    al_clear_depth_buffer (1000);
    DrawPyramid (ModelTexture, 0, 0, -4, Theta);
  { Paste the Buffer and render text. }
    al_set_target_bitmap (DisplaySubOrtho);
    al_set_render_state (ALLEGRO_DEPTH_TEST, 0);
    al_draw_text (
      TextFont, al_map_rgb_f (1, 1, 1),
      128, 16,
      ALLEGRO_ALIGN_CENTER,
      'Press Space to toggle fullscreen'
    );
    al_draw_bitmap (Buffer, 0, 256, 0);

    al_flip_display
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
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      begin
        al_acknowledge_resize (Window);
        SetPerspectiveTransform (al_get_backbuffer (Window))
      end;
    ALLEGRO_EVENT_DISPLAY_HALT_DRAWING:
      begin
        Background := True;
        al_acknowledge_drawing_halt (Window);
        al_stop_timer (Timer)
      end;
    ALLEGRO_EVENT_DISPLAY_RESUME_DRAWING:
      begin
        Background := False;
        al_acknowledge_drawing_resume (Window);
        al_start_timer (Timer)
      end;
    ALLEGRO_EVENT_KEY_DOWN:
      case Event.keyboard.keycode of
      ALLEGRO_KEY_ESCAPE:
        Terminated := True;
      ALLEGRO_KEY_SPACE:
        begin
          Fullscreen := not Fullscreen;
          al_set_display_flag (Window, ALLEGRO_FULLSCREEN_WINDOW, Fullscreen);
          SetPerspectiveTransform (al_get_backbuffer (Window));
        end;
      end;
    ALLEGRO_EVENT_TIMER:
      begin
        Redraw := True;
        Theta := fmod (Theta + RotationSpeed, ALLEGRO_TAU)
      end;
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
