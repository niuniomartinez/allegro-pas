program ex_04_bitmap_target;
(* Show how to change drawing target. *)
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
    al5image      in '../lib/al5image.pas',
    al5primitives in '../lib/al5primitives.pas',
    al5strings    in '../lib/al5strings.pas';

  const
  (* Window size. *)
    wWidth = 800; wHeight = 500;
  var
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Window: ALLEGRO_DISPLAYptr;
    Bitmap: ALLEGRO_BITMAPptr;
    bWidth, bHeight: Integer;
    Terminated: Boolean;

(* Program initialization. *)
  function Initialize: Boolean;
  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_init_image_addon
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
  begin
  { Allegro will destroy most objects at exit but it is a good idea to get used
    to destroy all created objects.
  }
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (Bitmap) then al_destroy_bitmap (Bitmap);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Create a new bitmap and draw things in it. *)
  function CreateBitmap: ALLEGRO_BITMAPptr;
  var
    lBitmap: ALLEGRO_BITMAPptr;
  begin
  { Load background. }
    Result := al_load_bitmap ('data/allegro.pcx');
    if not Assigned (Result) then
    begin
      ErrorMessage ('Can''t load "data/allegro.pcx".');
      Exit
    end;
  { Load foreground. }
    lBitmap := al_load_bitmap ('data/mysha256x256.png');
    if not Assigned (lBitmap) then
    begin
      al_destroy_bitmap (Result);
      ErrorMessage ('Can''t load "data/mysha256x256.png.');
      Exit (Nil)
    end;
  { Change target to background bitmap. }
    al_set_target_bitmap (Result);
  { Draw foreground on background bitmap. }
    al_draw_scaled_bitmap (
      lBitmap,
      0, 0, al_get_bitmap_width (lBitmap), al_get_bitmap_height (lBitmap),
      50, 100, 100, 100,
      0
    );
  { Draw a frame on background bitmap. }
    al_draw_rectangle (
      0, 0, al_get_bitmap_width (Result), al_get_bitmap_height (Result),
      al_map_rgb (0, 0, 0),
      2
    );
  { Restore display as target. }
    al_set_target_backbuffer (Window);
  { Destroy foreground as we don't need it anymore. }
    al_destroy_bitmap (lBitmap)
  end;



(* Draw window content. *)
  procedure UpdateScreen;
  begin
    al_draw_scaled_bitmap (
      Bitmap,
      0, 0, bWidth, bHeight,
      0, 0, wWidth, wHeight,
      0
    );
    al_draw_bitmap (Bitmap, 0, 0, 0);
    al_draw_bitmap (Bitmap, wWidth - bWidth, 0, ALLEGRO_FLIP_HORIZONTAL);
    al_draw_rotated_bitmap (
      Bitmap,
      bWidth / 2, bHeight / 2,
      wWidth - (bWidth / 1.5), wHeight - (bHeight / 1.5),
      ALLEGRO_TAU / 8,
      0
    );
    al_flip_display
  end;

begin
  if not Initialize then Exit;
  Bitmap := CreateBitmap;
  if not Assigned (Bitmap) then Exit;
{ Get bitmap sizes. }
  bWidth := al_get_bitmap_width (Bitmap);
  bHeight := al_get_bitmap_height (Bitmap);
{ "Game loop". }
  Terminated := False;
  repeat
    if al_is_event_queue_empty (EventQueue) then UpdateScreen;
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_KEY_DOWN:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
        Terminated := True;
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
