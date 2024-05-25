program ex_03_bitmap;
(* Show how to load a bitmap and draw it. *)
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
    allegro5, al5image, al5strings;

  const
  (* Window size. *)
    wWidth = 800; wHeight = 600;
  var
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Window: ALLEGRO_DISPLAYptr;
    Filename: String;
    Bitmap: ALLEGRO_BITMAPptr;
    bWidth, bHeight: Integer;
    Terminated: Boolean;

(* Program initialization. *)
  function Initialize: Boolean;
  begin
  { The first commandline argument can optionally specify an
    image to display instead of the default.  Allegro's image
    addon supports BMP, DDS, PCX, TGA and can be compiled with
    PNG and JPG support on all platforms.  Additional formats
    are supported by platform specific libraries and support for
    image formats can also be added at runtime.
  }
    if ParamCount > 0 then
      FileName := ParamStr (1)
    else
      FileName := 'data/mysha.pcx';
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;
  { Initialize the image addon. }
    if not al_init_image_addon then
    begin
      WriteLn ('Can''t initialize image loaders.');
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
  { Load bitmap. }
    Bitmap := al_load_bitmap (al_string_to_str (FileName));
    if not Assigned (Bitmap) then
    begin
    { Close window so it doesn't obscure the console. }
      al_destroy_display (Window); Window := Nil;
      WriteLn ('Can''t load "', FileName, '".');
      Exit (False)
    end;
  { Create the event queue. }
    EventQueue := al_create_event_queue;
    if not Assigned (EventQueue) then
    begin
    { Close window so it doesn't obscure the console. }
      al_destroy_display (Window); Window := Nil;
      WriteLn ('Can''t initialize event queue!');
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
