program ex_bitmap_memory;
(* Shows how RAM and VRAM bitmaps affect the execution.

   It shows the difference in Frames Per Second but you should look the CPU
   usage too.
 *)
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
    allegro5   in '../lib/allegro5.pas',
    al5font    in '../lib/al5font',
    al5image   in '../lib/al5image.pas',
    al5strings in '../lib/al5strings.pas';

  const
  (* Window size. *)
    wWidth = 640; wHeight = 480;
  var
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Window: ALLEGRO_DISPLAYptr;
    clrWhite, clrBlack, clrRed: ALLEGRO_COLOR;
    AccelFont, MemFont: ALLEGRO_FONTptr;
    AccelBmp, MemBmp: ALLEGRO_BITMAPptr;

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
  { Create window. }
    al_set_new_display_flags (ALLEGRO_WINDOWED);
    Window := al_create_display (wWidth, wHeight);
    if not Assigned (Window) then
    begin
      WriteLn ('Can''t create window.');
      Exit (False)
    end;
    clrWhite := al_map_rgb (255, 255, 255);
    clrBlack := al_map_rgb (0, 0, 0);
    clrRed := al_map_rgb (255, 0, 0);
  { Load VRAM stuff. }
    al_set_new_bitmap_flags (ALLEGRO_VIDEO_BITMAP);
    AccelBmp := al_load_bitmap ('data/mysha.pcx');
    if not Assigned (AccelBmp) then
    begin
      ErrorMessage ('Can''t load data/mysha.pcx!');
      Exit (False)
    end;
    AccelFont := al_load_font ('data/font.tga', 0, 0);
    if not Assigned (AccelFont) then
    begin
      ErrorMessage ('Can''t load data/font.tga!');
      Exit (False)
    end;
  { Load RAM stuff. }
    al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
    MemBmp := al_load_bitmap ('data/mysha.pcx');
    MemFont := al_load_font ('data/font.tga', 0, 0);
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
    if Assigned (AccelBmp) then al_destroy_bitmap (AccelBmp);
    if Assigned (MemBmp) then al_destroy_bitmap (MemBmp);
    if Assigned (AccelFont) then al_destroy_font (AccelFont);
    if Assigned (MemFont) then al_destroy_font (MemFont);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Does the test.

   This just draws the given bitmap on the window and calculates how many FPS
   (Frames Per Second) it does.

   Returns True if exit, False if continue.
 *)
  function Test (
    aBitmap: ALLEGRO_BITMAPptr;
    aFont: ALLEGRO_FONTptr;
    aMessage: String
  ): Boolean;
  var
    lStartTime, lFPS: Double;
    lFrames: LongInt;
    lTerminated: Boolean;

  (* Helper to print text. *)
    procedure Print (aFont: ALLEGRO_FONTptr; Message: String; x, y: Integer);
    begin
      al_draw_text (
        aFont, clrBlack,
        x+2, y+2, 0,
        al_string_to_str (Message)
      );
      al_draw_text (
        aFont, clrWhite,
        x, y, 0,
        al_string_to_str (Message)
      )
    end;

    procedure CheckEvents;
    begin
      if al_get_next_event (EventQueue, Event) then
      begin
        case Event.ftype of
        ALLEGRO_EVENT_KEY_DOWN:
          begin
            case Event.keyboard.keycode of
            ALLEGRO_KEY_SPACE:
              begin
                Result := False;
                lTerminated := True
              end;
            ALLEGRO_KEY_ESCAPE:
              begin
                Result := True;
                lTerminated := True
              end;
            end
          end;
        ALLEGRO_EVENT_DISPLAY_CLOSE:
          begin
            Result := True;
            lTerminated := True
          end;
        end
      end
    end;

    procedure UpdateScreen;
    begin
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
    { Clear the backbuffer with red so we can tell if the bitmap does not
      cover the entire backbuffer. }
      al_clear_to_color (clrRed);
      al_draw_scaled_bitmap (
        aBitmap, 0, 0,
        al_get_bitmap_width (aBitmap),
        al_get_bitmap_height (aBitmap),
        0, 0,
        al_get_bitmap_width (al_get_target_bitmap),
        al_get_bitmap_height (al_get_target_bitmap),
        0
      );
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    { Note this makes the memory buffer case much slower due to repeated
      locking of the backbuffer.  Officially you can't use al_lock_bitmap
      to solve the problem either. }
      Print (aFont, aMessage, 0, 0);
      Print (
        aFont, al_str_to_string (al_str_format ('%.1f FPS', [lFPS])), 0,
        al_get_font_line_height (aFont) + 5
      );
      al_flip_display
    end;

  begin
    lFrames := 0; lFPS := 0;
    lStartTime := al_get_time;
    lTerminated := False;
    repeat
      CheckEvents;
      UpdateScreen;
      Inc (lFrames);
      lFPS := lFrames / (al_get_time - lStartTime)
    until lTerminated
  end;

begin
  if not initialize then Exit;
{ Do the testing. }
  while true do
  begin
    if Test (MemBmp, MemFont, 'Memory bitmap (press SPACE key)') then
      Exit;
    if Test (AccelBmp, AccelFont, 'Accelerated bitmap (press SPACE key)') then
      Exit
  end
end.
