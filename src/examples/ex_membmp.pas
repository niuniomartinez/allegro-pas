program ex_membmp;
(*
  Copyright (c) 2012-2020 Guillermo Martínez J.

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
    Allegro5, al5base, al5font, al5Image, al5strings;

  procedure Print (aFont: ALLEGRO_FONTptr; Message: AL_STR; x, y: Integer);
  begin
    al_draw_text (aFont, al_map_rgb (0, 0, 0), x+2, y+2, 0, Message);
    al_draw_text (aFont, al_map_rgb (255, 255, 255), x, y, 0, Message)
  end;



  function Test
    (Bitmap: ALLEGRO_BITMAPptr; Font: ALLEGRO_FONTptr; Message: AL_STR)
    : Boolean;
  var
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    StartTime, FPS: Double;
    Frames: LongInt;
  begin
    Frames := 0;
    FPS := 0;

    Queue := al_create_event_queue;
    al_register_event_source (Queue, al_get_keyboard_event_source);

    StartTime := al_get_time;

    while True do
    begin
      if al_get_next_event (Queue, Event) then
      begin
        if Event.ftype = ALLEGRO_EVENT_KEY_DOWN then
        begin
          if Event.keyboard.keycode = ALLEGRO_KEY_SPACE then
          begin
            al_destroy_event_queue (Queue);
            Exit (False)
          end;
          if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
          begin
            al_destroy_event_queue (Queue);
            Exit (True)
          end
        end
      end;

      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);

    { Clear the backbuffer with red so we can tell if the bitmap does not
      cover the entire backbuffer. }
      al_clear_to_color (al_map_rgb (255, 0, 0));

      al_draw_scaled_bitmap (
        Bitmap, 0, 0,
        al_get_bitmap_width (Bitmap),
        al_get_bitmap_height (Bitmap),
        0, 0,
        al_get_bitmap_width (al_get_target_bitmap),
        al_get_bitmap_height (al_get_target_bitmap),
        0
      );

      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);

    { Note this makes the memory buffer case much slower due to repeated
      locking of the backbuffer.  Officially you can't use al_lock_bitmap
      to solve the problem either. }
      Print (Font, Message, 0, 0);
      Print (
        Font, al_str_format ('%.1f FPS', [FPS]), 0,
        al_get_font_line_height (Font) + 5
      );

      al_flip_display;

      Inc (Frames);
      FPS := Frames / (al_get_time - StartTime)
    end
  end;



  var
    Display: ALLEGRO_DISPLAYptr;
    AccelFont, MemFont: ALLEGRO_FONTptr;
    AccelBmp, MemBmp: ALLEGRO_BITMAPptr;
begin
  if not al_init then AbortExample ('Could not init Allegro.');

  al_install_keyboard;
  al_init_image_addon;
  al_init_font_addon;
  InitPlatformSpecific;

  Display := al_create_display (640, 400);
  if Display = Nil then AbortExample ('Error creating display.');

  AccelFont := al_load_font ('data/font.tga', 0, 0);
  if AccelFont = Nil then AbortExample ('font.tga not found');
  AccelBmp := al_load_bitmap ('data/mysha.pcx');
  if AccelBmp = Nil then AbortExample ('mysha.pcx not found');

  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);

  MemFont := al_load_font ('data/font.tga', 0, 0);
  MemBmp := al_load_bitmap ('data/mysha.pcx');

  repeat
    if Test (MemBmp, MemFont, 'Memory bitmap (press SPACE key)') then Exit;
    if Test (AccelBmp, AccelFont, 'Accelerated bitmap (press SPACE key)') then Exit
  until False
end.
