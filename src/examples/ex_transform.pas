program ex_transform;
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
  common,
  Allegro5, al5base, al5image, al5font, al5primitives, al5strings;

var
  Filename: AL_STR;
  Display: ALLEGRO_DISPLAYptr;
  Buffer, Bitmap, Subbitmap, BufferSubbitmap, Overlay: ALLEGRO_BITMAPptr;
  Timer: ALLEGRO_TIMERptr;
  Queue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Transform, Identity: ALLEGRO_TRANSFORM;
  Software, Redraw, Blend, UseSubbitmap, EndLoop: Boolean;
  w, h: Integer;
  Font, SoftFont: ALLEGRO_FONTptr;
  t: Single;
  Tint: ALLEGRO_COLOR;
begin
  Software := False;
  Redraw := False;
  Blend := False;
  UseSubbitmap := False;

  if Paramcount > 1 then
    Filename := al_string_to_str (ParamStr (1))
  else
    Filename := 'data/mysha.pcx';

  if not al_init then AbortExample ('Could not init Allegro.\n');

  al_init_primitives_addon;
  al_install_mouse;
  al_install_keyboard;

  al_init_image_addon;
  al_init_font_addon;

  Display := al_create_display (640, 480);
  if Display = Nil then AbortExample ('Error creating display');

  Subbitmap := al_create_sub_bitmap (al_get_backbuffer (Display), 50, 50, 640 - 50, 480 - 50);
  Overlay := al_create_sub_bitmap (al_get_backbuffer (Display), 100, 100, 300, 50);

  al_set_window_title (Display, Filename);

  Bitmap := al_load_bitmap (Filename);
  if Bitmap = Nil then
    AbortExample (Filename + ' not found or failed to load');
  Font := al_load_font ('data/bmpfont.tga', 0, 0);
  if Font = Nil then
    AbortExample ('data/bmpfont.tga not found or failed to load');

  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
  Buffer := al_create_bitmap (640, 480);
  BufferSubbitmap := al_create_sub_bitmap (Buffer, 50, 50, 640 - 50, 480 - 50);

  SoftFont := al_load_font ('data/bmpfont.tga', 0, 0);
  if SoftFont = Nil then
    AbortExample ('data/bmpfont.tga not found or failed to load');

  Timer := al_create_timer (1 / 60);
  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_display_event_source (Display));
  al_register_event_source (Queue, al_get_timer_event_source (Timer));
  al_start_timer (Timer);

  w := al_get_bitmap_width (Bitmap);
  h := al_get_bitmap_height (Bitmap);

  al_set_target_bitmap (Overlay);
  al_identity_transform (Transform);
  al_rotate_transform (Transform, -0.06);
  al_use_transform (Transform);

  EndLoop := False;
  repeat
    al_wait_for_event (Queue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EndLoop := True;
    ALLEGRO_EVENT_KEY_DOWN:
      case Event.keyboard.keycode of
      ALLEGRO_KEY_S:
        begin
          Software := not Software;
          if Software then
          begin
          { Restore identity transform on display bitmap. }
            al_identity_transform (Identity);
            al_use_transform (Identity);
          end;
        end;
      ALLEGRO_KEY_L:
        Blend := not Blend;
      ALLEGRO_KEY_B:
        UseSubbitmap := not UseSubbitmap;
      ALLEGRO_KEY_ESCAPE:
        EndLoop := True;
      end;
    ALLEGRO_EVENT_TIMER:
      Redraw := True;
    end;

    if Redraw and al_is_event_queue_empty (Queue) then
    begin
      t := 3 + al_get_time;
      Redraw := False;

      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ONE);
      if Blend then
        Tint := al_map_rgba_f (0.5, 0.5, 0.5, 0.5)
      else
        Tint := al_map_rgba_f (1, 1, 1, 1);

      if Software then
      begin
        al_set_target_bitmap (Buffer);
        if UseSubbitmap then
        begin
          al_clear_to_color (al_map_rgb_f (1, 0, 0));
          al_set_target_bitmap (BufferSubbitmap);
        end;
      end
      else begin
        al_set_target_backbuffer (Display);
        if UseSubbitmap then
        begin;
          al_clear_to_color (al_map_rgb_f (1, 0, 0));
          al_set_target_bitmap (Subbitmap);
        end;
      end;

    { Set the transformation on the target bitmap. }
      al_identity_transform (Transform);
      al_translate_transform (Transform, -640 / 2, -480 / 2);
      al_scale_transform (Transform, 0.15 + sin (t / 5), 0.15 + cos (t / 5));
      al_rotate_transform (Transform, t / 50);
      al_translate_transform (Transform, 640 / 2, 480 / 2);
      al_use_transform (Transform);

    { Draw some stuff. }
      al_clear_to_color (al_map_rgb_f (0, 0, 0));
      al_draw_tinted_bitmap (Bitmap, Tint, 0, 0, 0);
      al_draw_tinted_scaled_bitmap (Bitmap, Tint, w / 4, h / 4, w / 2, h / 2, w, 0, w / 2, h / 4, 0);
      al_draw_tinted_bitmap_region (Bitmap, Tint, w / 4, h / 4, w / 2, h / 2, 0, h, ALLEGRO_FLIP_VERTICAL);
      al_draw_tinted_scaled_rotated_bitmap (Bitmap, Tint, w / 2, h / 2, w + w / 2, h + h / 2, 0.7, 0.7, 0.3, 0);
      al_draw_pixel (w + w / 2, h + h / 2, al_map_rgb_f (0, 1, 0));
      al_put_pixel (w + w div 2 + 2, h + h div 2 + 2, al_map_rgb_f (0, 1, 1));
      al_draw_circle (w, h, 50, al_map_rgb_f (1, 0.5, 0), 3);

      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
      if Software then
      begin
        al_draw_text (SoftFont, al_map_rgba_f (1, 1, 1, 1),
          640 / 2, 430, ALLEGRO_ALIGN_CENTRE, 'Software Rendering'
        );
        al_set_target_backbuffer (Display);
        al_draw_bitmap (Buffer, 0, 0, 0);
      end
      else
        al_draw_text (Font, al_map_rgba_f (1, 1, 1, 1),
          640 / 2, 430, ALLEGRO_ALIGN_CENTRE, 'Hardware Rendering'
        );

    { Each target bitmap has its own transformation matrix, so this
      overlay is unaffected by the transformations set earlier. }
      al_set_target_bitmap (Overlay);
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ONE);
      al_draw_text (Font, al_map_rgba_f (1, 1, 0, 1),
        0, 10, ALLEGRO_ALIGN_LEFT, 'hello!'
      );

      al_set_target_backbuffer (Display);
      al_flip_display;
    end;
  until EndLoop;

  al_destroy_event_queue (Queue);
  al_destroy_timer (Timer);
  al_destroy_font (SoftFont);
  al_destroy_bitmap (BufferSubbitmap);
  al_destroy_bitmap (Buffer);
  al_destroy_font (Font);
  al_destroy_bitmap (Bitmap);
  al_destroy_bitmap (Overlay);
  al_destroy_bitmap (Subbitmap);
  al_destroy_display (Display);
end.
