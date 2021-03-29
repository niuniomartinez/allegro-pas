PROGRAM ex_resize2;
(*
 *    Test program for Allegro.
 *
 *    Resizing the window currently shows broken behaviour.
 *)
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
  allegro5, al5base, al5Image, al5font;

var
  Display: ALLEGRO_DISPLAYptr;
  Bmp: ALLEGRO_BITMAPptr;
  Queue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Font: ALLEGRO_FONTptr;
  Redraw: Boolean;
  MaxStr: AL_STR;

begin
  if not al_init then AbortExample ('Could not init Allegro.');
  al_install_keyboard;
  al_init_image_addon;
  al_init_font_addon;

  al_set_new_display_flags
    (ALLEGRO_RESIZABLE or ALLEGRO_GENERATE_EXPOSE_EVENTS);
  Display := al_create_display (640, 480);
  if Display = Nil then AbortExample ('Unable to set any graphic mode.');

  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
  Bmp := al_load_bitmap ('data/mysha.pcx');
  if Bmp = Nil then AbortExample ('Unable to load image.');

  Font := al_create_builtin_font;

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_display_event_source (Display));
  al_register_event_source (Queue, al_get_keyboard_event_source);

  Redraw := True;
  while True do
  begin
    if Redraw and al_is_event_queue_empty (Queue) then
    begin
      al_clear_to_color (al_map_rgb (255, 0, 0));
      al_draw_scaled_bitmap (
        Bmp,
        0, 0, al_get_bitmap_width (Bmp), al_get_bitmap_height (Bmp),
        0, 0, al_get_display_width (Display), al_get_display_height (Display),
        0
      );
      if (al_get_display_flags (Display) and ALLEGRO_MAXIMIZED) <> 0 then
        MaxStr := 'yes'
      else
        MaxStr := 'no';
      al_draw_multiline_textf (
        Font, al_map_rgb (255, 255, 0),
        0, 0, 640, al_get_font_line_height (Font),
        0,
        'size: %d x %d'#10 +
        'maximized: %s'#10 +
        '+ key to maximize'#10 +
        '- key to un-maximize',
        [
          al_get_display_width (Display),
          al_get_display_height (Display),
          MaxStr
        ]
      );
      al_flip_display;
      Redraw := False
    end;

    al_wait_for_event (Queue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      begin
        al_acknowledge_resize (Event.display.source);
        Redraw := True
      end;
    ALLEGRO_EVENT_DISPLAY_EXPOSE:
      Redraw := True;
    ALLEGRO_EVENT_KEY_DOWN:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
      begin
        al_destroy_bitmap (Bmp);
        al_destroy_display (Display);
        Exit
      end;
    ALLEGRO_EVENT_KEY_CHAR:
      if Event.keyboard.unichar = Ord ('+') then
        al_set_display_flag (Display, ALLEGRO_MAXIMIZED, True)
      else if Event.keyboard.unichar = Ord ('-') then
        al_set_display_flag (Display, ALLEGRO_MAXIMIZED, False);
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      begin
        al_destroy_bitmap (Bmp);
        al_destroy_display (Display);
        Exit
      end;
    end
  end
end.
