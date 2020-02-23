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

USES
  Common,
  allegro5, al5base, al5Image, al5font;

VAR
  Display: ALLEGRO_DISPLAYptr;
  Bmp: ALLEGRO_BITMAPptr;
  Queue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Font: ALLEGRO_FONTptr;
  Redraw: BOOLEAN;
  MaxStr: AL_STR;

BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');
  al_install_keyboard;
  al_init_image_addon;
  al_init_font_addon;

  al_set_new_display_flags
    (ALLEGRO_RESIZABLE OR ALLEGRO_GENERATE_EXPOSE_EVENTS);
  Display := al_create_display (640, 480);
  IF Display = NIL THEN AbortExample ('Unable to set any graphic mode.');

  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
  Bmp := al_load_bitmap ('data/mysha.pcx');
  IF Bmp = NIL THEN AbortExample ('Unable to load image.');

  Font := al_create_builtin_font;

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_display_event_source (Display));
  al_register_event_source (Queue, al_get_keyboard_event_source);

  Redraw := TRUE;
  WHILE TRUE DO
  BEGIN
    IF Redraw AND al_is_event_queue_empty (Queue) THEN
    BEGIN
      al_clear_to_color (al_map_rgb (255, 0, 0));
      al_draw_scaled_bitmap (
        Bmp,
        0, 0, al_get_bitmap_width (Bmp), al_get_bitmap_height (Bmp),
        0, 0, al_get_display_width (Display), al_get_display_height (Display),
        0
      );
      IF (al_get_display_flags (Display) AND ALLEGRO_MAXIMIZED) <> 0 THEN
        MaxStr := 'yes'
      ELSE
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
      Redraw := FALSE
    END;

    al_wait_for_event (Queue, @Event);
    CASE Event.ftype OF
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      BEGIN
        al_acknowledge_resize (Event.display.source);
        Redraw := TRUE
      END;
    ALLEGRO_EVENT_DISPLAY_EXPOSE:
      Redraw := TRUE;
    ALLEGRO_EVENT_KEY_DOWN:
      IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
      BEGIN
        al_destroy_bitmap (Bmp);
        al_destroy_display (Display);
        EXIT
      END;
    ALLEGRO_EVENT_KEY_CHAR:
      IF Event.keyboard.unichar = ORD ('+') THEN
        al_set_display_flag (Display, ALLEGRO_MAXIMIZED, TRUE)
      ELSE IF Event.keyboard.unichar = ORD ('-') THEN
        al_set_display_flag (Display, ALLEGRO_MAXIMIZED, FALSE);
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      BEGIN
        al_destroy_bitmap (Bmp);
        al_destroy_display (Display);
        EXIT
      END;
    END
  END
END.
