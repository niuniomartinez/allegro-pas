PROGRAM ex_bitmap_flip;
(* An example showing bitmap flipping flags, by Steven Wallace. *)
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
    Allegro5, al5base, al5image, al5font;

  CONST
    INTERVAL = 0.01;

  VAR
    BmpX: SINGLE = 200;
    BmpY: SINGLE = 200;
    BmpDx: SINGLE = 96;
    BmpDy: SINGLE = 96;
    BmpFlag: INTEGER = 0;



  PROCEDURE Update (Bmp: ALLEGRO_BITMAPptr);
  VAR
    Target: ALLEGRO_BITMAPptr;
    DisplayW, DisplayH, BitmapW, BitmapH: INTEGER;
  BEGIN
    Target := al_get_target_bitmap;
    DisplayW := al_get_bitmap_width (Target);
    DisplayH := al_get_bitmap_height (Target);
    BitmapW := al_get_bitmap_width (Bmp);
    BitmapH := al_get_bitmap_height (Bmp);

    BmpX := BmpX + BmpDx * INTERVAL;
    BmpY := BmpY + BmpDy * INTERVAL;

  { Make sure bitmap is still on the screen. }
    IF BmpY < 0 THEN
    BEGIN
      BmpY := 0;
      BmpDy := BmpDy * -1;
      BmpFlag := BmpFlag AND (NOT ALLEGRO_FLIP_VERTICAL)
    END;

    IF BmpX < 0 THEN
    BEGIN
      BmpX := 0;
      BmpDx := BmpDx * -1;
      BmpFlag := BmpFlag AND (NOT ALLEGRO_FLIP_HORIZONTAL)
    END;

    IF BmpY > DisplayH - BitmapH THEN
    BEGIN
      BmpY := DisplayH - BitmapH;
      BmpDy := BmpDy * -1;
      BmpFlag := BmpFlag OR ALLEGRO_FLIP_VERTICAL
    END;

    IF BmpX > DisplayW - BitmapW THEN
    BEGIN
      BmpX := DisplayW - BitmapW;
      BmpDx := BmpDx * -1;
      BmpFlag := BmpFlag OR ALLEGRO_FLIP_HORIZONTAL
    END
  END;

VAR
  Display: ALLEGRO_DISPLAYptr;
  Timer: ALLEGRO_TIMERptr;
  Queue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Bmp, MemBmp, DispBmp: ALLEGRO_BITMAPptr;
  Font: ALLEGRO_FONTptr;
  aText: AL_STR;
  Done, Redraw: BOOLEAN;
BEGIN
  Done := FALSE;
  redraw := TRUE;

  IF NOT al_init THEN AbortExample ('Failed to init Allegro.');

  IF NOT al_init_image_addon THEN AbortExample ('Failed to init IIO addon.');

  al_init_font_addon;
  InitPlatformSpecific;

  Display := al_create_display (640, 480);
  IF Display = NIL THEN AbortExample ('Error creating display.');

  IF NOT al_install_keyboard THEN AbortExample ('Error installing keyboard.');

  Font := al_load_font ('data/fixed_font.tga', 0, 0);
  IF Font = NIL THEN AbortExample ('Error loading data/fixed_font.tga');

  Bmp := al_load_bitmap ('data/mysha.pcx');
  IF Bmp = NIL THEN AbortExample ('Error loading data/mysha.pcx');
  DispBmp := Bmp;
  aText := 'Display bitmap (space to change)';

  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
  MemBmp := al_load_bitmap ('data/mysha.pcx');
  IF MemBmp = NIL THEN AbortExample ('Error loading data/mysha.pcx');

  Timer := al_create_timer (INTERVAL);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_timer_event_source (Timer));
  al_register_event_source (Queue, al_get_display_event_source (Display));

  al_start_timer (Timer);

  al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);

  WHILE NOT Done DO
  BEGIN
    IF Redraw AND al_is_event_queue_empty (Queue) THEN
    BEGIN
      Update (Bmp);
      al_clear_to_color (al_map_rgb_f (0, 0, 0));
      al_draw_tinted_bitmap (
        Bmp, al_map_rgba_f (1, 1, 1, 0.5),
        BmpX, BmpY, BmpFlag
      );
      al_draw_text (Font, al_map_rgba_f (1, 1, 1, 0.5), 0, 0, 0, aText);
      al_flip_display;
      Redraw := false
    END;

    al_wait_for_event (Queue, @Event);
    CASE Event.ftype OF
    ALLEGRO_EVENT_KEY_DOWN:
      IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
        Done := TRUE
      ELSE IF Event.keyboard.keycode = ALLEGRO_KEY_SPACE THEN
      BEGIN
        IF Bmp = MemBmp THEN
        BEGIN
          Bmp := DispBmp;
          aText := 'Display bitmap (space to change)'
        END
        ELSE BEGIN
          Bmp := MemBmp;
          aText := 'Memory bitmap (space to change)'
        END
      END;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Done := TRUE;
    ALLEGRO_EVENT_TIMER:
      Redraw := TRUE;
    END
  END;

  al_destroy_bitmap (Bmp)
END.
