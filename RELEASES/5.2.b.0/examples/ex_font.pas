PROGRAM ex_font;
(* An example. *)
(*
  Copyright (c) 2012-2018 Guillermo Martínez J.

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
    Allegro5, al5image, al5font;

  PROCEDURE WaitForEsc (Display: ALLEGRO_DISPLAYptr);
  VAR
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    ScreenClone: ALLEGRO_BITMAPptr;
    EndLoop: BOOLEAN;
    x, y, w, h: INTEGER;
  BEGIN
    al_install_keyboard;
    Queue := al_create_event_queue;
    al_register_event_source (Queue, al_get_keyboard_event_source);
    al_register_event_source (Queue, al_get_display_event_source (Display));
    ScreenClone := al_clone_bitmap (al_get_target_bitmap);
    EndLoop := FALSE;
    REPEAT
      al_wait_for_event (Queue, Event);
      IF Event._type = ALLEGRO_EVENT_DISPLAY_CLOSE THEN
	EndLoop := TRUE
      ELSE IF Event._type = ALLEGRO_EVENT_KEY_DOWN THEN
	BEGIN
	  IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
	    EndLoop := TRUE
	END
      ELSE IF Event._type = ALLEGRO_EVENT_DISPLAY_EXPOSE THEN
	BEGIN
	  x := Event.display.x;
	  y := Event.display.y;
	  w := Event.display.width;
	  h := Event.display.height;
	  al_draw_bitmap_region (
	    ScreenClone,
	    x, y, w, h,
	    x, y, 0
	  );
	  al_update_display_region (x, y, w, h)
	END
    UNTIL EndLoop;
    al_destroy_bitmap (ScreenClone);
    al_destroy_event_queue (Queue)
  END;



VAR
  Display: ALLEGRO_DISPLAYptr;
  Bitmap, FontBitmap: ALLEGRO_BITMAPptr;
  f1, f2, f3: ALLEGRO_FONTptr;
  Range, Index, X, Y, Start, Stop, Width, R, G, B: INTEGER;
  Ranges: ARRAY [0..7] OF LONGINT = (
    $0020, $007F, { ASCII }
    $00A1, $00FF, { Latin 1 }
    $0100, $017F, { Extended-A }
    $20AC, $20AC  { Euro }
  );
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');
  al_init_image_addon;
  al_init_font_addon;
  InitPlatformSpecific;

  al_set_new_display_option (ALLEGRO_SINGLE_BUFFER, 1, ALLEGRO_SUGGEST);
  al_set_new_display_flags (ALLEGRO_GENERATE_EXPOSE_EVENTS);
  Display := al_create_display (640, 480);
  IF Display = NIL THEN AbortExample ('Failed to create display.');

  Bitmap := al_load_bitmap ('data/mysha.pcx');
  IF Bitmap = NIL THEN AbortExample ('Failed to load misha.pcx.');

  f1 := al_load_font ('data/bmpfont.tga', 0, 0);
  IF f1 = NIL THEN AbortExample ('Failed to load bmpfont.tga.');

  FontBitmap := al_load_bitmap ('data/a4_font.tga');
  IF FontBitmap = NIL THEN AbortExample ('Failed to load a4_font.tga.');
  f2 := al_grab_font_from_bitmap (FontBitmap, 4, Ranges);

  f3 := al_create_builtin_font;
  IF f3 = NIL THEN AbortExample ('Failed to create builtin font.');


{ Draw background }
  al_draw_scaled_bitmap (bitmap, 0, 0, 320, 240, 0, 0, 640, 480, 0);

{ Draw red text }
  al_draw_text (f1, al_map_rgb (255, 0, 0), 10, 10, 0, 'red');

{ Draw green text }
  al_draw_text (f1, al_map_rgb (0, 255, 0), 120, 10, 0, 'green');

{ Draw a unicode symbol }
  al_draw_text (f2, al_map_rgb (0, 0, 255), 60, 60, 0, 'Mysha''s 0.02€');

{ Draw a yellow text with the builtin font. }
  al_draw_text (f3, al_map_rgb (255, 255, 0), 20, 200, ALLEGRO_ALIGN_CENTER,
        'a string from builtin font data');

{ Draw all individual glyphs the f2 font's range in rainbow colors. }
  x := 10; y := 300;
  al_draw_text (f3, al_map_rgb(0, 255, 255), x,  y - 20, 0, 'Draw glyphs: ');
  FOR Range := 0 TO 3 DO
  BEGIN
    Start := Ranges[2 * Range];
    Stop  := Ranges[2 * Range + 1];
    FOR Index := Start TO (Stop - 1) DO
    BEGIN
    { Use al_get_glyph_advance for the stride. }
      Width := al_get_glyph_advance (f2, Index, ALLEGRO_NO_KERNING);
    { I've translated "fabs" as "TRUNC".  May be it's not the best option. }
      r := ABS (TRUNC (sin (ALLEGRO_PI * (Index     ) * 36 / 360) * 255));
      g := ABS (TRUNC (sin (ALLEGRO_PI * (index + 12) * 36 / 360) * 255));
      b := ABS (TRUNC (sin (ALLEGRO_PI * (index + 24) * 36 / 360) * 255));
      al_draw_glyph (f2, al_map_rgb (r, g, b), x, y, Index);
      INC (x, Width);
      IF x > (al_get_display_width (Display) - 10) THEN
      BEGIN
        x := 10;
        INC (y, al_get_font_line_height (f2))
      END
    END
  END;

  al_flip_display;

  WaitForEsc (Display);

  al_destroy_bitmap (Bitmap);
  al_destroy_bitmap (FontBitmap);
  al_destroy_font (f1);
  al_destroy_font (f2);
  al_destroy_font (f3)
END.
