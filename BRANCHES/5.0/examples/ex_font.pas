PROGRAM ex_font;
(*<An example. *)
(* TODO: License. *)

  USES
    Allegro5, al5font, al5image;

  { #include "common.c" }

  CONST
    EURO = $e282ac;



  PROCEDURE WaitForEsc (Display: ALLEGRO_DISPLAYptr);
  BEGIN
{
   ALLEGRO_EVENT_QUEUE *queue;
   ALLEGRO_BITMAP *screen_clone;
   al_install_keyboard();
   queue = al_create_event_queue();
   al_register_event_source(queue, al_get_keyboard_event_source());
   al_register_event_source(queue, al_get_display_event_source(display));
   screen_clone = al_clone_bitmap(al_get_target_bitmap());
   while (1) {
      ALLEGRO_EVENT event;
      al_wait_for_event(queue, &event);
      if (event.type == ALLEGRO_EVENT_DISPLAY_CLOSE)
         break;
      else if (event.type == ALLEGRO_EVENT_KEY_DOWN) {
         if (event.keyboard.keycode == ALLEGRO_KEY_ESCAPE)
            break;
      }
      else if (event.type == ALLEGRO_EVENT_DISPLAY_EXPOSE) {
         int x = event.display.x;
         int y = event.display.y;
         int w = event.display.width;
         int h = event.display.height;

         al_draw_bitmap_region(screen_clone, x, y, w, h,
            x, y, 0);
         al_update_display_region(x, y, w, h);
      }
   }
   al_destroy_bitmap(screen_clone);
   al_destroy_event_queue(queue);
}
  END;



VAR
  Display: ALLEGRO_DISPLAYptr;
  Bitmap, FontBitmap: ALLEGRO_BITMAPptr;
  Font, A4Font: ALLEGRO_FONTptr;
  Ranges: ARRAY [0..7] OF LONGINT = (
    $0020, $007F, { ASCII }
    $00A1, $00FF, { Latin 1 }
    $0100, $017F, { Extended-A }
    $20AC, $20AC  { Euro }
  );
BEGIN
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Could not init Allegro.');
    EXIT;
  END;
  al_init_image_addon;
  al_init_font_addon;

  al_set_new_display_option (ALLEGRO_SINGLE_BUFFER, 0, ALLEGRO_SUGGEST);
  al_set_new_display_flags (ALLEGRO_GENERATE_EXPOSE_EVENTS);
  Display := al_create_display (320, 200);
  IF Display = NIL THEN
  BEGIN
    WriteLn ('Failed to create display.');
    EXIT;
  END;
{
    bitmap = al_load_bitmap("data/mysha.pcx");
    if (!bitmap) {
        abort_example("Failed to load mysha.pcx\n");
        return 1;
    }

    f = al_load_font("data/bmpfont.tga", 0, 0);
    if (!f) {
        abort_example("Failed to load bmpfont.tga\n");
        return 1;
    }
    
    font_bitmap = al_load_bitmap("data/a4_font.tga");
    if (!font_bitmap) {
        abort_example("Failed to load a4_font.tga\n");
        return 1;
    }
    a4f = al_grab_font_from_bitmap(font_bitmap, 4, ranges);

    /* Draw background */
    al_draw_bitmap(bitmap, 0, 0, 0);

    /* Draw red text */
    al_draw_textf(f, al_map_rgb(255, 0, 0), 10, 10, 0, "red");

    /* Draw green text */
    al_draw_textf(f, al_map_rgb(0, 255, 0), 10, 50, 0, "green");
    
    /* Draw a unicode symbol */
    al_draw_textf(a4f, al_map_rgb(0, 0, 255), 10, 90, 0, "Mysha's 0.02" EURO);

    al_flip_display();

    wait_for_esc(display);

    al_destroy_bitmap(bitmap);
    al_destroy_bitmap(font_bitmap);
    al_destroy_font(f);
    al_destroy_font(a4f);
    return 0;
}

END.
