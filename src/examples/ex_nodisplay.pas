PROGRAM ex_nodisplay;
(* Test that bitmap manipulation without a display works. *)
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
  allegro5, al5image;

VAR
  Bmp, Sprite: ALLEGRO_BITMAPptr;
  c1, c2, c3: ALLEGRO_COLOR;
  rc: BOOLEAN;

BEGIN
  IF NOT al_init THEN AbortExample ('Error initialising Allegro.');
  al_init_image_addon;
  InitPlatformSpecific;

  Sprite := al_load_bitmap ('data/cursor.tga');
  IF Sprite = NIL THEN AbortExample ('Error loading data/cursor.tga.');

  Bmp := al_create_bitmap (256, 256);
  IF Bmp = NIL THEN AbortExample ('Error creating bitmap.');

  al_set_target_bitmap (Bmp);

  c1 := al_map_rgb (255, 0, 0);
  c2 := al_map_rgb (0, 255, 0);
  c3 := al_map_rgb (0, 255, 255);

  al_clear_to_color (al_map_rgba(255, 0, 0, 128));
  al_draw_bitmap (Sprite, 0, 0, 0);
  al_draw_tinted_bitmap (Sprite, c1, 64, 0, ALLEGRO_FLIP_HORIZONTAL);
  al_draw_tinted_bitmap (Sprite, c2, 0, 64, ALLEGRO_FLIP_VERTICAL);
  al_draw_tinted_bitmap (Sprite, c3, 64, 64, ALLEGRO_FLIP_HORIZONTAL OR ALLEGRO_FLIP_VERTICAL);

  al_set_target_bitmap (NIL);

  rc := al_save_bitmap ('ex_nodisplay_out.tga', Bmp);

{$IFDEF LINUX}
  IF rc THEN
    WriteLn ('Saved ex_nodisplay_out.tga.')
  ELSE
{$ELSE}
  IF NOT RC THEN
{$ENDIF}
    AbortExample ('Error saving ex_nodisplay_out.tga.');

  al_destroy_bitmap (Sprite);
  al_destroy_bitmap (Bmp)
END.
