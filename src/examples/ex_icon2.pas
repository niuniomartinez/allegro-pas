program ex_icon2;
(*
 *      Example program for the Allegro library.
 *
 *      Set multiple window icons, a big one and a small one.
 *      The small would be used for the task bar, and the big
 *      one for the alt-tab popup, for example.
 *)
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

  uses
    Common,
    Allegro5, al5image;

  const
    NUM_ICONS = 2;

  var
    Display: ALLEGRO_DISPLAYptr;
    Icons: array [0..NUM_ICONS-1] of ALLEGRO_BITMAPptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    u, v: Integer;
begin
  if not al_init then AbortExample ('Could not init Allegro.');

  al_install_keyboard;
  al_init_image_addon;
  InitPlatformSpecific;

  Display := al_create_display (320, 200);
  if Display = Nil then AbortExample ('Could not create display');
  al_clear_to_color (al_map_rgb_f (0, 0, 0));
  al_flip_display;

{ First icon 16x16: Read from file. }
  Icons[0] := al_load_bitmap ('data/cursor.tga');
  if Icons[0] = Nil then AbortExample ('cursor.tga not found');

{ Second icon 32x32: Create it. }
  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
  Icons[1] := al_create_bitmap (32, 32);
  al_set_target_bitmap (Icons[1]);
  for v := 0 to 31 do
    for u := 0 to 31 do
      al_put_pixel (u, v, al_map_rgb_f (u / 31.0, v / 31.0, 1));
  al_set_target_backbuffer (Display);

  al_set_display_icons (Display, NUM_ICONS, Icons);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_display_event_source (Display));

  while True do
  begin
    al_wait_for_event (Queue, @Event);

    if (Event.ftype = ALLEGRO_EVENT_KEY_DOWN)
    and (Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE) then
      Exit;
    if Event.ftype = ALLEGRO_EVENT_DISPLAY_CLOSE then
      Exit;
  end
end.
