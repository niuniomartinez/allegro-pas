program ex_windows;
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
  Allegro5, al5font, al5image;

const
  W = 100;
  H = 100;
  Margin = 20;

var
  Displays: array [0..1] of ALLEGRO_DISPLAYptr;
  Info: array of  ALLEGRO_MONITOR_INFO;
  x, y, a, dw, dh: Integer;
  MyFont: ALLEGRO_FONTptr;
  Events: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  i: Integer;
  EndExample: Boolean;

begin
  Randomize;

  if not al_init then AbortExample ('Could not init Allegro.');

  al_install_mouse;
  al_init_font_addon;
  al_init_image_addon;

  SetLength (Info, al_get_num_video_adapters);

  for i := Low (Info) to High (Info) do al_get_monitor_info (i, Info[i]);

  x := Trunc (((info[0].x2 - info[0].x1) / 3) - (W / 2));
  y := Trunc (((info[0].y2 - info[0].y1) / 2) - (H / 2));

  al_set_new_window_position (x, y);

  Displays[0] := al_create_display (W, H);

  x := x * 2;
  al_set_new_window_position (x, y);

  Displays[1] := al_create_display (W, H);

  if (Displays[0] = Nil) or (Displays[1] = Nil) then
    AbortExample ('Could not create displays.');

  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
  MyFont := al_load_font ('data/fixed_font.tga', 0, 0);
  if MyFont = Nil then  AbortExample ('Could not load font.');

  events := al_create_event_queue;
  al_register_event_source (Events, al_get_mouse_event_source);
  al_register_event_source (Events, al_get_display_event_source (Displays[0]));
  al_register_event_source (Events, al_get_display_event_source (Displays[1]));

  EndExample := False;
  repeat
    for i := Low (Displays) to High (Displays) do
    begin
      al_set_target_backbuffer (Displays[i]);
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      if i = 0 then
         al_clear_to_color (al_map_rgb (255, 0, 255))
      else
         al_clear_to_color (al_map_rgb (155, 255, 0));
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_draw_text (myfont, al_map_rgb (0, 0, 0), 50, 50, ALLEGRO_ALIGN_CENTRE, 'Click me...');
      al_flip_display
    end;

    if al_wait_for_event_timed (events, @event, 1) then
    begin
      if Event.ftype = ALLEGRO_EVENT_DISPLAY_CLOSE then
	EndExample := True
      else if event.ftype = ALLEGRO_EVENT_MOUSE_BUTTON_DOWN then
      begin
	a := Random (Length (Info));
	dw := Info[a].x2 - Info[a].x1;
	dh := Info[a].y2 - Info[a].y1;
	x := Margin + Info[a].x1 + (Random (dw - W - Margin));
	y := Margin + Info[a].y1 + (Random (dh - H - Margin));
	al_set_window_position (Event.mouse.display, x, y);
      end
    end
  until EndExample;

  al_destroy_event_queue (Events);

  al_destroy_display (Displays[0]);
  al_destroy_display (Displays[1]);
end.
