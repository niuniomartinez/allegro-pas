program ex_expose;
(*
  Copyright (c) 2012-2019 Guillermo Martínez J.

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
  allegro5, al5image, al5primitives;

var
  Display: ALLEGRO_DISPLAYptr;
  Bitmap: ALLEGRO_BITMAPptr;
  Timer: ALLEGRO_TIMERptr;
  Queue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  x, y: Integer;

begin
  if not al_init then AbortExample ('Could not init Allegro.');

  al_init_primitives_addon;
  al_init_image_addon;
  al_install_keyboard;
  al_install_mouse;
  InitPlatformSpecific;

  al_set_new_display_flags
    (ALLEGRO_RESIZABLE or ALLEGRO_GENERATE_EXPOSE_EVENTS);
  al_set_new_display_option (ALLEGRO_SINGLE_BUFFER, -1, ALLEGRO_REQUIRE);
  Display := al_create_display (320, 200);
  if Display = Nil then AbortExample ('Error creating display.');

  Bitmap := al_load_bitmap ('data/mysha.pcx');
  if Bitmap = Nil then AbortExample ('mysha.pcx not found or failed to load.');
  al_draw_bitmap (Bitmap, 0, 0, 0);
  al_flip_display;

  Timer := al_create_timer (0.1);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_mouse_event_source);
  al_register_event_source (Queue, al_get_display_event_source (Display));
  al_register_event_source (Queue, al_get_timer_event_source (Timer));
  al_start_timer (Timer);

  while True do
  begin
    al_wait_for_event (Queue, @Event);
    case event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      begin
	al_destroy_event_queue (Queue);
	al_destroy_bitmap (Bitmap);
	Exit
      end;
    ALLEGRO_EVENT_KEY_DOWN:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
      begin
	al_destroy_event_queue (Queue);
	al_destroy_bitmap (Bitmap);
	Exit
      end;
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      al_acknowledge_resize (Event.display.source);
    ALLEGRO_EVENT_DISPLAY_EXPOSE:
    { Draw a red rectangle over the damaged area. }
      begin
	al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
	al_draw_filled_rectangle (
	  Event.display.x,
	  Event.display.y,
	  Event.display.x + Event.display.width,
	  Event.display.y + Event.display.height,
	  al_map_rgba_f (1, 0, 0, 1)
	);
	al_flip_display
      end;
    ALLEGRO_EVENT_TIMER:
    { Slowly restore the original bitmap. }
      begin
	al_set_blender (ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA);
	y := 0;
	while y < al_get_display_height (Display) do
	begin
	  x := 0;
	  while x < al_get_display_width (Display) do
	  begin
	    al_draw_tinted_bitmap (
	      Bitmap,
	      al_map_rgba_f (1, 1, 1, 0.1),
	      x, y, 0
	    );
	    x := x + 320
	  end;
	  y := y + 200
	end;
	al_flip_display
      end;
    end
  end
end.
