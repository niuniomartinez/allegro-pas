program ex_mouse_warp;
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
    common,
    Allegro5, al5font, al5image, al5primitives,
    sysutils;

  const
    Width = 640;
    Height = 480;

  var
    Font: ALLEGRO_FONTptr;
    Display: ALLEGRO_DISPLAYptr;
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    EndLoop, RightButtonDown, Redraw: Boolean;
    FakeX, FakeY, th: Integer;
    White, Black, Red: ALLEGRO_COLOR;

begin
  RightButtonDown := False;
  Redraw := True;
  FakeX := 0; FakeY := 0;

  if not al_init then
    AbortExample ('Could not init Allegro.');
  al_init_primitives_addon;
  al_init_font_addon;
  al_init_image_addon;
  al_install_mouse;
  al_install_keyboard;

  al_set_new_display_flags (ALLEGRO_WINDOWED);
  Display := al_create_display (Width, Height);
  if Display = Nil then
    AbortExample ('Could not create display.');

  EventQueue := al_create_event_queue;
  al_register_event_source (EventQueue, al_get_display_event_source (Display));
  al_register_event_source (EventQueue, al_get_mouse_event_source);
  al_register_event_source (EventQueue, al_get_keyboard_event_source);

  Font := al_load_font ('data/fixed_font.tga', 0, 0);
  White := al_map_rgb_f (1, 1, 1);
  Black := al_map_rgb_f (0, 0, 0);
  Red   := al_map_rgb_f (1, 0, 0);

  EndLoop := False;
  repeat
    al_get_next_event (EventQueue, Event);
    if Redraw and al_is_event_queue_empty (EventQueue) then
    begin
      th := al_get_font_line_height (Font);

      al_clear_to_color (Black);

      if RightButtonDown then
      begin
	al_draw_line (Width div 2, Height div 2, FakeX, FakeY, Red, 1);
	al_draw_line (FakeX - 5, FakeY, FakeX + 5, FakeY, White, 2);
	al_draw_line (FakeX, FakeY - 5, FakeX, FakeY + 5, White, 2);
      end;

      al_draw_text (Font, White, 0, 0, 0,
	Format ('x: %d y: %d dx: %d dy %d',
	  [event.mouse.x, event.mouse.y, event.mouse.dx, event.mouse.dy]
	)
      );
      al_draw_text (Font, White, Width div 2, Height div 2 - th,
	ALLEGRO_ALIGN_CENTRE, 'Left-Click to warp pointer to the middle once.'
      );
      al_draw_text (Font, White, Width div 2, Height div 2,
	ALLEGRO_ALIGN_CENTRE,
	'Hold right mouse button to constantly move pointer to the middle.'
      );
      al_flip_display;
      Redraw := False;
    end;

    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EndLoop := True;
    ALLEGRO_EVENT_KEY_DOWN:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
	EndLoop := True;
    ALLEGRO_EVENT_MOUSE_WARPED: { ??? }
    { WriteLn ('Warp') };
    ALLEGRO_EVENT_MOUSE_AXES:
      begin
	if RightButtonDown then
	begin
	  al_set_mouse_xy (Display, Width div 2, Height div 2);
	  Inc (FakeX, Event.mouse.dx);
	  Inc (FakeY, Event.mouse.dy);
	end;
	Redraw := True;
      end;
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
      begin
	if Event.mouse.button = 1 then
	  al_set_mouse_xy (Display, Width div 2, Height div 2);
	if Event.mouse.button = 2 then
	begin
	  RightButtonDown := True;
	  FakeX := Width div 2;
	  FakeY := Height div 2;
	end;
	Redraw := True;
      end;
    ALLEGRO_EVENT_MOUSE_BUTTON_UP:
      if Event.mouse.button = 2 then
        RightButtonDown := False;
    end;
  until EndLoop;

  al_destroy_font (Font);
  al_destroy_event_queue(EventQueue);
  al_destroy_display (Display);
end.
