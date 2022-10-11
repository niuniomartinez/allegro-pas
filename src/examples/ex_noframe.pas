program ex_noframe;
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
  allegro5, al5image;

var
  Display: ALLEGRO_DISPLAYptr;
  Bitmap: ALLEGRO_BITMAPptr;
  Events: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Down: Boolean;
  DownX, DownY, cx, cy: LongInt;
  Timer: ALLEGRO_TIMERptr;

begin
  Down := False;
  DownX := 0; DownY := 0;
  cx := 0; cy := 0;

  if not al_init then AbortExample ('Could not init Allegro.');

  al_install_mouse;
  al_install_keyboard;
  al_init_image_addon;
  InitPlatformSpecific;

  al_set_new_display_flags (ALLEGRO_FRAMELESS);
  Display := al_create_display (300, 200);
  if Display = Nil then AbortExample ('Error creating display.');

  Bitmap := al_load_bitmap ('data/fakeamp.bmp');
  if Bitmap = Nil then AbortExample ('Error loading fakeamp.bmp.');

  Timer := al_create_timer (1 / 30);

  Events := al_create_event_queue;
  al_register_event_source (Events, al_get_mouse_event_source);
  al_register_event_source (Events, al_get_keyboard_event_source);
  al_register_event_source (Events, al_get_display_event_source (Display));
  al_register_event_source (Events, al_get_timer_event_source (Timer));

  al_start_timer (Timer);

  while True do
  begin
    al_wait_for_event (Events, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
      begin
	if (Event.mouse.button = 1) and (Event.mouse.x <> 0) then
	begin
	  Down := True;
	  DownX := Event.mouse.x;
	  DownY := Event.mouse.y
	end;
	if Event.mouse.button = 2 then
	  al_set_display_flag (
	    Display, ALLEGRO_FRAMELESS,
	    not ((al_get_display_flags (Display) and ALLEGRO_FRAMELESS) <> 0)
	  )
      end;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Break;
    ALLEGRO_EVENT_MOUSE_BUTTON_UP:
      if Event.mouse.button = 1 then Down := False;
    ALLEGRO_EVENT_MOUSE_AXES:
      if Down then
	if al_get_mouse_cursor_position (cx, cy) then
	  al_set_window_position (Display, cx - DownX, cy - DownY);
    ALLEGRO_EVENT_KEY_DOWN:
      if event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then Break;
    ALLEGRO_EVENT_TIMER:
      begin
	al_draw_bitmap (Bitmap, 0, 0, 0);
	al_flip_display
      end;
    end
  end;

  al_destroy_timer (Timer);
  al_destroy_event_queue (Events);
  al_destroy_display (Display)
end.
