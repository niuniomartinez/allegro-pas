program ex_disable_screensaver;
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
    allegro5, al5font;

var
  Display: ALLEGRO_DISPLAYptr;
  Font: ALLEGRO_FONTptr;
  Events: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Done, Active, FullScreen: Boolean;

begin
  Done := False;
  Active := True;
  FullScreen := False;

  if ParamCount = 2 then
    if ParamStr (1) = '-fullscreen' then
      FullScreen := True;

  if not al_init then AbortExample ('Could not init Allegro.');

  al_install_keyboard;
  al_init_font_addon;

  if FullScreen then
    al_set_new_display_flags
      (ALLEGRO_GENERATE_EXPOSE_EVENTS or ALLEGRO_FULLSCREEN)
  else
    al_set_new_display_flags
      (ALLEGRO_GENERATE_EXPOSE_EVENTS);

  Display := al_create_display (640, 480);
  if Display = Nil then AbortExample ('Could not create display.');

  Font := al_create_builtin_font;
  if Font = Nil then AbortExample ('Error creating builtin font.');

  Events := al_create_event_queue;
  al_register_event_source (Events, al_get_keyboard_event_source);
{ For expose events }
  al_register_event_source (Events, al_get_display_event_source (Display));

  repeat
    al_clear_to_color (al_map_rgb (0, 0, 0));
    if Active then
      al_draw_text (Font, al_map_rgb_f (1, 1, 1), 0, 0, 0, 'Screen saver: Normal')
    else
      al_draw_text (Font, al_map_rgb_f (1, 1, 1), 0, 0, 0, 'Screen saver: Inhibited');
    al_flip_display;
    al_wait_for_event (Events, @Event);
    if Event.ftype = ALLEGRO_EVENT_KEY_DOWN then
      case Event.keyboard.keycode of
      ALLEGRO_KEY_ESCAPE:
	Done := True;
      ALLEGRO_KEY_SPACE:
	if al_inhibit_screensaver (Active) then Active := not Active;
      end
  until Done;

  al_destroy_font (Font);
  al_destroy_event_queue (Events)
end.
