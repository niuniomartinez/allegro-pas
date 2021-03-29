PROGRAM ex_clipboard;
(* An example showing bitmap flipping flags, by Steven Wallace.

  NOTE: Previous comment seems misplaced... *)
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
    allegro5, al5base, al5image, al5font;

  const
    INTERVAL = 0.1;

  var
    Display: ALLEGRO_DISPLAYptr;
    Timer: ALLEGRO_TIMERptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Font: ALLEGRO_FONTptr;
    Text: AL_STRptr;
    Done, Redraw: Boolean;

begin
  Text := Nil;
  Done := False;
  Redraw := True;

  if not al_init then AbortExample ('Could not init Allegro.');

  if not al_init_image_addon then AbortExample ('Failed to init IIO addon.');

  al_init_font_addon;
  InitPlatformSpecific;

  Display := al_create_display (640, 480);
  if Display = Nil then AbortExample ('Error creating display.');


  if not al_install_keyboard then AbortExample ('Error installing keyboard.');

  Font := al_load_font ('data/fixed_font.tga', 0, 0);
  if Font = Nil then AbortExample ('Error loading data/fixed_font.tga');

  Timer := al_create_timer (INTERVAL);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_timer_event_source (Timer));
  al_register_event_source (Queue, al_get_display_event_source (Display));

  al_start_timer (Timer);

  al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);

  repeat
    if Redraw and al_is_event_queue_empty (Queue) then
    begin
      if Text <> Nil then al_free (Text);
      if al_clipboard_has_text (Display) then
        Text := al_get_clipboard_text (Display)
      else
        Text := Nil;

      al_clear_to_color (al_map_rgb_f (0, 0, 0));

      if Text <> Nil then
        al_draw_text (Font, al_map_rgba_f (1, 1, 1, 1.0), 0, 0, 0, Text)
      else
        al_draw_text (Font, al_map_rgba_f(1, 0, 0, 1.0), 0, 0, 0,
                      'No clipboard text available.');
      al_flip_display;
      Redraw := False;
    end;

    al_wait_for_event (Queue, @Event);
    case Event.ftype OF
    ALLEGRO_EVENT_KEY_DOWN:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
        Done := true
      else if Event.keyboard.keycode = ALLEGRO_KEY_SPACE then
        al_set_clipboard_text (Display, 'Copied from Allegro!');
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Done := true;
    ALLEGRO_EVENT_TIMER:
      Redraw := true
    end
  until Done;

  al_destroy_font (Font);
end.
