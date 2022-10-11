program ex_display_events;
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

  uses
    Common,
    Allegro5, al5base, al5font, al5primitives, al5strings;

  const
    MAX_EVENTS = 23;

  var
    Events: array [1..MAX_EVENTS] of AL_STR;



  procedure AddEvent (const Message: AL_STR);
  var
    Ndx: Integer;
  begin
    for Ndx := High (Events) - 1 downto Low (Events) do
      Events[Ndx + 1] := Events[Ndx];
    Events[Low (Events)] := Message
  end;



  var
    Display: ALLEGRO_DISPLAYptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Font: ALLEGRO_FONTptr;
    Color, Black, Red, Blue: ALLEGRO_COLOR;
    i: Integer;
    EndExample: Boolean;
    x, y: Single;
begin
  if not al_init then AbortExample ('Could not init Allegro.');

  al_init_primitives_addon;
  al_install_mouse;
  al_install_keyboard;
  al_init_font_addon;

  al_set_new_display_flags (ALLEGRO_RESIZABLE);
  Display := al_create_display (640, 480);
  if Display = Nil then AbortExample ('Error creating display');

  Font := al_create_builtin_font;
  if Font = Nil then AbortExample ('Error creating builtin font');

  Black := al_map_rgb_f (0, 0, 0);
  Red := al_map_rgb_f (1, 0, 0);
  Blue := al_map_rgb_f (0, 0, 1);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_mouse_event_source);
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_display_event_source (Display));

  for i := Low (Events) to High (Events) do Events[i] := '';
  EndExample := False;
  repeat
    if al_is_event_queue_empty (Queue) then
    begin
      x := 8; y := 28;
      al_clear_to_color (al_map_rgb ($FF, $FF, $C0));

      al_draw_text (Font, Blue, 8, 8, 0, 'Display events (newest on top)');

      Color := Red;
      for i := Low (Events) to High (Events) do
      begin
        if Events[i] <> '' then
        begin
          al_draw_text (Font, Color, x, y, 0, Events[i]);
          Color := Black;
          y := y + 20
        end
      end;
      al_flip_display
    end;

    al_wait_for_event (Queue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_MOUSE_ENTER_DISPLAY:
      AddEvent ('ALLEGRO_EVENT_MOUSE_ENTER_DISPLAY');
    ALLEGRO_EVENT_MOUSE_LEAVE_DISPLAY:
      AddEvent ('ALLEGRO_EVENT_MOUSE_LEAVE_DISPLAY');
    ALLEGRO_EVENT_KEY_DOWN:
      if Event.Keyboard.keycode = ALLEGRO_KEY_ESCAPE then EndExample := True;
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      begin
        AddEvent (al_str_format (
          'ALLEGRO_EVENT_DISPLAY_RESIZE x=%d, y=%d, width=%d, height=%d',
          [
            Event.display.x, Event.display.y, Event.display.width,
            Event.display.height
          ]
        ));
        al_acknowledge_resize (Event.display.source)
      end;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      AddEvent ('ALLEGRO_EVENT_DISPLAY_CLOSE');
    ALLEGRO_EVENT_DISPLAY_LOST:
      AddEvent ('ALLEGRO_EVENT_DISPLAY_LOST');
    ALLEGRO_EVENT_DISPLAY_FOUND:
      AddEvent ('ALLEGRO_EVENT_DISPLAY_FOUND');
    ALLEGRO_EVENT_DISPLAY_SWITCH_OUT:
      AddEvent ('ALLEGRO_EVENT_DISPLAY_SWITCH_OUT');
    ALLEGRO_EVENT_DISPLAY_SWITCH_IN:
      AddEvent ('ALLEGRO_EVENT_DISPLAY_SWITCH_IN');
    end
  until EndExample;

  al_destroy_event_queue (Queue)
end.
