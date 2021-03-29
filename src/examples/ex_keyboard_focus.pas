PROGRAM ex_keyboard_focus;
(*
 *    Example program for the Allegro library.
 *
 *    This program tests if the ALLEGRO_KEYBOARD_STATE `display' field
 *    is set correctly to the focused display.
 *)
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
   allegro5;

var
  Display1, Display2: ALLEGRO_DISPLAYptr;

  procedure Redraw (Color1, Color2: ALLEGRO_COLOR);
  begin
    al_set_target_backbuffer (Display1);
    al_clear_to_color (Color1);
    al_flip_display();

    al_set_target_backbuffer (Display2);
    al_clear_to_color (Color2);
    al_flip_display()
  end;

var
  Black, Red: ALLEGRO_COLOR;
  KbdState: ALLEGRO_KEYBOARD_STATE;
begin
  if not al_init then AbortExample ('Error initialising Allegro.');

  if not al_install_keyboard then AbortExample ('Error installing keyboard.');

  Display1 := al_create_display (300, 300);
  Display2 := al_create_display (300, 300);
  if (Display1 = Nil) or (Display2 = Nil) then
    AbortExample ('Error creating displays.');

  Black := al_map_rgb (0, 0, 0);
  Red := al_map_rgb (255, 0, 0);

  while True do
  begin
    al_get_keyboard_state (KbdState);
    if al_key_down (KbdState, ALLEGRO_KEY_ESCAPE) then Exit;

    if KbdState.display = Display1 then
      Redraw (Red, Black)
    else if KbdState.display = Display2 then
      Redraw (Black, Red)
    else
      Redraw (Black, Black);

    al_rest(0.1)
  end
end.
