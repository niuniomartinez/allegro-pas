PROGRAM ex_mouse;
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
    Allegro5, al5image, al5primitives;

  const
    NUM_BUTTONS = 3;

  procedure DrawMouseButton (But: Integer; Down: Boolean);
  const
    Offset: array [1..NUM_BUTTONS] of Integer = (0, 70, 35);
  var
    Grey, Black: ALLEGRO_COLOR;
    x, y: Integer;
  begin
    x := 400 + Offset[But];
    y := 130;

    Grey := al_map_rgb ($E0, $E0, $E0);
    Black := al_map_rgb (0, 0, 0);

    al_draw_filled_rectangle (x, y, x + 27, y + 42, Grey);
    al_draw_rectangle (x + 0.5, y + 0.5, x + 26.5, y + 41.5, Black, 0);
    if Down then
      al_draw_filled_rectangle (x + 2, y + 2, x + 25, y + 40, Black)
  end;



var
  Display: ALLEGRO_DISPLAYptr;
  Cursor: ALLEGRO_BITMAPptr;
  msEstate: ALLEGRO_MOUSE_STATE;
  KbdState: ALLEGRO_KEYBOARD_STATE;
  i: Integer;
begin
  if not al_init then AbortExample ('Could not init Allegro.');

  al_init_primitives_addon;
  al_install_mouse;
  al_install_keyboard;
  al_init_image_addon;
  InitPlatformSpecific;

  Display := al_create_display (640, 480);
  if Display = Nil then AbortExample ('Error creating display');

  al_hide_mouse_cursor (display);

  Cursor := al_load_bitmap ('data/cursor.tga');
  if Cursor = Nil then AbortExample ('Error loading cursor.tga');

  repeat
    al_get_mouse_state (msEstate);
    al_get_keyboard_state (KbdState);

    al_clear_to_color (al_map_rgb ($FF, $FF, $C0));
    for i := 1 to NUM_BUTTONS do
      DrawMouseButton (i, al_mouse_button_down (msEstate, i));
    al_draw_bitmap (Cursor, msEstate.x, msestate.y, 0);
    al_flip_display;

    al_rest (0.005);
  until al_key_down (KbdState, ALLEGRO_KEY_ESCAPE)
end.

