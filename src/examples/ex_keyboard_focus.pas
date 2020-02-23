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

USES
   Common,
   allegro5;

VAR
  Display1, Display2: ALLEGRO_DISPLAYptr;

  PROCEDURE Redraw (Color1, Color2: ALLEGRO_COLOR);
  BEGIN
    al_set_target_backbuffer (Display1);
    al_clear_to_color (Color1);
    al_flip_display();

    al_set_target_backbuffer (Display2);
    al_clear_to_color (Color2);
    al_flip_display()
  END;

VAR
  Black, Red: ALLEGRO_COLOR;
  KbdState: ALLEGRO_KEYBOARD_STATE;
BEGIN
  IF NOT al_init THEN AbortExample ('Error initialising Allegro.');

  IF NOT al_install_keyboard THEN AbortExample ('Error installing keyboard.');

  Display1 := al_create_display (300, 300);
  Display2 := al_create_display (300, 300);
  IF (Display1 = NIL) OR (Display2 = NIL) THEN
    AbortExample ('Error creating displays.');

  Black := al_map_rgb (0, 0, 0);
  Red := al_map_rgb (255, 0, 0);

  WHILE TRUE DO
  BEGIN
    al_get_keyboard_state (KbdState);
    IF al_key_down (KbdState, ALLEGRO_KEY_ESCAPE) THEN EXIT;

    IF KbdState.display = Display1 THEN
      Redraw (Red, Black)
    ELSE IF KbdState.display = Display2 THEN
      Redraw (Black, Red)
    ELSE
      Redraw (Black, Black);

    al_rest(0.1)
  END
END.
