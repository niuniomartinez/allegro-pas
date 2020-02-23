PROGRAM ex_mouse_focus;
(*
 *    Example program for the Allegro library.
 *
 *    This program tests if the ALLEGRO_MOUSE_STATE `display' field
 *    is set correctly.
 *)
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

  USES
    Common,
    allegro5;

  VAR
    Display1, Display2: ALLEGRO_DISPLAYptr;

  PROCEDURE Redraw (Color1, Color2: ALLEGRO_COLOR);
  BEGIN
    al_set_target_backbuffer (Display1);
    al_clear_to_color (Color1);
    al_flip_display;

    al_set_target_backbuffer (Display2);
    al_clear_to_color (Color2);
    al_flip_display;
  END;

  VAR
    Black, Red: ALLEGRO_COLOR;
    OldMouseState, MouseState: ALLEGRO_MOUSE_STATE;
    kst: ALLEGRO_KEYBOARD_STATE;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');
  IF NOT al_install_mouse THEN AbortExample ('Could not install mouse.');
  IF NOT al_install_keyboard THEN AbortExample ('Could not install keyboard.');

  Display1 := al_create_display (300, 300);
  Display2 := al_create_display (300, 300);
  IF (Display1 = NIL) OR (Display2 = NIL) THEN
  BEGIN
    al_destroy_display (Display1);
    al_destroy_display (Display2);
    AbortExample ('Couldn''t open displays.')
  END;

  OpenLog;
  LogWriteLn ('Move the mouse cursor over the displays.');

  Black := al_map_rgb (0, 0, 0);
  Red := al_map_rgb (255, 0, 0);

  OldMouseState.display := NIL;
  OldMouseState.x := 0;
  OldMouseState.y := 0;

  REPEAT
    al_get_mouse_state (MouseState);
    IF (MouseState.display <> OldMouseState.display)
    OR (MouseState.x <> OldMouseState.x) OR (MouseState.y <> OldMouseState.y)
    THEN BEGIN
      IF MouseState.display = NIL THEN
	LogWriteLn ('Outside either display.')
      ELSE IF MouseState.display = Display1 THEN
	LogPrintLn ('In display 1, x = %d, y = %d.', [MouseState.x, MouseState.y])
      ELSE IF MouseState.display = Display2 THEN
	LogPrintLn ('In display 2, x = %d, y = %d.', [MouseState.x, MouseState.y])
      ELSE
	LogPrintLn (
	  'Unknown display = %p, x = %d, y = %d.',
	  [MouseState.display, MouseState.x, MouseState.y]
	);
      OldMouseState := MouseState
    END;

    IF MouseState.display = Display1 THEN
      Redraw (Red, Black)
    ELSE IF MouseState.display = Display2 THEN
       Redraw (Black, Red)
    ELSE
       Redraw (Black, Black);

    al_rest (0.1);

    al_get_keyboard_state (kst);
  UNTIL al_key_down (kst, ALLEGRO_KEY_ESCAPE);
  CloseLog (FALSE)
END.
