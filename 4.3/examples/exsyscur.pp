PROGRAM exsyscur;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/
 *
 *    This program demonstrates the use of hardware accelerated mouse cursors.
 *
 *    By Guillermo "Ñuño" Martínez <niunio(at)users.sourceforge.net>
 *    From an example program for the Allegro library, by Peter Wang and
 *    Evert Glebbeek.
 *
 *)

{$IFDEF FPC}
{ Free Pascal. }
  {$LONGSTRINGS ON}
{$ENDIF}

USES
  sysutils,
{ It needs some Allegro.pas units. }
  allegro,
  aldraw,   { Drawing primitives. }
  algraph,  { Graphic mode configuration. }
  almouse,  { Mouse input. }
  altext;   { Text drawing. }



VAR
  Black, White: LONGINT;



PROCEDURE DoSelectCursor (cursor: LONGINT);
VAR
  Txt1, Txt2: STRING;
BEGIN
  IF al_gfx_capabilities AND AL_GFX_HW_CURSOR <> 0 THEN
    Txt1 := 'HW_CURSOR   '
  ELSE
    Txt1 := 'no HW_CURSOR';
  IF al_gfx_capabilities AND AL_GFX_SYSTEM_CURSOR <> 0 THEN
    Txt2 := 'SYSTEM_CURSOR   '
  ELSE
    Txt2 := 'no SYSTEM_CURSOR';
  al_textout_centre_ex (al_screen, al_font, 'Before: '+Txt1+', '+Txt2,
		AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 3 + 2 * al_text_height (al_font),
		black, white);

  al_select_mouse_cursor(cursor);
  al_show_mouse (al_screen);

  IF al_gfx_capabilities AND AL_GFX_HW_CURSOR <> 0 THEN
    Txt1 := 'HW_CURSOR   '
  ELSE
    Txt1 := 'no HW_CURSOR';
  IF al_gfx_capabilities AND AL_GFX_SYSTEM_CURSOR <> 0 THEN
    Txt2 := 'SYSTEM_CURSOR   '
  ELSE
    Txt2 := 'no SYSTEM_CURSOR';
  al_textout_centre_ex (al_screen, al_font, 'After: '+Txt1+', '+Txt2,
		AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 3 + 3 * al_text_height (al_font),
		black, white);
END;



FUNCTION HandleKey: BOOLEAN;
VAR
  k: INTEGER;
BEGIN
   K := al_readkey AND $FF;
   IF (k = 27) OR (k = ORD ('q')) OR (k = ORD ('Q')) THEN
   BEGIN
     HandleKey := TRUE;
     EXIT
   END;
   IF K = ORD ('1') THEN
     DoSelectCursor (AL_MOUSE_CURSOR_ALLEGRO);
   IF K = ORD ('2') THEN
     DoSelectCursor (AL_MOUSE_CURSOR_ARROW);
   IF K = ORD ('3') THEN
     DoSelectCursor (AL_MOUSE_CURSOR_BUSY);
   IF K = ORD ('4') THEN
     DoSelectCursor (AL_MOUSE_CURSOR_QUESTION);
   IF K = ORD ('5') THEN
     DoSelectCursor (AL_MOUSE_CURSOR_EDIT);
   HandleKey := FALSE;
END;



VAR
  Height: INTEGER;
(* Program starts here. *)
BEGIN
{ Initialize Allegro }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Error initializing Allegro: '+al_error);
    EXIT
  END;

{ Initialize mouse and keyboard }
  al_install_timer;
  al_install_mouse;
  al_install_keyboard;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Error setting video mode: '+al_error);
    EXIT
  END;
  Black := al_makecol (0, 0, 0);
  White := al_makecol (255, 255, 255);
  al_clear_to_color (al_screen, White);

  al_enable_hardware_cursor;

  Height := AL_SCREEN_H DIV 3 + 5 * al_text_height (al_font);
  al_textout_centre_ex (al_screen, al_font, '1) MOUSE_CURSOR_ALLEGRO  ',
			AL_SCREEN_W DIV 2, Height, black, white);
  INC (Height, al_text_height (al_font));
  al_textout_centre_ex (al_screen, al_font, '2) MOUSE_CURSOR_ARROW    ',
			AL_SCREEN_W DIV 2, Height, black, white);
  INC (Height, al_text_height (al_font));
  al_textout_centre_ex (al_screen, al_font, '3) MOUSE_CURSOR_BUSY     ',
			AL_SCREEN_W DIV 2, Height, black, white);
  INC (Height, al_text_height (al_font));
  al_textout_centre_ex (al_screen, al_font, '4) MOUSE_CURSOR_QUESTION ',
			AL_SCREEN_W DIV 2, Height, black, white);
  INC (Height, al_text_height (al_font));
  al_textout_centre_ex (al_screen, al_font, '5) MOUSE_CURSOR_EDIT     ',
			AL_SCREEN_W DIV 2, Height, black, white);
  INC (Height, al_text_height (al_font));
  al_textout_centre_ex (al_screen, al_font, 'Escape) Quit             ',
			AL_SCREEN_W DIV 2, Height, black, white);

{ first cursor shown }
  DoSelectCursor (AL_MOUSE_CURSOR_ALLEGRO);

  REPEAT ; UNTIL  HandleKey;

  al_exit;
END.
