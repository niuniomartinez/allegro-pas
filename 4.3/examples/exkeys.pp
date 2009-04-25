PROGRAM exkeys;
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
 *    This program demonstrates how to access the keyboard. The
 *    first part shows the basic use of readkey(). The second part
 *    shows how to extract the ASCII value. Next come the scan codes.
 *    The fourth test detects modifier keys like alt or shift. The
 *    fifth test requires some focus to be passed. The final step
 *    shows how to use the global key array to read simultaneous
 *    key presses.
 *    The last method to detect key presses are keyboard callbacks.
 *    This is demonstrated by installing a keyboard callback,
 *    which marks all pressed keys by drawing to a grid.
 *
 *	by Ñuño Martínez <niunio(at)users.sourceforge.net>
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See readme.txt for license and copyright information.
 *)

USES
  allegro, sysutils;



VAR
 KeyNames: ARRAY [0..AL_KEY_MAX] OF STRING =
(
   '(none)',         'KEY_A',          'KEY_B',          'KEY_C',
   'KEY_D',          'KEY_E',          'KEY_F',          'KEY_G',
   'KEY_H',          'KEY_I',          'KEY_J',          'KEY_K',
   'KEY_L',          'KEY_M',          'KEY_N',          'KEY_O',
   'KEY_P',          'KEY_Q',          'KEY_R',          'KEY_S',
   'KEY_T',          'KEY_U',          'KEY_V',          'KEY_W',
   'KEY_X',          'KEY_Y',          'KEY_Z',          'KEY_0',
   'KEY_1',          'KEY_2',          'KEY_3',          'KEY_4',
   'KEY_5',          'KEY_6',          'KEY_7',          'KEY_8',
   'KEY_9',          'KEY_0_PAD',      'KEY_1_PAD',      'KEY_2_PAD',
   'KEY_3_PAD',      'KEY_4_PAD',      'KEY_5_PAD',      'KEY_6_PAD',
   'KEY_7_PAD',      'KEY_8_PAD',      'KEY_9_PAD',      'KEY_F1',
   'KEY_F2',         'KEY_F3',         'KEY_F4',         'KEY_F5',
   'KEY_F6',         'KEY_F7',         'KEY_F8',         'KEY_F9',
   'KEY_F10',        'KEY_F11',        'KEY_F12',        'KEY_ESC',
   'KEY_TILDE',      'KEY_MINUS',      'KEY_EQUALS',     'KEY_BACKSPACE',
   'KEY_TAB',        'KEY_OPENBRACE',  'KEY_CLOSEBRACE', 'KEY_ENTER',
   'KEY_COLON',      'KEY_QUOTE',      'KEY_BACKSLASH',  'KEY_BACKSLASH2',
   'KEY_COMMA',      'KEY_STOP',       'KEY_SLASH',      'KEY_SPACE',
   'KEY_INSERT',     'KEY_DEL',        'KEY_HOME',       'KEY_END',
   'KEY_PGUP',       'KEY_PGDN',       'KEY_LEFT',       'KEY_RIGHT',
   'KEY_UP',         'KEY_DOWN',       'KEY_SLASH_PAD',  'KEY_ASTERISK',
   'KEY_MINUS_PAD',  'KEY_PLUS_PAD',   'KEY_DEL_PAD',    'KEY_ENTER_PAD',
   'KEY_PRTSCR',     'KEY_PAUSE',      'KEY_ABNT_C1',    'KEY_YEN',
   'KEY_KANA',       'KEY_CONVERT',    'KEY_NOCONVERT',  'KEY_AT',
   'KEY_CIRCUMFLEX', 'KEY_COLON2',     'KEY_KANJI',      'KEY_EQUALS_PAD',
   'KEY_BACKQUOTE',  'KEY_SEMICOLON',  'KEY_COMMAND',    'KEY_UNKNOWN1',
   'KEY_UNKNOWN2',   'KEY_UNKNOWN3',   'KEY_UNKNOWN4',   'KEY_UNKNOWN5',
   'KEY_UNKNOWN6',   'KEY_UNKNOWN7',   'KEY_UNKNOWN8',   'KEY_LSHIFT',
   'KEY_RSHIFT',     'KEY_LCONTROL',   'KEY_RCONTROL',   'KEY_ALT',
   'KEY_ALTGR',      'KEY_LWIN',       'KEY_RWIN',       'KEY_MENU',
   'KEY_SCRLOCK',    'KEY_NUMLOCK',    'KEY_CAPSLOCK',   'KEY_MAX'
);



(* Keyboard callback.  Keyboard callback.  We are very evil and draw to the
 * screen from within the callback.  Don't do this in your own programs. *)
PROCEDURE KeypressHandler (Scancode: LONGINT); CDECL;
VAR
  i, x, y, Color: INTEGER;
BEGIN
   i := Scancode AND $7F;
   x := AL_SCREEN_W - 100 * 3 + (i MOD 3) * 100;
   y := AL_SCREEN_H DIV 2 + TRUNC (i / 3 - 21) * 10;
   IF (Scancode AND $80) <> 0 THEN
     Color := al_makecol (255, 255, 0)
   ELSE
     Color := al_makecol(128, 0, 0);
   al_rectfill (al_screen, x, y, x + 95, y + 8, Color);
   al_textout_ex (al_screen, al_font, al_scancode_to_name (i), x + 1, y +1, al_makecol (0, 0, 0), -1);
END;



(* Helper function for making more room on the screen *)
PROCEDURE Scroll;
BEGIN
   al_blit (al_screen, al_screen, 0, 32, 0, 24, AL_SCREEN_W DIV 2, AL_SCREEN_H-32);
   al_rectfill (al_screen, 0, AL_SCREEN_H-16, AL_SCREEN_W DIV 2, AL_SCREEN_H-1, al_makecol (255, 255, 255));
END;



VAR
  k, T: LONGINT;
  Buf: STRING;
BEGIN
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_timer;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 640, 480, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Shows an error message. }
      al_message ('Unable to set any graphic mode'+chr(13)+al_error);
    { Shutdowns Allegro. }
      al_exit;
      EXIT;
    END;
  al_set_palette (al_desktop_palette);

  al_clear_to_color (al_screen, al_makecol (255, 255, 255));

{ Draw the initial keys grid by simulating release of every key. }
  FOR k := 0 TO AL_KEY_MAX DO
    KeypressHandler (k + $80);

  al_keyboard_lowlevel_callback := @KeypressHandler;

  al_acquire_screen;
  {  textprintf_centre_ex(screen, font, SCREEN_W/2, 8, makecol(0, 0, 0), makecol(255, 255, 255),
			"Driver: %s", keyboard_driver->name); }

{ Keyboard input can be accessed with the al_readkey function }
  al_textout_ex (al_screen, al_font, 'Press some keys (ESC to finish)',
		 8, AL_SCREEN_H - 16, al_makecol (0, 0, 0), al_makecol (255, 255, 255));
  Scroll;

  REPEAT
    al_release_screen;
    k := al_readkey;
    al_acquire_screen;
    Scroll;
    al_textout_ex (al_screen, al_font, 'al_readkey returned ' + IntToStr (k)
		   + '($' + IntToHex (k,4) + ')',
		   8, AL_SCREEN_H-16, al_makecol (0, 0, 0), al_makecol (255, 255, 255));
  UNTIL (k AND $FF) = 27;

{ The ASCII code is in the low byte of the return value }
  Scroll; Scroll; Scroll;
  al_textout_ex (al_screen, al_font, 'Press some more keys (ESC to finish)',
		 8, AL_SCREEN_H - 16, al_makecol (0, 0, 0), al_makecol (255, 255, 255));
  Scroll;

  REPEAT
    al_release_screen;
    k := al_readkey;
    al_acquire_screen;
    Scroll;
    al_textout_ex (al_screen, al_font, 'ASCII code is ' + IntToStr (k AND $FF),
		   8, AL_SCREEN_H-16, al_makecol (0, 0, 0), al_makecol (255, 255, 255));
  UNTIL (k AND $FF) = 27;

{ The hardware scan code is in the high byte of the return value }
  Scroll; Scroll; Scroll;
  al_textout_ex (al_screen, al_font, 'Press some more keys (ESC to finish)',
		 8, AL_SCREEN_H - 16, al_makecol (0, 0, 0), al_makecol (255, 255, 255));
  Scroll;

  REPEAT
    al_release_screen;
    k := al_readkey;
    al_acquire_screen;
    Scroll;
    al_textout_ex (al_screen, al_font, 'Scan code is ' + IntToStr (k SHR 8)
		   + '(' + KeyNames[k SHR 8] + ')',
		   8, AL_SCREEN_H-16, al_makecol (0, 0, 0), al_makecol (255, 255, 255));
  UNTIL (k AND $FF) = 27;

{ key qualifiers are stored in the al_key_shifts variable.  Note that this
  version of the code uses al_ureadkey instead of al_readkey:  that is
  necessary if you want to access Unicode characters from outside the normal
  ASCII range, for example to support Russian or Chinese. }
  Scroll; Scroll; Scroll;
  al_textout_ex (al_screen, al_font, 'Press some more keys (ESC to finish)',
		 8, AL_SCREEN_H - 16, al_makecol (0, 0, 0), al_makecol (255, 255, 255));
  Scroll;

  REPEAT
    al_release_screen;
    k := al_ureadkey (T);
    al_acquire_screen;
    Buf := '';
    IF (al_key_shifts AND AL_KB_SHIFT_FLAG)    <> 0 THEN Buf := buf + 'shift ';
    IF (al_key_shifts AND AL_KB_CTRL_FLAG)     <> 0 THEN Buf := buf + 'ctrl ';
    IF (al_key_shifts AND AL_KB_ALT_FLAG)      <> 0 THEN Buf := buf + 'alt ';
    IF (al_key_shifts AND AL_KB_LWIN_FLAG)     <> 0 THEN Buf := buf + 'lwin ';
    IF (al_key_shifts AND AL_KB_RWIN_FLAG)     <> 0 THEN Buf := buf + 'rwin ';
    IF (al_key_shifts AND AL_KB_MENU_FLAG)     <> 0 THEN Buf := buf + 'menu ';
    IF (al_key_shifts AND AL_KB_COMMAND_FLAG)  <> 0 THEN Buf := buf + 'command ';
    IF (k > 0) THEN
      Buf := Buf + chr (k)
    ELSE
      Buf := Buf + ' ';
    Buf := Buf + '[' + IntToHex (k, 4) + ']';
    IF (al_key_shifts AND AL_KB_CAPSLOCK_FLAG) <> 0 THEN Buf := buf + ' caps';
    IF (al_key_shifts AND AL_KB_NUMLOCK_FLAG)  <> 0 THEN Buf := buf + ' num';
    IF (al_key_shifts AND AL_KB_SCROLOCK_FLAG) <> 0 THEN Buf := buf + ' scrl';
    Scroll;
    al_textout_ex (al_screen, al_font, Buf, 8, AL_SCREEN_H-16,
		   al_makecol (0, 0, 0), al_makecol (255, 255, 255));
  UNTIL k = 27;

{ Various scan codes are defined in allegro as AL_KEY_* constants }
  Scroll; Scroll; Scroll;
  al_textout_ex (al_screen, al_font, 'Press F6',
		 8, AL_SCREEN_H - 16, al_makecol (0, 0, 0), al_makecol (255, 255, 255));
  Scroll;

  al_release_screen;
  k := al_readkey;
  al_acquire_screen;

  WHILE ((k SHR 8) <> AL_KEY_F6) AND ((k SHR 8) <> AL_KEY_ESC) DO
  BEGIN
    Scroll();
    al_textout_ex (al_screen, al_font, 'You pressed the wrong key. Press F6 instead.',
		   8, AL_SCREEN_H - 16, al_makecol (0, 0, 0), al_makecol (255, 255, 255));
    al_release_screen;
    k := al_readkey;
    al_acquire_screen
  END;

{ For detecting multiple simultaneous key presses, use the key[] array }
  Scroll; Scroll; Scroll;
  al_textout_ex (al_screen, al_font, 'Press a combination of numbers',
		 8, AL_SCREEN_H - 16, al_makecol (0, 0, 0), al_makecol (255, 255, 255));
  Scroll; Scroll;

  al_release_screen;

  REPEAT
    Buf := '          ';
    IF al_key[AL_KEY_1] <> 0 THEN Buf[1] := '1';
    IF al_key[AL_KEY_2] <> 0 THEN Buf[2] := '2';
    IF al_key[AL_KEY_3] <> 0 THEN Buf[3] := '3';
    IF al_key[AL_KEY_4] <> 0 THEN Buf[4] := '4';
    IF al_key[AL_KEY_5] <> 0 THEN Buf[5] := '5';
    IF al_key[AL_KEY_6] <> 0 THEN Buf[6] := '6';
    IF al_key[AL_KEY_7] <> 0 THEN Buf[7] := '7';
    IF al_key[AL_KEY_8] <> 0 THEN Buf[8] := '8';
    IF al_key[AL_KEY_9] <> 0 THEN Buf[9] := '9';
    IF al_key[AL_KEY_0] <> 0 THEN Buf[10]:= '0';
    al_textout_ex (al_screen, al_font, Buf,
		   8, AL_SCREEN_H - 16, al_makecol (0, 0, 0), al_makecol (255, 255, 255));
    al_rest(1);
  UNTIL al_keypressed AND (al_key[AL_KEY_ESC] <> 0);

  al_clear_keybuf;
  al_keyboard_lowlevel_callback := NIL;

  al_exit;
END.
