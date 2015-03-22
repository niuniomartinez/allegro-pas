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
 *	This program demonstrates how to access the keyboard. The
 *	first part shows the basic use of al_readkey(). The second part
 *	shows how to extract the ASCII value. Next come the scan codes.
 *	The fourth test detects modifier keys like alt or shift. The
 *	fifth test requires some focus to be passed. The final step
 *	shows how to use the global key array to read simultaneous
 *	key presses.
 *
 *	The last method to detect key presses are keyboard callbacks.
 *	This is demonstrated by installing a keyboard callback,
 *	which marks all pressed keys by drawing to a grid.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See README file for license and copyright information.
 *)

USES
  albase,
  allegro, sysutils;



VAR
 KeyNames: ARRAY [0..AL_KEY_MAX] OF STRING =
(
   '(none)',            'AL_KEY_A',          'AL_KEY_B',
   'AL_KEY_C',          'AL_KEY_D',          'AL_KEY_E',
   'AL_KEY_F',          'AL_KEY_G',          'AL_KEY_H',
   'AL_KEY_I',          'AL_KEY_J',          'AL_KEY_K',
   'AL_KEY_L',          'AL_KEY_M',          'AL_KEY_N',
   'AL_KEY_O',          'AL_KEY_P',          'AL_KEY_Q',
   'AL_KEY_R',          'AL_KEY_S',          'AL_KEY_T',
   'AL_KEY_U',          'AL_KEY_V',          'AL_KEY_W',
   'AL_KEY_X',          'AL_KEY_Y',          'AL_KEY_Z',
   'AL_KEY_0',          'AL_KEY_1',          'AL_KEY_2',
   'AL_KEY_3',          'AL_KEY_4',          'AL_KEY_5',
   'AL_KEY_6',          'AL_KEY_7',          'AL_KEY_8',
   'AL_KEY_9',          'AL_KEY_0_PAD',      'AL_KEY_1_PAD',
   'AL_KEY_2_PAD',      'AL_KEY_3_PAD',      'AL_KEY_4_PAD',
   'AL_KEY_5_PAD',      'AL_KEY_6_PAD',      'AL_KEY_7_PAD',
   'AL_KEY_8_PAD',      'AL_KEY_9_PAD',      'AL_KEY_F1',
   'AL_KEY_F2',         'AL_KEY_F3',         'AL_KEY_F4',
   'AL_KEY_F5',         'AL_KEY_F6',         'AL_KEY_F7',
   'AL_KEY_F8',         'AL_KEY_F9',         'AL_KEY_F10',
   'AL_KEY_F11',        'AL_KEY_F12',        'AL_KEY_ESC',
   'AL_KEY_TILDE',      'AL_KEY_MINUS',      'AL_KEY_EQUALS',
   'AL_KEY_BACKSPACE',  'AL_KEY_TAB',        'AL_KEY_OPENBRACE',
   'AL_KEY_CLOSEBRACE', 'AL_KEY_ENTER',      'AL_KEY_COLON',
   'AL_KEY_QUOTE',      'AL_KEY_BACKSLASH',  'AL_KEY_BACKSLASH2',
   'AL_KEY_COMMA',      'AL_KEY_STOP',       'AL_KEY_SLASH',
   'AL_KEY_SPACE',      'AL_KEY_INSERT',     'AL_KEY_DEL',
   'AL_KEY_HOME',       'AL_KEY_END',        'AL_KEY_PGUP',
   'AL_KEY_PGDN',       'AL_KEY_LEFT',       'AL_KEY_RIGHT',
   'AL_KEY_UP',         'AL_KEY_DOWN',       'AL_KEY_SLASH_PAD',
   'AL_KEY_ASTERISK',   'AL_KEY_MINUS_PAD',  'AL_KEY_PLUS_PAD',
   'AL_KEY_DEL_PAD',    'AL_KEY_ENTER_PAD',  'AL_KEY_PRTSCR',
   'AL_KEY_PAUSE',      'AL_KEY_ABNT_C1',    'AL_KEY_YEN',
   'AL_KEY_KANA',       'AL_KEY_CONVERT',    'AL_KEY_NOCONVERT',
   'AL_KEY_AT',         'AL_KEY_CIRCUMFLEX', 'AL_KEY_COLON2',
   'AL_KEY_KANJI',      'AL_KEY_EQUALS_PAD', 'AL_KEY_BACKQUOTE',
   'AL_KEY_SEMICOLON',  'AL_KEY_COMMAND',    'AL_KEY_UNKNOWN1',
   'AL_KEY_UNKNOWN2',   'AL_KEY_UNKNOWN3',   'AL_KEY_UNKNOWN4',
   'AL_KEY_UNKNOWN5',   'AL_KEY_UNKNOWN6',   'AL_KEY_UNKNOWN7',
   'AL_KEY_UNKNOWN8',   'AL_KEY_LSHIFT',     'AL_KEY_RSHIFT',
   'AL_KEY_LCONTROL',   'AL_KEY_RCONTROL',   'AL_KEY_ALT',
   'AL_KEY_ALTGR',      'AL_KEY_LWIN',       'AL_KEY_RWIN',
   'AL_KEY_MENU',       'AL_KEY_SCRLOCK',    'AL_KEY_NUMLOCK',
   'AL_KEY_CAPSLOCK',   'AL_KEY_MAX'
);



(* Keyboard callback.  We are very evil and draw to the screen from within the
   callback.  Don't do this in your own programs. *)
  PROCEDURE KeypressHandler (Scancode: AL_INT); CDECL;
  VAR
    i, x, y, Color: INTEGER;
  BEGIN
     i := Scancode AND $7F;
     x := AL_SCREEN_W - 100 * 3 + (i MOD 3) * 100;
     y := AL_SCREEN_H DIV 2 + (i DIV 3 - 21) * 10;
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
      EXIT;
    END;
  al_set_palette (al_desktop_palette);

  al_clear_to_color (al_screen, al_makecol (255, 255, 255));

{ Draw the initial keys grid by simulating release of every key. }
  FOR k := 1 TO AL_KEY_MAX DO
    KeypressHandler (k + $80 - 1);

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
    IF al_key[AL_KEY_1] THEN Buf[1] := '1';
    IF al_key[AL_KEY_2] THEN Buf[2] := '2';
    IF al_key[AL_KEY_3] THEN Buf[3] := '3';
    IF al_key[AL_KEY_4] THEN Buf[4] := '4';
    IF al_key[AL_KEY_5] THEN Buf[5] := '5';
    IF al_key[AL_KEY_6] THEN Buf[6] := '6';
    IF al_key[AL_KEY_7] THEN Buf[7] := '7';
    IF al_key[AL_KEY_8] THEN Buf[8] := '8';
    IF al_key[AL_KEY_9] THEN Buf[9] := '9';
    IF al_key[AL_KEY_0] THEN Buf[10]:= '0';
    al_textout_ex (al_screen, al_font, Buf,
		   8, AL_SCREEN_H - 16, al_makecol (0, 0, 0), al_makecol (255, 255, 255));
    al_rest(1);
  UNTIL al_keypressed AND al_key[AL_KEY_ESC];

  al_clear_keybuf;
  al_keyboard_lowlevel_callback := NIL;
END.
