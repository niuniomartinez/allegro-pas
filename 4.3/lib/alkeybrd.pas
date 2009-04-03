UNIT alkeybrd;
(*< The Allegro keyboard handler provides both buffered input and a set of
    flags storing the current state of each key.  Note that it is not possible
    to correctly detect every combination of keys, due to the design of the PC
    keyboard.  Up to two or three keys at a time will work fine, but if you
    press more than that the extras are likely to be ignored (exactly which
    combinations are possible seems to vary from one keyboard to another).

    Allegro comes with a prepackaged `keyboard.dat' file which you can put
    along with your binary.  If this file is present, Allegro will be able to
    extract the keyboard mapping information stored there.  However, the end
    user still needs to select which keyboard mapping to use.  This can be
    accomplished through the keyboard variable of the system section in a
    standard `allegro.cfg' configuration file.  Read unit @code(alconfig) for
    more information about this. *)

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$MODE FPC}
{$ELSE}
{ Assumes Codegear Delphi/Turbo. }
 {$A-}
{$ENDIF}
{$H+}



INTERFACE

USES
  albase;



(* Installs the Allegro keyboard interrupt handler.  You must call this before
   using any of the keyboard input routines.  Once you have set up the Allegro
   handler, you can no longer use operating system calls or the run-time
   library functions to access the keyboard.

   Note that on some platforms the keyboard won't work unless you have set a
   graphics mode, even if this function returns a success value before calling
   @link(al_set_gfx_mode).  This can happen in environments with graphic
   windowed modes, since Allegro usually reads the keyboard through the
   graphical window (which appears after the @code(al_set_gfx_mode) call).

   @returns(@true on success, or @false on failure @(but you may decide not to
     check the return value as this function is very unlikely to fail@).)  *)
  FUNCTION al_install_keyboard: BOOLEAN;

(* Removes the keyboard handler, returning control to the operating system.
   You don't normally need to bother calling this, because @link(al_exit) will
   do it for you.  However, you might want to call this during runtime if you
   want to change the keyboard mapping on those platforms were keyboard
   mappings are needed.  You would first modify the configuration variable
   holding the keyboard mapping and then reinstall the keyboard handler. *)
  PROCEDURE al_remove_keyboard;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_keyboard';

(* Wherever possible, Allegro will read the keyboard input asynchronously (ie.
   from inside an interrupt handler), but on some platforms that may not be
   possible, in which case you must call this routine at regular intervals to
   update the keyboard state variables.

   To help you test your keyboard polling code even if you are programming on
   a platform that doesn't require it, after the first time that you call this
   function Allegro will switch into polling mode, so from that point onwards
   you will have to call this routine in order to get any keyboard input at
   all, regardless of whether the current driver actually needs to be polled or
   not.

   The @link(al_keypressed), @link(al_readkey), and @link(al_ureadkey)
   functions call @code(al_poll_keyboard) automatically, so you only need to
   use this function when accessing the @link(al_key) array and
   @link(al_key_shifts) variable.

   @returns(@true on success or @false on failure @(ie. no keyboard driver
     installed@).) *)
  FUNCTION al_poll_keyboard: LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'poll_keyboard';

(* Returns @true if the current keyboard driver is operating in polling mode. *)
  FUNCTION al_keyboard_needs_poll: BOOLEAN;

(* Returns @true if there are keypresses waiting in the input buffer.  You can
   use this to see if the next call to @link(al_readkey) is going to block or
   to simply wait for the user to press a key while you still update the screen
   possibly drawing some animation.  Example:
   @longcode(#
  WHILE NOT al_keypressed DO
    AnimateLogo (al_screen);
   #) *)
  FUNCTION al_keypressed: BOOLEAN;

(* Returns the next character from the keyboard buffer, in ASCII format.  If
   the buffer is empty, it waits until a key is pressed.  You can see if there
   are queued keypresses with @link(al_keypressed).

   The low byte of the return value contains the ASCII code of the key, and the
   high byte the scancode.  The scancode remains the same whatever the state of
   the shift, ctrl and alt keys, while the ASCII code is affected by shift and
   ctrl in the normal way (shift changes case, ctrl+letter gives the position
   of that letter in the alphabet, eg. ctrl+A = 1, ctrl+B = 2, etc).  Pressing
   alt+key returns only the scancode, with a zero ASCII code in the low byte.
   For example:
   @longcode(#
VAR
  val: INTEGER;
  ...
  val := al_readkey;

  IF (val AND $ff) = ORD ('d') THEN
    al_message ('You pressed "d"');

  IF (val RSH 8) = AL_KEY_SPACE THEN
    al_message ('You pressed Space');

  IF (val AND $ff) = 3 THEN
    al_message ('You pressed Control+C');

  IF val = (AL_KEY_X LSH 8) THEN
    al_message ('You pressed Alt+X');
   #)

   This function cannot return character values greater than 255.  If you need
   to read Unicode input, use @link(al_ureadkey) instead. *)
  FUNCTION al_readkey: LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'readkey';

(* Returns the next character from the keyboard buffer, in Unicode format.  If
   the buffer is empty, it waits until a key is pressed.  You can see if there
   are queued keypresses with @link(al_keypressed).  The return value contains
   the Unicode value of the key, and the argument will be set to the scancode.
   Unlike @link(al_readkey), this function is able to return character values
   greater than 255.  Example:
   @longcode(#
VAR
  val, scancode: INTEGER;
  ...
  val := al_ureadkey (scancode);
  IF val = $00F1 THEN
    al_message ('You pressed n with tilde');

  IF val = $00DF THEN
    al_message ('You pressed sharp s');
   #)

   You should be able to find Unicode character maps at
   http://www.unicode.org/. *)
  FUNCTION al_ureadkey (VAR scancode: LONGINT): LONGINT;

(* Stuffs a key into the keyboard buffer, just as if the user had pressed it.
   The parameter is in the same format returned by @link(al_readkey). *)
  PROCEDURE al_simulate_keypress (keycode: LONGINT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'simulate_keypress';

(* Stuffs a key into the keyboard buffer, just as if the user had pressed it.
   The parameter is in the same format returned by @link(al_ureadkey). *)
  PROCEDURE al_simulate_ukeypress (keycode, scancode: LONGINT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'simulate_ukeypress';

(* Empties the keyboard buffer.  Usually you want to use this in your program
   before reading keys to avoid previously buffered keys to be returned by
   calls to @link(al_readkey) or @link(al_ureadkey). *)
  PROCEDURE al_clear_keybuf;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_keybuf';

(* Overrides the state of the keyboard LED indicators.  The parameter is a
   bitmask containing any of the values @code(AL_KB_SCROLOCK_FLAG),
   @code(AL_KB_NUMLOCK_FLAG), and @code(AL_KB_CAPSLOCK_FLAG), or -1 to restore
   the default behavior.

   Note that the led behaviour cannot be guaranteed on some platforms, some
   leds might not react, or none at all.  Therefore you shouldn't rely only on
   them to communicate information to the user, just in case it doesn't get
   through. *)
  PROCEDURE al_set_leds (leds: LONGINT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_leds';

(* Sets the keyboard repeat rate.  Times are given in milliseconds.  Passing
   zero times will disable the key repeat. *)
  PROCEDURE al_set_keyboard_rate (key_delay, key_repeat: LONGINT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_keyboard_rate';

(* Converts the given scancode to an ASCII character for that key (mangling
   Unicode values), returning the unshifted uncapslocked result of pressing the
   key, or zero if the key isn't a character-generating key or the lookup can't
   be done.  The lookup cannot be done for keys like the F1-F12 keys or the
   cursor keys, and some drivers will only return approximate values.
   Generally, if you want to display the name of a key to the user, you should
   use the @link(al_scancode_to_name) function. *)
  FUNCTION al_scancode_to_ascii (scancode: LONGINT): LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scancode_to_ascii';

(* This function returns a string pointer containing the name of they key with
   the given scancode.  This is useful if you e.g. let the user choose a key
   for some action, and want to display something more meaningful than just the
   scancode. *)
  FUNCTION al_scancode_to_name (scancode: LONGINT): STRING;



(* Access to the Allegro's public variables. *)
TYPE
{ @exclude }
  AL_KEY_LISTptr = ^AL_KEY_LIST;
{ @exclude }
  AL_KEY_LIST	 = ARRAY [0..126] OF BYTE;



VAR
(* Array of flags indicating the state of each key, ordered by scancode.
   Wherever possible these values will be updated asynchronously, but if
   @link(al_keyboard_needs_poll) returns @true, you must manually call
   @link(al_poll_keyboard) to update them with the current input state.  The
   scancodes are defined as a series of @code(AL_KEY_* ) constants (and are
   also listed below). For example, you could write:
   @longcode(#
IF  al_key[AL_KEY_SPACE] <> 0 THEN
  WriteLn ('Space is pressed');
   #)

   Note that the array is supposed to represent which keys are physically held
   down and which keys are not, so it is semantically read-only.

   These are the keyboard scancodes:
   @longcode(#
          KEY_A ... KEY_Z,
          KEY_0 ... KEY_9,
          KEY_0_PAD ... KEY_9_PAD,
          KEY_F1 ... KEY_F12,

          KEY_ESC, KEY_TILDE, KEY_MINUS, KEY_EQUALS,
          KEY_BACKSPACE, KEY_TAB, KEY_OPENBRACE, KEY_CLOSEBRACE,
          KEY_ENTER, KEY_COLON, KEY_QUOTE, KEY_BACKSLASH,
          KEY_BACKSLASH2, KEY_COMMA, KEY_STOP, KEY_SLASH,
          KEY_SPACE,

          KEY_INSERT, KEY_DEL, KEY_HOME, KEY_END, KEY_PGUP,
          KEY_PGDN, KEY_LEFT, KEY_RIGHT, KEY_UP, KEY_DOWN,

          KEY_SLASH_PAD, KEY_ASTERISK, KEY_MINUS_PAD,
          KEY_PLUS_PAD, KEY_DEL_PAD, KEY_ENTER_PAD,

          KEY_PRTSCR, KEY_PAUSE,

          KEY_ABNT_C1, KEY_YEN, KEY_KANA, KEY_CONVERT, KEY_NOCONVERT,
          KEY_AT, KEY_CIRCUMFLEX, KEY_COLON2, KEY_KANJI,

          KEY_LSHIFT, KEY_RSHIFT,
          KEY_LCONTROL, KEY_RCONTROL,
          KEY_ALT, KEY_ALTGR,
          KEY_LWIN, KEY_RWIN, KEY_MENU,
          KEY_SCRLOCK, KEY_NUMLOCK, KEY_CAPSLOCK

          KEY_EQUALS_PAD, KEY_BACKQUOTE, KEY_SEMICOLON, KEY_COMMAND
   #)

   Finally, you may notice an @italic(`odd') behaviour of the
   @code(AL_KEY_PAUSE) key.  This key only generates an interrupt when it is
   pressed, not when it is released.  For this reason, Allegro pretends the
   pause key is a @italic(`state') key, which is the only way to make it
   usable. *)
  al_key: AL_KEY_LISTptr; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'key';

(* Bitmask containing the current state of @code(shift/ctrl/alt), the special
   Windows keys, and the accent escape characters.  Wherever possible this
   value will be updated asynchronously, but if @link(al_keyboard_needs_poll)
   returns @true, you must manually call @link(al_poll_keyboard) to update it
   with the current input state.  This can contain any of the flags:
   @longcode(#
          KB_SHIFT_FLAG
          KB_CTRL_FLAG
          KB_ALT_FLAG
          KB_LWIN_FLAG
          KB_RWIN_FLAG
          KB_MENU_FLAG
          KB_COMMAND_FLAG
          KB_SCROLOCK_FLAG
          KB_NUMLOCK_FLAG
          KB_CAPSLOCK_FLAG
          KB_INALTSEQ_FLAG
          KB_ACCENT1_FLAG
          KB_ACCENT2_FLAG
          KB_ACCENT3_FLAG
          KB_ACCENT4_FLAG
   #) *)
  al_key_shifts: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'key_shifts';

(* The DJGPP keyboard handler provides an 'emergency exit' sequence which you
   can use to kill off your program.  If you are running under DOS this is the
   three finger salute, ctrl+alt+del.  Most multitasking OS's will trap this
   combination before it reaches the Allegro handler, in which case you can use
   the alternative ctrl+alt+end.  If you want to disable this behaviour in
   release versions of your program, set this flag to @code(NOT 0). *)
  al_three_finger_flag: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'three_finger_flag';

(* By default, the capslock, numlock, and scroll-lock keys toggle the keyboard
   LED indicators when they are pressed.  If you are using these keys for input
   in your game (eg. capslock to fire) this may not be desirable, so you can
   set this flag to zero and prevent the LED's being updated. *)
  al_key_led_flag: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'key_led_flag';



CONST
{ @exclude Key scan-code identifiers. }
  AL_KEY_A		= 1;
{ @exclude }
  AL_KEY_B		= 2;
{ @exclude }
  AL_KEY_C              = 3;
{ @exclude }
  AL_KEY_D              = 4;
{ @exclude }
  AL_KEY_E              = 5;
{ @exclude }
  AL_KEY_F              = 6;
{ @exclude }
  AL_KEY_G		= 7;
{ @exclude }
  AL_KEY_H		= 8;
{ @exclude }
  AL_KEY_I              = 9;
{ @exclude }
  AL_KEY_J              = 10;
{ @exclude }
  AL_KEY_K              = 11;
{ @exclude }
  AL_KEY_L              = 12;
{ @exclude }
  AL_KEY_M              = 13;
{ @exclude }
  AL_KEY_N              = 14;
{ @exclude }
  AL_KEY_O              = 15;
{ @exclude }
  AL_KEY_P              = 16;
{ @exclude }
  AL_KEY_Q              = 17;
{ @exclude }
  AL_KEY_R              = 18;
{ @exclude }
  AL_KEY_S              = 19;
{ @exclude }
  AL_KEY_T              = 20;
{ @exclude }
  AL_KEY_U              = 21;
{ @exclude }
  AL_KEY_V              = 22;
{ @exclude }
  AL_KEY_W              = 23;
{ @exclude }
  AL_KEY_X              = 24;
{ @exclude }
  AL_KEY_Y              = 25;
{ @exclude }
  AL_KEY_Z              = 26;
{ @exclude }
  AL_KEY_0	        = 27;
{ @exclude }
  AL_KEY_1	        = 28;
{ @exclude }
  AL_KEY_2		= 29;
{ @exclude }
  AL_KEY_3	        = 30;
{ @exclude }
  AL_KEY_4		= 31;
{ @exclude }
  AL_KEY_5		= 32;
{ @exclude }
  AL_KEY_6		= 33;
{ @exclude }
  AL_KEY_7		= 34;
{ @exclude }
  AL_KEY_8		= 35;
{ @exclude }
  AL_KEY_9		= 36;
{ @exclude }
  AL_KEY_0_PAD		= 37;
{ @exclude }
  AL_KEY_1_PAD		= 38;
{ @exclude }
  AL_KEY_2_PAD		= 39;
{ @exclude }
  AL_KEY_3_PAD		= 40;
{ @exclude }
  AL_KEY_4_PAD		= 41;
{ @exclude }
  AL_KEY_5_PAD		= 42;
{ @exclude }
  AL_KEY_6_PAD		= 43;
{ @exclude }
  AL_KEY_7_PAD  	= 44;
{ @exclude }
  AL_KEY_8_PAD  	= 45;
{ @exclude }
  AL_KEY_9_PAD		= 46;
{ @exclude }
  AL_KEY_F1		= 47;
{ @exclude }
  AL_KEY_F2		= 48;
{ @exclude }
  AL_KEY_F3		= 49;
{ @exclude }
  AL_KEY_F4		= 50;
{ @exclude }
  AL_KEY_F5		= 51;
{ @exclude }
  AL_KEY_F6		= 52;
{ @exclude }
  AL_KEY_F7		= 53;
{ @exclude }
  AL_KEY_F8		= 54;
{ @exclude }
  AL_KEY_F9		= 55;
{ @exclude }
  AL_KEY_F10		= 56;
{ @exclude }
  AL_KEY_F11		= 57;
{ @exclude }
  AL_KEY_F12		= 58;
{ @exclude }
  AL_KEY_ESC		= 59;
{ @exclude }
  AL_KEY_TILDE		= 60;
{ @exclude }
  AL_KEY_MINUS		= 61;
{ @exclude }
  AL_KEY_EQUALS		= 62;
{ @exclude }
  AL_KEY_BACKSPACE	= 63;
{ @exclude }
  AL_KEY_TAB		= 64;
{ @exclude }
  AL_KEY_OPENBRACE	= 65;
{ @exclude }
  AL_KEY_CLOSEBRACE	= 66;
{ @exclude }
  AL_KEY_ENTER		= 67;
{ @exclude }
  AL_KEY_COLON		= 68;
{ @exclude }
  AL_KEY_QUOTE		= 69;
{ @exclude }
  AL_KEY_BACKSLASH	= 70;
{ @exclude }
  AL_KEY_BACKSLASH2	= 71;
{ @exclude }
  AL_KEY_COMMA  	= 72;
{ @exclude }
  AL_KEY_STOP		= 73;
{ @exclude }
  AL_KEY_SLASH  	= 74;
{ @exclude }
  AL_KEY_SPACE  	= 75;
{ @exclude }
  AL_KEY_INSERT  	= 76;
{ @exclude }
  AL_KEY_DEL  	     	= 77;
{ @exclude }
  AL_KEY_HOME  	     	= 78;
{ @exclude }
  AL_KEY_END  	     	= 79;
{ @exclude }
  AL_KEY_PGUP  	     	= 80;
{ @exclude }
  AL_KEY_PGDN  	     	= 81;
{ @exclude }
  AL_KEY_LEFT  		= 82;
{ @exclude }
  AL_KEY_RIGHT  	= 83;
{ @exclude }
  AL_KEY_UP  	     	= 84;
{ @exclude }
  AL_KEY_DOWN  	     	= 85;
{ @exclude }
  AL_KEY_SLASH_PAD      = 86;
{ @exclude }
  AL_KEY_ASTERISK       = 87;
{ @exclude }
  AL_KEY_MINUS_PAD      = 88;
{ @exclude }
  AL_KEY_PLUS_PAD       = 89;
{ @exclude }
  AL_KEY_DEL_PAD  	= 90;
{ @exclude }
  AL_KEY_ENTER_PAD      = 91;
{ @exclude }
  AL_KEY_PRTSCR  	= 92;
{ @exclude }
  AL_KEY_PAUSE  	= 93;
{ @exclude }
  AL_KEY_ABNT_C1  	= 94;
{ @exclude }
  AL_KEY_YEN  	  	= 95;
{ @exclude }
  AL_KEY_KANA  	     	= 96;
{ @exclude }
  AL_KEY_CONVERT  	= 97;
{ @exclude }
  AL_KEY_NOCONVERT      = 98;
{ @exclude }
  AL_KEY_AT  	      	= 99;
{ @exclude }
  AL_KEY_CIRCUMFLEX     = 100;
{ @exclude }
  AL_KEY_COLON2  	= 101;
{ @exclude }
  AL_KEY_KANJI  	= 102;
{ @exclude }
  AL_KEY_EQUALS_PAD	= 103;  { MacOS X }
{ @exclude }
  AL_KEY_BACKQUOTE	= 104;  { MacOS X }
{ @exclude }
  AL_KEY_SEMICOLON	= 105;  { MacOS X }
{ @exclude }
  AL_KEY_COMMAND	= 106;  { MacOS X }
{ @exclude }
  AL_KEY_UNKNOWN1	= 107;
{ @exclude }
  AL_KEY_UNKNOWN2	= 108;
{ @exclude }
  AL_KEY_UNKNOWN3	= 109;
{ @exclude }
  AL_KEY_UNKNOWN4	= 110;
{ @exclude }
  AL_KEY_UNKNOWN5	= 111;
{ @exclude }
  AL_KEY_UNKNOWN6	= 112;
{ @exclude }
  AL_KEY_UNKNOWN7	= 113;
{ @exclude }
  AL_KEY_UNKNOWN8	= 114;

{ @exclude }
  AL_KEY_MODIFIERS      = 115;

{ @exclude }
  AL_KEY_LSHIFT  	= 115;
{ @exclude }
  AL_KEY_RSHIFT 	= 116;
{ @exclude }
  AL_KEY_LCONTROL       = 117;
{ @exclude }
  AL_KEY_RCONTROL       = 118;
{ @exclude }
  AL_KEY_ALT  	      	= 119;
{ @exclude }
  AL_KEY_ALTGR  	= 120;
{ @exclude }
  AL_KEY_LWIN  	      	= 121;
{ @exclude }
  AL_KEY_RWIN  	      	= 122;
{ @exclude }
  AL_KEY_MENU  	      	= 123;
{ @exclude }
  AL_KEY_SCRLOCK  	= 124;
{ @exclude }
  AL_KEY_NUMLOCK  	= 125;
{ @exclude }
  AL_KEY_CAPSLOCK       = 126;

{ @exclude }
  AL_KEY_MAX		= 127;

{ @exclude Shift keys flags. }
  AL_KB_SHIFT_FLAG	 = $0001;
{ @exclude }
  AL_KB_CTRL_FLAG	 = $0002;
{ @exclude }
  AL_KB_ALT_FLAG         = $0004;
{ @exclude }
  AL_KB_LWIN_FLAG        = $0008;
{ @exclude }
  AL_KB_RWIN_FLAG        = $0010;
{ @exclude }
  AL_KB_MENU_FLAG        = $0020;
{ @exclude }
  AL_KB_SCROLOCK_FLAG    = $0100;
{ @exclude }
  AL_KB_NUMLOCK_FLAG     = $0200;
{ @exclude }
  AL_KB_CAPSLOCK_FLAG    = $0400;
{ @exclude }
  AL_KB_INALTSEQ_FLAG    = $0800;
{ @exclude }
  AL_KB_ACCENT1_FLAG     = $1000;
{ @exclude }
  AL_KB_ACCENT2_FLAG     = $2000;
{ @exclude }
  AL_KB_ACCENT3_FLAG     = $4000;
{ @exclude }
  AL_KB_ACCENT4_FLAG     = $8000;



IMPLEMENTATION

  FUNCTION install_keyboard: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_keyboard: BOOLEAN;
  BEGIN
    al_install_keyboard := install_keyboard = 0;
  END;



  FUNCTION keyboard_needs_poll: LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_keyboard_needs_poll: BOOLEAN;
  BEGIN
    al_keyboard_needs_poll := keyboard_needs_poll <> 0;
  END;



  FUNCTION keypressed: LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_keypressed: BOOLEAN;
  BEGIN
    al_keypressed := keypressed <> 0;
  END;



  FUNCTION ureadkey (scancode: PLONGINT): LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_ureadkey (VAR scancode: LONGINT): LONGINT;
  BEGIN
    al_ureadkey := ureadkey (@scancode);
  END;



  FUNCTION scancode_to_name (scancode: LONGINT): PCHAR;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scancode_to_name';

  FUNCTION al_scancode_to_name (scancode: LONGINT): STRING;
  BEGIN
    al_scancode_to_name := scancode_to_name (scancode);
  END;

END.
